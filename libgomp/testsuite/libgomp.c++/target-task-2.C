// { dg-do run }
// { dg-require-effective-target target_device }

#include <omp.h>

#define EPS 0.00001
#define N 10000

extern "C" void abort (void);

#pragma omp declare target
void init (float *a, float *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      a[i] = 0.1 * i;
      b[i] = 0.01 * i * i;
    }
}
#pragma omp end declare target

void vec_mult_ref (float *p, float *v1, float *v2, int n)
{
  v1 = new float[n];
  v2 = new float[n];

  init (v1, v2, n);

  for (int i = 0; i < n; i++)
    p[i] = v1[i] * v2[i];

  delete [] v1;
  delete [] v2;
}

void vec_mult (float *p1, float *p2, float *v1, float *v2, int n)
{
  #pragma omp task shared(v1, v2) depend(out: v1, v2)
    #pragma omp target map(v1, v2)
      {
	if (omp_is_initial_device ())
	  abort ();

	v1 = new float[n];
	v2 = new float[n];

	init (v1, v2, n);
      }

  #pragma omp task shared(v1, v2) depend(in: v1, v2)
    #pragma omp target map(to: v1, v2) map(from: p2[0:n])
      {
	if (omp_is_initial_device ())
	  abort ();

	#pragma omp parallel for
	  for (int i = 0; i < n; i++)
	    p2[i] = v1[i] * v2[i];

	delete [] v1;
	delete [] v2;
      }
}

void check (float *a, float *b, int n)
{
  for (int i = 0 ; i < n ; i++)
    {
      float err = (a[i] == 0.0) ? b[i] : (b[i] - a[i]) / a[i];
      if (((err > 0) ? err : -err) > EPS)
	abort ();
    }
}

int main ()
{
  float *p1 = new float[N];
  float *p2 = new float[N];
  float *v1, *v2;

  vec_mult_ref (p1, v1, v2, N);
  vec_mult (p1, p2, v1, v2, N);

  check (p1, p2, N);

  delete [] p1;
  delete [] p2;

  return 0;
}
