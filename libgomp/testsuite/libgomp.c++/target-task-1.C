// { dg-do run }
// { dg-require-effective-target target_device }

#define EPS 0.00001
#define N 100000
#define CHUNKSZ 1000

extern "C" void abort (void);

float Y[N];
float Z[N];

#pragma omp declare target
float F (float a)
{
  return -a;
}
#pragma omp end declare target

void pipedF_ref ()
{
  for (int i = 0; i < N; i++)
    Y[i] = F (Y[i]);
}

void pipedF ()
{
  for (int C = 0; C < N; C += CHUNKSZ)
    {
      #pragma omp task
	#pragma omp target map(Z[C:CHUNKSZ])
	  #pragma omp parallel for
	    for (int i = C; i < C + CHUNKSZ; i++)
	      Z[i] = F (Z[i]);
    }
  #pragma omp taskwait
}

void init ()
{
  for (int i = 0; i < N; i++)
    Y[i] = Z[i] = 0.1 * i;
}

void check ()
{
  for (int i = 0; i < N; i++)
    {
      float err = (Z[i] == 0.0) ? Y[i] : (Y[i] - Z[i]) / Z[i];
      if (((err > 0) ? err : -err) > EPS)
	abort ();
    }
}

int main ()
{
  init ();

  pipedF_ref ();
  pipedF ();

  check ();

  return 0;
}
