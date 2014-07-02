// { dg-do run }
// { dg-require-effective-target target_device }

#include <omp.h>

#define EPS 0.00001
#define N 1000

extern "C" void abort (void);

#pragma omp declare target
float Q[N][N];
float Pfun (const int i, const int k)
{
  return Q[i][k] * Q[k][i];
}
#pragma omp end declare target

void init ()
{
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      Q[i][j] = 0.001 * i * j;
}

float accum_ref (int k)
{
  float tmp = 0.0;

  for (int i = 0; i < N; i++)
    tmp += Pfun (i, k);

  return tmp;
}

float accum (int k)
{
  float tmp = 0.0;

  #pragma omp target
    #pragma omp parallel for reduction(+:tmp)
      for (int i = 0; i < N; i++)
	tmp += Pfun (i, k);

  return tmp;
}

void check (float a, float b)
{
  float err = (b == 0.0) ? a : (a - b) / b;
  if (((err > 0) ? err : -err) > EPS)
    abort ();
}

int main ()
{
  init ();

  #pragma omp target update to(Q)

  for (int i = 0; i < N; i++)
    check (accum (i), accum_ref (i));

  return 0;
}
