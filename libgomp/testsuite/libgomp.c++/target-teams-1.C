// { dg-do run }
// { dg-require-effective-target target_device }

#define EPS 0.00001
#define N 1000

extern "C" void abort (void);

void init (float B[], float C[], int n)
{
  for (int i = 0; i < n; i++)
    {
      B[i] = 0.1 * i;
      C[i] = 0.01 * i * i;
    }
}

float dotprod_ref (float B[], float C[], int n)
{
  float sum = 0.0;

  for (int i = 0; i < n; i++)
    sum += B[i] * C[i];

  return sum;
}

float dotprod_1 (float B[], float C[], int n, int block_size,
		 int num_teams, int block_threads)
{
  float sum = 0;

  #pragma omp target map(to: B[0:n], C[0:n])
    #pragma omp teams num_teams(num_teams) thread_limit(block_threads) reduction(+:sum)
      #pragma omp distribute
	for (int i0 = 0; i0 < n; i0 += block_size)
	  #pragma omp parallel for reduction(+:sum)
	    for (int i = i0; i < ((i0 + block_size > n) ? n : i0 + block_size); i++)
	      sum += B[i] * C[i];

  return sum;
}

float dotprod_2 (float B[], float C[], int n)
{
  float sum = 0;

  #pragma omp target teams map(to: B[0:n], C[0:n])
    #pragma omp distribute parallel for reduction(+:sum)
      for (int i = 0; i < n; i++)
	sum += B[i] * C[i];

  return sum;
}

void check (float a, float b)
{
  float err = (b == 0.0) ? a : (a - b) / b;
  if (((err > 0) ? err : -err) > EPS)
    abort ();
}

int main ()
{
  float *v1 = new float[N];
  float *v2 = new float[N];

  float p, p1, p2;

  init (v1, v2, N);

  p = dotprod_ref (v1, v2, N);
  p1 = dotprod_1 (v1, v2, N, 32, 2, 8);
  p2 = dotprod_2 (v1, v2, N);

  check (p, p1);
  check (p, p2);

  return 0;
}
