! { dg-do run }
! { dg-require-effective-target target_device }

module target21_mod
  !$omp declare target (N, p, v1, v2)
  integer, parameter :: N = 10000
  real :: p(N), v1(N), v2(N)
end module

subroutine init (v1, v2, N)
  integer :: i, N
  real :: v1(N), v2(N)
  do i = 1, N
    v1(i) = i + 2.0
    v2(i) = i - 3.0
  end do
end subroutine

subroutine check (p, N)
  integer :: i, N
  real :: p(N)
  do i = 1, N
    if (p(i) /= (i + 2.0) * (i - 3.0)) call abort
  end do
end subroutine

subroutine vec_mult ()
  use target21_mod
  integer :: i
  call init (v1, v2, N);
  !$omp target update to(v1, v2)
  !$omp target
    !$omp parallel do
    do i = 1,N
      p(i) = v1(i) * v2(i)
    end do
  !$omp end target
  !$omp target update from (p)
  call check (p, N)
end subroutine

program target21
  call vec_mult ()
end program
