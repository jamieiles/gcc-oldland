! { dg-do run }
! { dg-require-effective-target target_device }

module target9_mod
contains
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

  subroutine vec_mult_1 (N)
    integer :: i, N
    real :: p(N), v1(N), v2(N)
    call init (v1, v2, N)
    !$omp target
      !$omp parallel do
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target
    call check (p, N)
  end subroutine

  subroutine vec_mult_2 (N)
    integer :: i, N
    real :: p(N), v1(N), v2(N)
    call init (v1, v2, N)
    !$omp target map(v1,v2,p)
      !$omp parallel do
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target
  call check (p, N)
  end subroutine

  subroutine vec_mult_3 (N)
    integer :: i, N
    real :: p(N), v1(N), v2(N)
    call init (v1, v2, N)
    !$omp target map(to: v1,v2) map(from: p)
      !$omp parallel do
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target
    call check (p, N)
  end subroutine
end module

program target9
  use target9_mod, only : vec_mult_1, vec_mult_2, vec_mult_3
  integer :: n
  n = 10000
  call vec_mult_1 (n)
  call vec_mult_2 (n)
  call vec_mult_3 (n)
end program
