! { dg-do run }
! { dg-require-effective-target target_device }

module target16_mod
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

  subroutine foo (p, v1, v2, N)
    real, dimension(:) :: p, v1, v2
    integer :: i, N
    call init (v1, v2, N)
    !$omp target data map(to: v1, v2, N) map(from: p)
      call vec_mult (p, v1, v2, N)
    !$omp end target data
    call check (p, N)
  end subroutine

  subroutine vec_mult (p, v1, v2, N)
    real, dimension(:) :: p, v1, v2
    integer :: i, N
    !$omp target map(to: v1, v2, N) map(from: p)
      !$omp parallel do
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target
  end subroutine
end module

program target16
  use target16_mod, only : foo
  integer, parameter :: N = 1024
  real, allocatable, dimension(:) :: p, v1, v2
  allocate(p(N), v1(N), v2(N))
  call foo (p, v1, v2, N)
  deallocate (p, v1, v2)
end program
