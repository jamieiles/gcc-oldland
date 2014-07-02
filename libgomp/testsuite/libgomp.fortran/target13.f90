! { dg-do run }
! { dg-require-effective-target target_device }

module target13_mod
contains
  subroutine init (v1, v2, N)
    integer :: i, N
    real :: v1(N), v2(N)
    do i = 1, N
      v1(i) = i + 2.0
      v2(i) = i - 3.0
    end do
  end subroutine

  subroutine init_again (v1, v2, N)
    integer :: i, N
    real :: v1(N), v2(N)
    do i = 1, N
      v1(i) = i - 3.0
      v2(i) = i + 2.0
    end do
  end subroutine

  subroutine check (p, N)
    integer :: i, N
    real :: p(N)
    do i = 1, N
      if (p(i) /= 2 * (i + 2.0) * (i - 3.0)) call abort
    end do
  end subroutine

  subroutine vec_mult (N)
    real :: p(N), v1(N), v2(N)
    integer :: i, N
    call init (v1, v2, N)
    !$omp target data map(from: p)
      !$omp target map(to: v1, v2 )
        !$omp parallel do
        do i = 1, N
          p(i) = v1(i) * v2(i)
        end do
      !$omp end target
      call init_again (v1, v2, N)
      !$omp target map(to: v1, v2 )
        !$omp parallel do
        do i = 1, N
          p(i) = p(i) + v1(i) * v2(i)
        end do
      !$omp end target
    !$omp end target data
    call check (p, N)
  end subroutine
end module

program target13
  use target13_mod, only : vec_mult
  integer :: n
  n = 10000
  call vec_mult (n)
end program
