! { dg-do run }
! { dg-require-effective-target target_device }

module target10_mod
contains
  subroutine init (v1, v2, N)
    integer :: i, N
    real, pointer, dimension(:) :: v1, v2
    do i = 1, N
      v1(i) = i + 2.0
      v2(i) = i - 3.0
    end do
  end subroutine

  subroutine check (p, N)
    integer :: i, N
    real, pointer, dimension(:) :: p
    do i = 1, N
      if (p(i) /= (i + 2.0) * (i - 3.0)) call abort
    end do
  end subroutine

  subroutine vec_mult_1 (p, v1, v2, N)
    integer :: i, N
    real, pointer, dimension(:) :: p, v1, v2
    !$omp target map(to: v1(1:N), v2(:N)) map(from: p(1:N))
      !$omp parallel do
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target
  end subroutine

  subroutine vec_mult_2 (p, v1, v2, N)
    real, dimension(*) :: p, v1, v2
    integer :: i, N
    !$omp target map(to: v1(1:N), v2(:N)) map(from: p(1:N))
      !$omp parallel do
      do i = 1, N
        p(i) = v1(i) * v2(i)
      end do
    !$omp end target
  end subroutine
end module

program target10
  use target10_mod, only : init, check, vec_mult_1, vec_mult_2
  real, pointer, dimension(:) :: p1, p2, v1, v2
  integer :: n
  n = 10000
  allocate (p1(n), p2(n), v1(n), v2(n))
  call init (v1, v2, n)
  call vec_mult_1 (p1, v1, v2, n)
  call vec_mult_2 (p2, v1, v2, n)
  call check (p1, N)
  call check (p2, N)
  deallocate (p1, p2, v1, v2)
end program
