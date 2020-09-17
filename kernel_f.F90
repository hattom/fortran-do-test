#ifndef VEC_LENGTH
#define VEC_LENGTH 8
#endif

subroutine f_inner_sub(a, b)
  real, intent(inout) :: a
  integer, intent(in) :: b
  a = 0.5 * (a + b/a)
end subroutine f_inner_sub

subroutine f_inner_sub_vec(avec, b)
  real, intent(inout), dimension(VEC_LENGTH) :: avec
  integer, intent(in) :: b
  integer :: vec_elem
  real :: a
  do vec_elem=1,VEC_LENGTH
    a = avec(vec_elem)
    a = 0.5 * (a + (b+vec_elem-1)/a)
    avec(vec_elem) = a
  end do
end subroutine f_inner_sub_vec

subroutine f_outer_sub(a, b, c)
  real, intent(inout) :: a
  integer, intent(in) :: b, c
  integer :: i
  do i=1,c
    a = 0.5 * (a + b/a)
  end do
end subroutine f_outer_sub

subroutine f_outer_sub_vec(avec, b, c)
  real, intent(inout), dimension(VEC_LENGTH) :: avec
  real :: a
  integer, intent(in) :: b, c
  integer :: i, vec_elem
  do vec_elem = 1, VEC_LENGTH
    a = avec(vec_elem)
    do i=1,c
      a = 0.5 * (a + (b+vec_elem-1)/a)
    end do
    avec(vec_elem) = a
  end do
end subroutine f_outer_sub_vec

subroutine f_outer_sub_vec2(avec, b, c)
  real, intent(inout), dimension(VEC_LENGTH) :: avec
  integer, intent(in) :: b, c
  integer :: i
  integer, dimension(VEC_LENGTH) :: bvec
  do i=1,VEC_LENGTH
    bvec(i) = b + i - 1
  end do
  do i=1,c
    avec(:) = 0.5 * (avec(:) + bvec(:)/avec(:))
  end do
end subroutine f_outer_sub_vec2

subroutine f_outer_sub_vec2b(avec, b, c)
  real, intent(inout), dimension(VEC_LENGTH) :: avec
  integer, intent(in) :: b, c
  integer :: i
  real, dimension(VEC_LENGTH) :: bvec
  do i=1,VEC_LENGTH
    bvec(i) = real(b + i - 1)
  end do
  do i=1,c
    avec(:) = 0.5 * (avec(:) + bvec(:)/avec(:))
  end do
end subroutine f_outer_sub_vec2b
