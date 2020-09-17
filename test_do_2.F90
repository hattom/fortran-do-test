#ifndef VEC_LENGTH
#define VEC_LENGTH 8
#endif

#ifndef N_POWER
#define N_POWER 20
#endif

#ifndef M_POWER
#define M_POWER 13
#endif

module test_do
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
  implicit none
  public
    interface
      subroutine fc_inner_sub(a, b) bind(c, name='c_inner_sub')
        use, intrinsic :: iso_c_binding
        real(c_float), intent(inout) :: a
        integer(c_int), intent(in) :: b
      end subroutine fc_inner_sub
    end interface
contains
subroutine run
  use omp_lib, only: omp_get_wtime
  implicit none

  integer, parameter :: n = 2**N_POWER, m = 2**M_POWER
  integer, parameter :: n_1 = 256
  integer, parameter :: n_2 = n/n_1
  real,  allocatable :: q(:)

  integer :: i, ii, iii, j, iloop
  real    :: x
  real(kind=REAL64) :: t0
  character(len=100) :: str
  logical, dimension(0:100) :: do_test=.true.

#ifdef WHICH_TESTS32
  do i=1,32
    do_test(i-1) = btest(WHICH_TESTS32, i-1)
  enddo
#endif
#ifdef WHICH_TESTS64
  do i=1,32
    do_test(i-1+32) = btest(WHICH_TESTS64, i-1)
  enddo
#endif

  iloop = 0

  allocate(q(n))

  ! this doesn't stop the warmup from being slow
  q(:) = 0.0
  ! this also doesn't stop the warmup from being slow
  do i=1,n
  q(i) = 0.0
  enddo
#if FALSE
  ! this does stop the warmup from being slow
  !$omp parallel do
  do i=1,n
    q(i) = 0.0
  enddo
  !$omp end parallel do
#endif

#ifdef SET_TO_NEGATIVE_ONE
  ! setting to zero does nothing
  q(:) = -1.
#else
  iloop = -1
  str="-1 warmup (slow, first memory touch)"
  t0 = omp_get_wtime()
  do i = 1, n
    q(i) = i
    do j = 1, m
      q(i) = 0.5 * (q(i) + i / q(i))
    end do
  end do
  print *, iloop, omp_get_wtime() - t0, trim(str)
  iloop = iloop + 1
#endif

  if(do_test(iloop)) then
    str="0 serial"
    t0 = omp_get_wtime()
    do i = 1, n
      q(i) = i
      do j = 1, m
        q(i) = 0.5 * (q(i) + i / q(i))
      end do
    end do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="1 omp parallel simd"
    t0 = omp_get_wtime()
    !$omp parallel do simd
    do i = 1, n
      q(i) = i
      do j = 1, m
        q(i) = 0.5 * (q(i) + i / q(i))
      end do
    end do
    !$omp end parallel do simd
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 0
  if(do_test(iloop)) then
    str="1b omp parallel"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i = 1, n
      q(i) = i
      do j = 1, m
        q(i) = 0.5 * (q(i) + i / q(i))
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 0
  if(do_test(iloop)) then
    str="1c omp simd"
    t0 = omp_get_wtime()
    !$omp simd
    do i = 1, n
      q(i) = i
      do j = 1, m
        q(i) = 0.5 * (q(i) + i / q(i))
      end do
    end do
    !$omp end simd
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="2 do conc"
    t0 = omp_get_wtime()
    do concurrent (i = 1:n)
      q(i) = i
      do j = 1, m
        q(i) = 0.5 * (q(i) + i / q(i))
      end do
    end do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="3 do conc block"
    t0 = omp_get_wtime()
    do concurrent (i = 1:n_2, ii=1:n_1)
      q(i*n_1 + ii) = i*n_1 + ii
      do j = 1, m
        q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
      end do
    end do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="4 serial block"
    t0 = omp_get_wtime()
    do i=1,n_2
      do ii=1,n_1
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
    end do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="5 omp parallel block"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n_2
      do ii=1,n_1
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="6 omp parallel simd block"
    t0 = omp_get_wtime()
    !$omp parallel do simd
    do i=1,n_2
      do ii=1,n_1
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
    end do
    !$omp end parallel do simd
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="7 omp parallel, block omp simd nested"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n_2
      !$omp simd
      do ii=1,n_1
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
      !$omp end simd
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="8 omp parallel collapse block"
    t0 = omp_get_wtime()
    !$omp parallel do collapse(2)
    do i=1,n_2
      do ii=1,n_1
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 0
  if(do_test(iloop)) then
    str="8b omp parallel collapse block"
    t0 = omp_get_wtime()
    !$omp parallel do collapse(2)
      do ii=1,n_1
    do i=1,n_2
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 0
  if(do_test(iloop)) then
    str="8c omp parallel collapse block"
    t0 = omp_get_wtime()
    !$omp parallel do collapse(2)
    do iii=1,n_2
      do ii=1,n_1
        i = iii*n_1 + ii
        q(i) = i
        do j = 1, m
          q(i) = 0.5 * (q(i) + i / q(i))
        end do
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="9 omp parallel simd collapse block"
    t0 = omp_get_wtime()
    !$omp parallel do simd collapse(2)
    do i=1,n_2
      do ii=1,n_1
        q(i*n_1 + ii) = i*n_1 + ii
        do j = 1, m
          q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
      end do
    end do
    !$omp end parallel do simd
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="10 omp parallel inner sub"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      do j = 1, m
        call inner_kernel(q(i), i)
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="11 omp parallel inner func"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      do j = 1, m
        q(i) = inner_kernel_func(q(i), i)
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="12 omp parallel inner func pe"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      do j = 1, m
        q(i) = inner_kernel_func_pe(q(i), i)
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="13 omp parallel inner func pe v"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      do j = 1, m
        q(i) = inner_kernel_func_pe_v(q(i), i)
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="14 omp parallel outer sub"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      call outer_kernel(q(i), i, m)
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="15 omp parallel outer func"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = outer_kernel_func(i, m)
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="16 omp parallel outer func pe"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = outer_kernel_func_pe(i, m)
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="17 omp parallel outer func pe v"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = outer_kernel_func_pe_v(i, m)
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="18 omp parallel inner sub c"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      do j = 1, m
        call fc_inner_sub(q(i), i)
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="19 omp parallel inner sub f"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      q(i) = i
      do j = 1, m
        call f_inner_sub(q(i), i)
      end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="20 omp parallel outer sub f"
    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n
      call f_outer_sub(q(i), i, m)
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="21 omp parallel outer sub f vec"
    t0 = omp_get_wtime()
    !$omp parallel
    !$omp do
    do i=1,VEC_LENGTH*(n/VEC_LENGTH),VEC_LENGTH
      call f_outer_sub_vec(q(i:i+(VEC_LENGTH-1)), i, m)
    end do
    !$omp single
    do i=VEC_LENGTH*(n/VEC_LENGTH)+1,n
      call f_outer_sub(q(i), i, m)
    end do
    !$omp end single
    !$omp end parallel
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif

  iloop = iloop + 1
  if(do_test(iloop)) then
    str="22 omp parallel inner sub f vec"
    t0 = omp_get_wtime()
    !$omp parallel
    !$omp do
    do i=1,VEC_LENGTH*(n/VEC_LENGTH),VEC_LENGTH
      do ii=0,VEC_LENGTH-1
        q(i+ii) = i+ii
      end do
      do j = 1, m
        call f_inner_sub_vec(q(i:i+(VEC_LENGTH-1)), i)
      end do
    end do
    !$omp single
    do i=VEC_LENGTH*(n/VEC_LENGTH)+1,n
      do j = 1, m
        call f_inner_sub(q(i), i)
      end do
    end do
    !$omp end single
    !$omp end parallel
    print *, iloop, omp_get_wtime() - t0, trim(str)
  endif
end subroutine run

subroutine inner_kernel(q_elem, elem_id)
  implicit none
  real, intent(inout) :: q_elem
  integer, intent(in) :: elem_id
  q_elem = 0.5 * (q_elem + elem_id/ q_elem)
end subroutine inner_kernel

real function inner_kernel_func(q_elem, elem_id)
  implicit none
  real, intent(in) :: q_elem
  integer, intent(in) :: elem_id
  inner_kernel_func = 0.5 * (q_elem + elem_id/ q_elem)
end function inner_kernel_func

pure elemental real function inner_kernel_func_pe(q_elem, elem_id)
  implicit none
  real, intent(in) :: q_elem
  integer, intent(in) :: elem_id
  inner_kernel_func_pe = 0.5 * (q_elem + elem_id/ q_elem)
end function inner_kernel_func_pe

pure elemental real function inner_kernel_func_pe_v(q_elem, elem_id)
  implicit none
  real, intent(in), value :: q_elem
  integer, intent(in) :: elem_id
  inner_kernel_func_pe_v = 0.5 * (q_elem + elem_id/ q_elem)
end function inner_kernel_func_pe_v

! !$omp declare simd
pure elemental real function inner_kernel_func_pe_o(q_elem, elem_id)
  implicit none
  real, intent(in) :: q_elem
  integer, intent(in) :: elem_id
  inner_kernel_func_pe_o = 0.5 * (q_elem + elem_id/ q_elem)
end function inner_kernel_func_pe_o

subroutine outer_kernel(q_elem, elem_id, nsteps)
  implicit none
  real, intent(inout) :: q_elem
  integer, intent(in) :: elem_id, nsteps
  integer             :: istep
  do istep = 1,nsteps
    q_elem = 0.5 * (q_elem + elem_id/ q_elem)
  end do
end subroutine outer_kernel

real function outer_kernel_func(elem_id, nsteps)
  implicit none
  integer, intent(in) :: elem_id, nsteps
  integer             :: istep
  outer_kernel_func = elem_id
  do istep = 1,nsteps
    outer_kernel_func = 0.5 * (outer_kernel_func + elem_id/ outer_kernel_func)
  end do
end function outer_kernel_func

pure elemental real function outer_kernel_func_pe(elem_id, nsteps)
  implicit none
  integer, intent(in) :: elem_id, nsteps
  integer             :: istep
  outer_kernel_func_pe = elem_id
  do istep = 1,nsteps
    outer_kernel_func_pe = 0.5 * (outer_kernel_func_pe + elem_id/ outer_kernel_func_pe)
  end do
end function outer_kernel_func_pe

pure elemental real function outer_kernel_func_pe_v(elem_id, nsteps)
  implicit none
  integer, intent(in) :: elem_id, nsteps
  integer             :: istep
  outer_kernel_func_pe_v = elem_id
  do istep = 1,nsteps
    outer_kernel_func_pe_v = 0.5 * (outer_kernel_func_pe_v + elem_id/ outer_kernel_func_pe_v)
  end do
end function outer_kernel_func_pe_v

!!$omp declare simd
pure elemental real function outer_kernel_func_pe_o(elem_id, nsteps)
  implicit none
  integer, intent(in) :: elem_id, nsteps
  integer             :: istep
  outer_kernel_func_pe_o = elem_id
  do istep = 1,nsteps
    outer_kernel_func_pe_o = 0.5 * (outer_kernel_func_pe_o + elem_id/ outer_kernel_func_pe_o)
  end do
end function outer_kernel_func_pe_o

end module test_do

program test
  use test_do
  call run
end program test
