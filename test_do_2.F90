program test_do

    use omp_lib, only: omp_get_wtime

    integer, parameter :: n = 2**20, m = 2**13
    integer, parameter :: n_1 = 256
    integer, parameter :: n_2 = n/n_1
    real,  allocatable :: q(:)

    integer :: i, iloop
    real    :: x, t0

    iloop = 1

    allocate(q(n))

    t0 = omp_get_wtime()
    !$omp parallel do simd
    do i = 1, n
        q(i) = i
        do j = 1, m
            q(i) = 0.5 * (q(i) + i / q(i))
        end do
    end do
    !$omp end parallel do simd
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    do concurrent (i = 1:n)
        q(i) = i
        do j = 1, m
            q(i) = 0.5 * (q(i) + i / q(i))
        end do
    end do
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    do concurrent (i = 1:n_2, ii=1:n_1)
        q(i*n_1 + ii) = i
        do j = 1, m
            q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
        end do
    end do
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    do i=1,n_2
        do ii=1,n_1
            q(i*n_1 + ii) = i
            do j = 1, m
                q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
            end do
        end do
    end do
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n_2
        do ii=1,n_1
            q(i*n_1 + ii) = i
            do j = 1, m
                q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
            end do
        end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    !$omp parallel do simd
    do i=1,n_2
        do ii=1,n_1
            q(i*n_1 + ii) = i
            do j = 1, m
                q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
            end do
        end do
    end do
    !$omp end parallel do simd
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    !$omp parallel do
    do i=1,n_2
        !$omp simd
        do ii=1,n_1
            q(i*n_1 + ii) = i
            do j = 1, m
                q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
            end do
        end do
        !$omp end simd
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    !$omp parallel do collapse(2)
    do i=1,n_2
        do ii=1,n_1
            q(i*n_1 + ii) = i
            do j = 1, m
                q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
            end do
        end do
    end do
    !$omp end parallel do
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

    t0 = omp_get_wtime()
    !$omp parallel do simd collapse(2)
    do i=1,n_2
        do ii=1,n_1
            q(i*n_1 + ii) = i
            do j = 1, m
                q(i*n_1 + ii) = 0.5 * (q(i*n_1 + ii) + (i*n_1 + ii) / q(i*n_1 + ii))
            end do
        end do
    end do
    !$omp end parallel do simd
    print *, iloop, omp_get_wtime() - t0
    iloop = iloop + 1

end program test_do
