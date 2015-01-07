subroutine MATRIXMEAN(matrix, m, n, mean)
    implicit none
    
    integer :: m, n, i, j
    !parameter(n, m)
    real(kind=8) :: matrix(m, n)
    real(kind=8) :: mean(n), temp_mean
    
    do i = 1,m
        temp_mean = 0
        do j = 1,n
            temp_mean = temp_mean + matrix(i, j)
        end do
        mean(i) = temp_mean / n
    end do
    
    
end subroutine
    