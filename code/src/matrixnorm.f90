subroutine MATRIXNORM(matrix, m, n, mean, matnorm)
    integer :: m, n, i, j
    real(kind = 8) :: mean(n), matnorm(m, n), matrix(m,n)
    
    do i = 1, m
        do j = 1, n
            matnorm(i, j) = matrix(i, j) - mean(i)
        end do
    end do
    
end subroutine
    