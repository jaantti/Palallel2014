subroutine EUCLDIST(V1, V2, N, DIST)
    implicit none
    integer :: N, i
    double precision :: V1(N), V2(N), DIST, sum
    
    sum = 0    
    do i = 1, N
        sum = sum + (V1(i) - V2(i))**2
    end do
    
    DIST = SQRT(sum)
    
       
end subroutine
    