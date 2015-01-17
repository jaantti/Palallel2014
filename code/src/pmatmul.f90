subroutine PMATMUL(A, B, C, ar, ac, bc)
    implicit none
    include 'omp_lib.h'
    integer :: ar, ac, bc
    double precision :: A(ar,ac), B(ac,bc), C(ar,bc)
    integer :: i, j, k
    
    do i = 1, ar
        C(i,:) = 0
    end do
    
    !$OMP PARALLEL DO    
    do i = 1, ar
        !!$OMP PARALLEL DO
        do j = 1, bc
            do k = 1, ac
                C(i,j) = C(i,j) + A(i,k)*B(k,j)
            end do
        end do
        !!$OMP END PARALLEL DO
    end do
    !$OMP END PARALLEL DO
        
    !print*, 'ar', ar
    !print*, 'ac', ac
    
end subroutine