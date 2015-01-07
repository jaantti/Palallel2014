    program main
    implicit none
    integer iheight, iwidth, Maxwidth, Maxheight, i, j
    parameter(Maxwidth=3220,Maxheight=2415)    
    integer image(Maxheight,Maxwidth,3)
    !integer, dimension(3,3) :: mat = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    real(kind = 8) :: mean(3), matmean(3, 3), mat(3,3) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    
    call READBMP(image, iheight, iwidth)
    
    print*, 'image dimensions', iheight, iwidth    
    
    !do i = 1, iheight
    !    do j = 1, iwidth
    !        print*, i, j, image(i,j,1), image(i,j,2), image(i,j,3) 
    !    end do
    !end do

    do i = 1,3
        do j = 1,3
            print *, mat(i,j)
        end do
    end do
    
    call MATRIXMEAN(mat, 3, 3, mean)
    call MATRIXNORM(mat, 3, 3, mean, matmean)

    print *, 'mean:'
    print *, mean
    
    print *, 'norm:'
    do i = 1, 3
        do j = 1, 3
            print *, matmean(i,j)
        end do
    end do
    
    
end program