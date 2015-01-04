program main
    implicit none
    integer iheight, iwidth, Maxwidth, Maxheight, i, j
    parameter(Maxwidth=3220,Maxheight=2415)    
    integer image(Maxheight,Maxwidth,3)
    
    call READBMP(image, iheight, iwidth)
    
    print*, 'image dimensions', iheight, iwidth
    
    do i = 1, iheight
        do j = 1, iwidth
            print*, i, j, image(i,j,1), image(i,j,2), image(i,j,3) 
        end do
    end do
    
end program