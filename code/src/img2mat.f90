subroutine IMG2MAT(imgfolder, imgmat, classes, training, istraining)
    integer :: classes, training, i, j, iwidth, iheight
    logical :: istraining
    real(kind = 8) :: imgmat(classes*training, 92*112), img(92,112)
    character(len = 250) :: imgfolder
    character(len = 250) :: imgpath
    character(len = 5) :: nr, nr2
    real :: starttime, endtime
    real(kind = 8) :: imgvector(1, 92*112)
    
    if (istraining) then
        call CPU_TIME(starttime)
        do i = 1, classes            
            write(nr, '(i5)') i
            nr = adjustl(nr)
        
            do j = 1, training
                write(nr2, '(i5)') j
                nr2 = adjustl(nr2)
                imgpath = trim(imgfolder) // '\s' // trim(nr) // '\' // trim(nr2) // '.bmp'
                print *, imgpath                
                call READBMP(img, iwidth, iheight, imgpath) 
                !print*, i, j, 'img'
                !print*, img                
                imgvector = reshape(img, (/ 1, 10304 /))

                imgmat(i*j, :) = imgvector(1,:)
            end do
            
        end do
        
        print*, 'time:', endtime-starttime
        !call WRITE_MATRIX(imgmat)
        
        !print*, 'matrix:'
        !print*, imgmat
        !do i = 1,classes*training
        !    do j = 1,92*112
        !        print*, i, j, imgmat(i,j)
        !    end do
        !end do
        
    else
        
    end if
    
        
end subroutine