subroutine IMG2MAT(imgfolder, img_train, img_test, classes, training, istraining)
    integer :: classes, training, i, j, iwidth, iheight
    logical :: istraining
    real(kind = 8) :: img_train(92*112, classes*training)
    real(kind = 8) :: img_test(92*112, classes*(10-training))    
    real(kind = 8) :: img(92,112)
    character(len = 250) :: imgfolder
    character(len = 250) :: imgpath
    character(len = 5) :: nr, nr2
    real :: starttime, endtime
    real(kind = 8) :: imgvector(92*112, 1)
    

    call CPU_TIME(starttime)
    do i = 1, classes            
        write(nr, '(i5)') i
        nr = adjustl(nr)
        
        do j = 1, training
            write(nr2, '(i5)') j
            nr2 = adjustl(nr2)
            imgpath = trim(imgfolder) // '\s' // trim(nr) // '\' // trim(nr2) // '.bmp'
            print *, trim(imgpath)                
            call READBMP(img, iwidth, iheight, imgpath) 
            !print*, i, j, 'img'
            !print*, img                
            imgvector = reshape(img, (/ 10304, 1 /))

            img_train(:, i*j) = imgvector(:, 1)
        end do            
    end do
    
    do i = 1, classes
        write(nr, '(i5)') i
        nr = adjustl(nr)
        
        do j = training + 1, 10
            write(nr2, '(i5)') j
            nr2 = adjustl(nr2)
            imgpath = trim(imgfolder) // '\s' // trim(nr) // '\' // trim(nr2) // '.bmp'
            print *, trim(imgpath)                
            call READBMP(img, iwidth, iheight, imgpath) 
            !print*, i, j, 'img'
            !print*, img                
            imgvector = reshape(img, (/ 10304, 1 /))

            img_test(:, i*(j-training)) = imgvector(:, 1)
        end do
    end do
    call CPU_TIME(endtime)
        
    print*, 'time:', endtime-starttime        

    
        
end subroutine