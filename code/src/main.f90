    program main
    implicit none
    integer iheight, iwidth, Maxwidth, Maxheight, i, j
    integer :: classes, training, N
	character(len=250) :: imgfolder
    parameter( classes=1, training=10 )
    parameter(Maxwidth=3220,Maxheight=2415)    
	parameter	(N=3)
	parameter ( imgfolder = 'C:\Users\Kaarel\Documents\Paralleelarvutused\projekt\code\pictures3' ) 

    integer image(Maxheight,Maxwidth)  
    real(kind = 8) :: mean(N), matmean(N, N), mat(N,N) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    real(kind=8) :: trainimg(classes*training, 92*112), meanimg(92*112), norm_img(classes*training, 92*112)
    DOUBLE PRECISION :: VR(N, N)
	logical :: istraining
    
    istraining = .TRUE.    
    call IMG2MAT(imgfolder, trainimg, classes, training, istraining)

    !Calculate mean image
    call MATRIXMEAN(trainimg, classes*training, 92*112, meanimg)
    call MATRIXNORM(trainimg, classes*training, 92*112, meanimg, norm_img)
    
    !print*, 'normalized image'
    !print*, norm_img

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
    
	call RIGHTEIGENVECTOR(mat, N, N, VR, N, 4*N)
	print *, 'Eigenvectors:', char(10), VR
	
	
end program