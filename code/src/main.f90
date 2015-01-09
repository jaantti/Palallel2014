    program main
    implicit none
    integer iheight, iwidth, Maxwidth, Maxheight, i, j
    integer :: classes, training, N
	character(len=250) :: imgfolder
    parameter( classes=1, training=10 )
    parameter(Maxwidth=3220,Maxheight=2415)    
	parameter	(N=5)
	parameter ( imgfolder = 'C:\Users\Antti\GitHub\Palallel2014\code\pictures3' ) 
    integer image(Maxheight,Maxwidth)  
    double precision :: mean(N), matmean(N, N)
    !double precision :: mat(N,N) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    double precision :: mat(N,N) = (/ -1.01, 3.98, 3.30, 4.43, 7.31, 0.86, 0.53, 8.26, 4.96,-6.43, -4.60,-7.04,-3.89,-7.66,-6.16, 3.31, 5.29, 8.20,-7.33, 2.47, -4.81, 3.55,-1.51, 6.18, 5.58 /)
    real(kind=8) :: trainimg(classes*training, 92*112), meanimg(92*112), norm_img(classes*training, 92*112)
    DOUBLE PRECISION :: VR(N, N), VL(N, N), WR(N, N), WI(N, N)
	logical :: istraining
    
    istraining = .TRUE.    
    call IMG2MAT(imgfolder, trainimg, classes, training, istraining)

    !Calculate mean image
    call MATRIXMEAN(trainimg, classes*training, 92*112, meanimg)
    call MATRIXNORM(trainimg, classes*training, 92*112, meanimg, norm_img)
    
    !print*, 'normalized image'
    !print*, norm_img

    do i = 1,N
        do j = 1,N
            print *, mat(i,j)
        end do
    end do
    
    call MATRIXMEAN(mat, N, N, mean)
    call MATRIXNORM(mat, N, N, mean, matmean)

    print *, 'mean:'
    print *, mean
    
    print *, 'norm:'
    do i = 1, N
        do j = 1, N
            print *, matmean(i,j)
        end do
    end do
	
    !Calculates using mat, not matmean (normalized image)
	call RIGHTEIGENVECTOR(mat, N, N, VR, VL, N, WR, WI)
	print *, 'Eigenvectors:', char(10), VR	    
    
    CALL PRINT_EIGENVECTORS( 'Left eigenvectors', N, WI, VL, N )    
    CALL PRINT_EIGENVECTORS( 'Right eigenvectors', N, WI, VR, N )
	
end program