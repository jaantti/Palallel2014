    program main
    implicit none
    integer iheight, iwidth, Maxwidth, Maxheight, i, j
    integer :: classes, training, N, class_train
	character(len=250) :: imgfolder
    parameter( classes=10, training=9 )
    parameter(class_train = classes*training)
    parameter(Maxwidth=3220,Maxheight=2415)    
	parameter	(N=5)
	parameter ( imgfolder = 'C:\Users\Antti\GitHub\Palallel2014\code\pictures3' ) 
    integer image(Maxheight,Maxwidth)  
    double precision :: mean(N), matmean(N, N)
    !double precision :: mat(N,N) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    double precision :: mat(N,N) = (/ -1.01, 3.98, 3.30, 4.43, 7.31, 0.86, 0.53, 8.26, 4.96,-6.43, -4.60,-7.04,-3.89,-7.66,-6.16, 3.31, 5.29, 8.20,-7.33, 2.47, -4.81, 3.55,-1.51, 6.18, 5.58 /)
    real(kind=8) :: trainimg(92*112, classes*training), testimg(92*112, classes*(10-training)), meanimg(92*112), norm_img(92*112, classes*training), norm_test(92*112, classes*(10-training))
    !DOUBLE PRECISION :: VR(N, N), VL(N, N), WR(N, N), WI(N, N)
	double precision :: VR(class_train, class_train), VL(class_train, class_train), WR(class_train, class_train), WI(class_train, class_train)
    double precision :: Y(92*112, class_train), eig_temp(class_train, class_train), train_vec(class_train, class_train), test_vec(class_train, classes*(10-training))
    logical :: istraining
    
    double precision :: min_dist(classes*(10-training)), new_dist
    integer :: min_index(classes*(10-training))
    integer recog;
    double precision performance
    
    istraining = .TRUE.    
    call IMG2MAT(imgfolder, trainimg, testimg, classes, training, istraining)
    
    print*, 'trainimg', trainimg
    
    !Calculate mean image
    call MATRIXMEAN(trainimg, classes*training, 92*112, meanimg)
    call MATRIXNORM(trainimg, classes*training, 92*112, meanimg, norm_img)
    
    !print*, 'normalized image'
    !print*, norm_img

    !do i = 1,N
    !    do j = 1,N
    !        print *, mat(i,j)
    !    end do
    !end do
    !
    !call MATRIXMEAN(mat, N, N, mean)
    !call MATRIXNORM(mat, N, N, mean, matmean)
    !
    !print *, 'mean:'
    !print *, mean
    !
    !print *, 'norm:'
    !do i = 1, N
    !    do j = 1, N
    !        print *, matmean(i,j)
    !    end do
    !end do
	
    eig_temp = MATMUL(TRANSPOSE(norm_img), norm_img)
    !print*, 'norm_img', norm_img
    !print*, 'eig_temp', eig_temp
    
	call RIGHTEIGENVECTOR(eig_temp, N, N, VR, VL, N, WR, WI)
	!print *, 'Eigenvectors:', char(10), VR	    
    
    !CALL PRINT_EIGENVECTORS( 'Left eigenvectors', N, WI, VL, N )    
    !CALL PRINT_EIGENVECTORS( 'Right eigenvectors', class_train, WI, VR, class_train )
        
    !Calculate training vector
    Y = MATMUL(norm_img, VR)
    !Reduce dimensionality
    train_vec = MATMUL(TRANSPOSE(Y), norm_img)
    
    !Testing
    !Normalize testing images
    call MATRIXNORM(testimg, classes*(10-training), 92*112, meanimg, norm_test)
    
    !Calculate testing matrix
    test_vec = MATMUL(TRANSPOSE(Y), norm_test)
    
    !Calculate estimated class for image
    do i = 1, classes * (10 - training)
        min_dist(i) = HUGE(min_dist)
        min_index(i) = 0;
        !print*, 'mindist', min_dist(i)
        do j = 1, class_train
            call EUCLDIST(train_vec(:,j), test_vec(:,i), class_train, new_dist)
            !print *, new_dist
            if (new_dist < min_dist(i)) then
                min_dist(i) = new_dist
                min_index(i) = FLOOR((j-1.0)/training)+1
            end if
        end do        
    end do
        
    !Calculate performance
	recog = 0;
    do i = 1,classes * (10 - training)
        if (min_index(i) == FLOOR((i-1.0)/(10 - training)) + 1) then
            recog = recog + 1
        end if
    end do
    
    print*, 'minIndex',min_index
    print*, 'minDist', min_dist
    print*, 'recog', recog
    
    performance = recog / (classes * (10.0 - training))
    print*, 'Performance', performance
    
end program