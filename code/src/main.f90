program main
    implicit none
    include 'omp_lib.h'        
    integer iheight, iwidth, Maxwidth, Maxheight, i, j
    integer :: classes, nTraining, nTest, N, LDA, LDVL, LDVR, LWMAX, class_train, dim1, dim2
	character(len=250) :: imgfolder
	parameter( dim1=92, dim2=112)
    parameter( classes=40, nTraining=7, nTest = classes-nTraining )
    parameter(class_train = classes*nTraining)
    parameter(Maxwidth=3220,Maxheight=2415)
	parameter(N=5)
	parameter(LDA=class_train, LDVL=class_train, LDVR=class_train, LWMAX = 1000)
	parameter ( imgfolder = '/gpfs/hpchome/jaantti/Palallel2014/code/pictures3' )
    !parameter ( imgfolder = 'C:\Users\Antti\GitHub\Palallel2014\code\pictures3' )
    integer image(Maxheight,Maxwidth)  
    double precision :: mean(N), matmean(N, N)
    double precision :: trainimg(dim1*dim2, classes*nTraining), testimg(dim1*dim2, classes*nTest), meanimg(dim1), norm_img(dim1*dim2, classes*nTraining), norm_test(dim1*dim2, classes*nTest), t_img(classes*nTraining, dim1*dim2)
	double precision :: VR(class_train, class_train), VL(class_train, class_train), WR(class_train, class_train), WI(class_train, class_train)
    double precision :: Y(dim1*dim2, class_train), eig_temp(class_train, class_train), train_vec(class_train, class_train), test_vec(class_train, classes*nTest)
    logical :: istraining
    
    double precision :: min_dist(classes*(nTest)), new_dist
    integer :: min_index(classes*(nTest))
    double precision recog;
    double precision performance
    double precision t1, t2, tstart, tfinish
	
	INTEGER :: LWORK,INFO
	double precision :: WORK(5*class_train)
	
	! -- Eigenvector operability testing --
	!double precision :: testWR(5,5), testWI(5,5), testVL(5,5), testVR(5,5),testWORK(20,20)
	!double precision :: mat(N,N) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    !double precision :: mat(5,5) = (/ -1.01, 3.98, 3.30, 4.43, 7.31, 0.86, 0.53, 8.26, 4.96,-6.43, -4.60,-7.04,-3.89,-7.66,-6.16, 3.31, 5.29, 8.20,-7.33, 2.47, -4.81, 3.55,-1.51, 6.18, 5.58 /)
	!! CALL DGEEV( 'N', 'V', 5, mat, 5, testWR, testWI, testVL, 5, testVR, 5, testWORK, 20, INFO )
	!! CALL PRINT_EIGENVECTORS( 'Test Right eigenvectors', 5, testWI, testVR, 5 )
    
	
	
	!call getarg(1,imgfolder)
	
	! -- Read input files --
    print*,'Started reading.'
	t1 = omp_get_wtime()
	istraining = .TRUE.    
    call IMG2MAT(imgfolder, trainimg, testimg, classes, nTraining, istraining)
			!call IMG2MAT('/gpfs/hpchome/jaantti/Palallel2014/code/pictures3', trainimg, testimg, classes, nTraining, istraining)
    !print*, 'trainimg', trainimg
	t2 = omp_get_wtime()
    write (*,990) 'img matmean+matnorm:', t2-t1
	
    ! -- Calculate mean image --
    tstart = omp_get_wtime()
    t1 = omp_get_wtime()
    call MATRIXMEAN(trainimg, classes*nTraining, dim1*dim2, meanimg)
    call MATRIXNORM(trainimg, classes*nTraining, dim1*dim2, meanimg, norm_img)
    t2 = omp_get_wtime()
    write (*,990) 'img matmean+matnorm:', t2-t1
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
    t1 = omp_get_wtime()
    t_img = transpose(norm_img)
    t2 = omp_get_wtime()
    write (*,990) 't_img(transpose norm_img)', t2-t1
    
	t1 = omp_get_wtime()
    eig_temp = MATMUL(t_img, norm_img)
    t2 = omp_get_wtime()
    write (*,990) 'eig_temp (matmul t_img*norm_img)', t2-t1
    
    t1 = omp_get_wtime()
    t_img = transpose(norm_img)
    t2 = omp_get_wtime()
    write (*,990) 't_img(transpose norm_img) (again)', t2-t1
    !print*, 'norm_img', norm_img
    !print*, 'eig_temp', SIZE(eig_temp, 1),SIZE(eig_temp, 2)
	!print*,'class_train', class_train
	
	! -- Calculate eigenvector WORK size -- !! broken, gives 0
	LWORK = -1
	call DGEEV( 'N', 'V', class_train, eig_temp, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
	!print*, 'work', MIN( LWMAX, INT( WORK( 1 ) ) ) 
	!print*, 'info', INFO
	! LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )  !! gives 0, not usable?
	LWORK = 200 * class_train
	
	! -- Find eigenvectors --
    t1 = omp_get_wtime()
	call DGEEV( 'N', 'V', class_train, eig_temp, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
    t2 = omp_get_wtime()
    write (*,990) 'eigenvectors (DGEEV)', t2-t1

	!print *, 'Eigenvectors:', char(10), VR	    
    !CALL WRITE_EIGENVECTORS( 'Right eigenvectors', class_train, WI, VR, class_train )
    !CALL PRINT_EIGENVECTORS( 'Left eigenvectors', N, WI, VL, N )    
	!CALL PRINT_EIGENVECTORS( 'Right eigenvectors', class_train, WI, VR, class_train )
        
    ! -- Calculate training vector --
    t1 = omp_get_wtime()
    Y = MATMUL(norm_img, VR)
    t2 = omp_get_wtime()
    write (*,990) 'Y (matmul norm_img*eig) ', t2-t1
    ! -- Reduce dimensionality --
    t1 = omp_get_wtime()
    train_vec = MATMUL(TRANSPOSE(Y), norm_img)
    t2 = omp_get_wtime()
    write (*,990) 'train_vec (matmul transpose(Y)*norm_img)', t2-t1
    
    ! -- Testing --
    ! -- Normalize testing images --
	t1 = omp_get_wtime()
    call MATRIXNORM(testimg, classes*(nTest), dim1*dim2, meanimg, norm_test)  
    ! -- Calculate testing matrix --
    test_vec = MATMUL(TRANSPOSE(Y), norm_test)
    t2 = omp_get_wtime()
	write (*,990) 'test_vec (matnorm + matmul transpose(Y)*norm_test)', t2-t1
	
    ! -- Calculate estimated class for image --
	t1 = omp_get_wtime()
    do i = 1, classes * (nTest)
        min_dist(i) = HUGE(min_dist)
        min_index(i) = 0;
        !print*, 'mindist', min_dist(i)
        do j = 1, class_train
            call EUCLDIST(train_vec(:,j), test_vec(:,i), class_train, new_dist)
            
            if (new_dist < min_dist(i)) then
				!print *,'newnearest',i, new_dist
                min_dist(i) = new_dist
                min_index(i) = FLOOR((j-1.0)/nTraining)+1
            end if
        end do        
    end do
    t2 = omp_get_wtime()
	write (*,990) 'classify (eucldist in double loop)', t2-t1
    
    ! -- Calculate performance --
	recog = 0;
    do i = 1,classes * (nTest)
        if (min_index(i) == FLOOR((i-1.0)/(nTest)) + 1) then
            recog = recog + 1
        end if
    end do
    
!	print*,'WR sum',SUM(WR)
!	print*,'WI sum',SUM(WI)
!    print*, 'minIndex',min_index
!    print*, 'minDist', min_dist
!    print*, 'recog', recog
    
    performance = recog / (classes * nTest)
    print*, 'Performance', performance
    tfinish = omp_get_wtime()
    write (*,990) 'total time', tfinish-tstart
	
	
	990 format (A, F8.6)
end program