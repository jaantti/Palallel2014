subroutine RIGHTEIGENVECTOR(A, N, LDA, VR, LDVR,LWORK)
	implicit none

    INTEGER          ::  N, INFO, LDA, LDVL, LDVR, LWORK, LWMAX
	PARAMETER		   ( LWMAX = 1000 ) 
    DOUBLE PRECISION ::  A( LDA, N ), VL( 1, 1 ), VR( LDVR, * ), WR( N ), WI(N), WORK( LWMAX )
	
	LDVL = 1
	LWORK = -1
	! Query optimal workspace
	call DGEEV( 'N', 'V', N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
	LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
	! Calculate eigenvectors
	call DGEEV( 'N', 'V', N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
	
	! Check for convergence
	IF( INFO.GT.0 ) THEN
      WRITE(*,*)'The algorithm failed to compute eigenvalues.'
      STOP
    END IF
end subroutine