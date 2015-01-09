subroutine RIGHTEIGENVECTOR(A, N, LDA, VR, VL, LDVR, WR, WI)
	implicit none

    INTEGER          ::  N, INFO, LDA, LDVL, LDVR, LWORK, LWMAX
	PARAMETER		   ( LWMAX = 1000 ) 
    DOUBLE PRECISION ::  A( LDA, N ), VL( N, N ), VR( N, N ), WR( N ), WI(N), WORK( LWMAX )
	
	LDVL = N
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