subroutine RIGHTEIGENVECTOR(A, N, LDA, VR, LDVR,LWORK)
	implicit none

    INTEGER          ::  N, INFO, LDA, LDVL, LDVR, LWORK
    DOUBLE PRECISION ::  A( LDA, N ), VL( 1, 1 ), VR( LDVR, * ), WR( N ), WI(N), WORK( MAX(1,LWORK) )
	
	LDVL = 1
	call DGEEV( 'N', 'V', N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
end subroutine