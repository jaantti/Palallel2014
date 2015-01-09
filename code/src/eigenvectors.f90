subroutine RIGHTEIGENVECTOR(A, N, LDA, VR, VL, LDVR, LWORK, INFO)
	implicit none

    INTEGER          ::  N, INFO, LDA, LDVL, LDVR, LWORK
    DOUBLE PRECISION ::  A( LDA, N ), VL( N, N ), VR( N, N ), WR( N ), WI(N), WORK( MAX(1,LWORK) )
	
	LDVL = N
	call DGEEV( 'N', 'V', N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
end subroutine