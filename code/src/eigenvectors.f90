subroutine RIGHTEIGENVECTOR(A, N, LDA, VR, LDVR)
	implicit none
    
    INTEGER          ::  INFO, LDA, LDVL=1, LDVR, LWORK, N
	DOUBLE PRECISION ::  RWORK( N*2 )
    COMPLEX*16       ::  A( N, N ), VL( LDVL, LDVL ), VR( LDVR, LDVR ),
     $                   W( N*2 ), WORK( N*2 )
	
	ZGEEV( 'N', 'V', N, A, LDA, W, VL, LDVL, VR, LDVR,
     $                  WORK, LWORK, RWORK, INFO )
end subroutine