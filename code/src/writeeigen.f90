SUBROUTINE WRITE_EIGENVECTORS( DESC, N, WI, V, LDV )
      CHARACTER*(*)    DESC
      INTEGER          N, LDV
      DOUBLE PRECISION WI( * ), V( LDV, * )
   
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0 )
      INTEGER          I, J
	  INTEGER,PARAMETER ::		   OUTUNIT = 1
	  OPEN(unit=OUTUNIT,file = "eigenvector_results")
      WRITE(OUTUNIT,*)
      WRITE(OUTUNIT,*) DESC
      DO I = 1, N
         J = 1
         DO WHILE( J.LE.N )
            IF( WI( J ).EQ.ZERO ) THEN
               WRITE(OUTUNIT,9998,ADVANCE='NO') V( I, J )
               J = J + 1
            ELSE
               WRITE(OUTUNIT,9999,ADVANCE='NO') V( I, J ), V( I, J+1 )
               WRITE(OUTUNIT,9999,ADVANCE='NO') V( I, J ), -V( I, J+1 )
               J = J + 2
            END IF
         END DO
         WRITE(OUTUNIT,*)
      END DO

 9998 FORMAT( 11(:,1X,F6.2) )
 9999 FORMAT( 11(:,1X,'(',F6.2,',',F6.2,')') )
      RETURN
      END