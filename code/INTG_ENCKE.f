CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE INTG_ENCKE (INTEGRAT, VTIME, DIM_V, RSTATE, STATE,
     &DET,ROUTINE_EXTERNAL,OSTATE_DR)

        IMPLICIT NONE

        INCLUDE 'common.dat'

        CHARACTER*(*) INTEGRAT
        INTEGER DIM_V
        REAL*8 VTIME, RSTATE(DIM_V), STATE(DIM_V), DET
        REAL*8 OSTATE_DR(DIM_V), V_DELR(DIM_V)


        INTEGER I
C
C       RK78 Variables.
C

        REAL*8 HMIN, HMAX, E1
        REAL*8 B(48),F(48),R(13,48)


        EXTERNAL ROUTINE_EXTERNAL

        IF (INTEGRAT .EQ. 'RK45') THEN

          DO I=1,DIM_V
            V_DELR(I)=RSTATE(I)-STATE(I)
          END DO
          CALL RK4VEC_ENCKE (VTIME, V_DELR, DIM_V, RSTATE, STATE, DET,
     &ROUTINE_EXTERNAL,OSTATE_DR)

        END IF

        IF (INTEGRAT .EQ. 'RK78') THEN
C          E1=1.D-13
C          HMIN=1.D-4
C          HMAX=1.D0
          E1=DET*1.0D-6
          HMIN=DET/100.D0
          HMAX=DET
          OSTATE_DR=RSTATE

          DO I=1,DIM_V
            OSTATE_DR(I)=RSTATE(I)-STATE(I)
          END DO

 !         CALL RK78_ENCKE (VTIME,OSTATE_DR, RSTATE, STATE,DIM_V, DET,
 !    &HMIN,HMAX,E1,R,B,F,ROUTINE_EXTERNAL)

          VTIME=VTIME-DET

        END IF




        RETURN
        END
