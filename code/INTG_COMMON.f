CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



        SUBROUTINE INTG_COMMON (INTEGRAT, VTIME, DIM_V, STATE,
     &DET,ROUTINE_EXTERNAL,OSTATE_R)

        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER DIM_V
        CHARACTER*(*) INTEGRAT
        REAL*8 VTIME, STATE(DIM_V), OSTATE_R(DIM_V)
        REAL*8 DET

        EXTERNAL ROUTINE_EXTERNAL

C
C       RK78 Variables.
C

        REAL*8 HMIN, HMAX, E1
        REAL*8 B(48),F(48),R(13,48)

        IF (INTEGRAT .EQ. 'RK45') THEN

          CALL RK4VEC (VTIME,DIM_V,STATE, DET, ROUTINE_EXTERNAL,
     &OSTATE_R)

        END IF


        IF (INTEGRAT .EQ. 'RK78') THEN

C          E1=1.D-13
C          HMIN=1.D-4
C          HMAX=1.D0
          E1=DET*1.0D-6
          HMIN=DET/100.D0
          HMAX=DET
          OSTATE_R=STATE

          CALL RK89 (VTIME,OSTATE_R,DIM_V, DET,HMIN,HMAX,E1,R,B,F,
     &ROUTINE_EXTERNAL)

          VTIME=VTIME-DET

        END IF

        RETURN
        END
