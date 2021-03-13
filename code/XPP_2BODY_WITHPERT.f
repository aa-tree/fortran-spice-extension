CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE XPP_2BODY_WITHPERT(VTIME, VDIM, VSTATE, OSTATE)

        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VTIME
        REAL*8 VSTATE(VDIM), OSTATE(VDIM)



        INTEGER I,J
        REAL*8 MU_J
        REAL*8, DIMENSION(6) :: RJI, RJ
        REAL*8, DIMENSION(3) :: RTEMP
        REAL*8 LTIME, TEMP_MOD
C
C       X' is V, which is taken from the input state vector
C
        OSTATE(1)=VSTATE(4)
        OSTATE(2)=VSTATE(5)
        OSTATE(3)=VSTATE(6)
        OSTATE(4)=0.0D0
        OSTATE(5)=0.0D0
        OSTATE(6)=0.0D0



C
C FOR BODY 2
C
C
C        CALL GET_VEC_MOD(RTEMP, TEMP_MOD)
        DO J=1,MMAX

            IF(LIST_I(J,1) .NE. IBC) THEN

                DO I=1,3

                    CALL SPKEZ(LIST_I(J,1), VTIME, 'J2000', 'NONE', IBC,
     &RJ, LTIME)

                    CALL GET_MU(LIST_I(J,1), MU_J)

                    IF(MU_J .NE. 0) THEN

                        RJI=VSTATE(I)-RJ(I)

                        CALL GET_VEC_MOD(RJI, TEMP_MOD)

                        IF(TEMP_MOD .NE. 0) THEN

                            RTEMP(I)=(-1.0D0*MU_J*RJI(I))
                            RTEMP(I)=RTEMP(I)/(TEMP_MOD)**3.0D0
                            OSTATE(I+3)=OSTATE(I+3)+RTEMP(I)

                        END IF

                    ELSE

                    END IF
                END DO

            END IF
        END DO

        RETURN
        END






