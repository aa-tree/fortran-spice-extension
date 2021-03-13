CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE XPP_TWOBODY_NO_PERT(VTIME, VSTATE, VDIM, OSTATE)


        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VTIME
        REAL*8 VSTATE(VDIM), OSTATE(VDIM)



        INTEGER I,J
        REAL*8 MU_J
        REAL*8, DIMENSION(6) :: RJI, RJ, O_POS
        REAL*8, DIMENSION(3) :: RTEMP
        REAL*8 T_OSTATE, LTIME, TEMP_MOD
C
C       X' is V, which is taken from the input state vector
C
        OSTATE(1)=VSTATE(4)
        OSTATE(2)=VSTATE(5)
        OSTATE(3)=VSTATE(6)
        OSTATE(4)=0
        OSTATE(5)=0
        OSTATE(6)=0


C
C        FOR BODY 1 (Primary body)
C

        CALL GET_MU(IPB, MU_J)


C       CALL GET_INDEX_LIST(IPB, J)
        IF(SYS_TYPE .EQ. 2) THEN

C            CALL SPKEZ(IPB, VTIME, 'J2000', 'NONE', IBC,
C     &RJ, LTIME)

            CALL GET_POS_WRT_IBC(VTIME, IPB, RJ)





            DO I=1,3
                RJI(I)= VSTATE(I)-RJ(I)
            END DO

            CALL GET_VEC_MOD(RJI, TEMP_MOD)

            IF(TEMP_MOD .NE. 0 ) THEN
                DO I=1,3

                    T_OSTATE=(-1*MU_J)*RJI(I)
                    OSTATE(I+3)=T_OSTATE/(TEMP_MOD)**3.0D0

                END DO
            END IF

        ELSE
            WRITE(*,*) 'Two body system not defined.'
            WRITE(*,*) 'CALL START_TWOBODYSYSTEM before running this'
            WRITE(*,*) ' routine.'
            WRITE(*,*) 'Killing program.'
            STOP
        END IF



        RETURN
        END
