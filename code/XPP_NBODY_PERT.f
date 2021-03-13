CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE XPP_NBODY_PERT(VTIME, VSTATE, VDIM, OSTATE)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VTIME
        REAL*8 VSTATE(6), OSTATE(6)

        INTEGER I,J
        REAL*8 MU_J
        REAL*8 RJI(6), RJ(6), RJ0(6)
        REAL*8 RTEMP(3)
        REAL*8 T_OSTATE, LTIME, TEMP_MOD



C
C       X' is V, which is taken from the input state vector
C

        DO I=1,3
            OSTATE(I)=VSTATE(I+3)
        END DO

        DO I=1,3
            OSTATE(I+3)=0.0D0
        END DO

        IF(PERTURBATION) THEN
            DO I=1,6
                PERT_ARRAY(I)=0.0D0
            END DO

            DO I=1,3
                PERT_ARRAY(I)=OSTATE(I)
            END DO
        END IF

        IF(SYS_TYPE .EQ. 0) THEN
            WRITE(*,*) 'Error in XPP_NBODY_PERT.'
            WRITE(*,*) 'Two body system not defined.'
            WRITE(*,*) 'Call START_TWOBODYSYSTEM to define a system.'
            STOP
        END IF



C
C       For the primary body. Call START_TWOBODYSYSTEM before this
C       routine.
C

        CALL GET_MU(IPB, MU_J)

!        CALL SPKEZ(IPB, VTIME, 'J2000', 'NONE', 0,
!     &RJ, LTIME)

        CALL GET_POS_WRT_IBC(VTIME, IPB, RJ)

        !DO I=1,6
        !  RJ(I)=0.00D0
        !END DO


        !CALL TRANS_ORIGIN_FROMSSB(VTIME, IBC, RJ0, RJ)


        DO I=1,6
            RJI(I)= VSTATE(I)-RJ(I)
        END DO

        CALL GET_VEC_MOD(RJI, TEMP_MOD)

        IF(TEMP_MOD .NE. 0 ) THEN
            DO I=1,3

                T_OSTATE=(-1.0D0*MU_J)*RJI(I)
                OSTATE(I+3)=T_OSTATE/(TEMP_MOD)**3.0D0

            END DO
        END IF





C
C       For all bodies in the system.
C



        DO J=1,MMAX

        IF(LIST_I(J,1) .NE. IPB) THEN


            CALL GET_POS_WRT_IBC(VTIME, LIST_I(J,1), RJ)

            DO I=1,3
                RJI(I)= VSTATE(I)-RJ(I)
            END DO

            CALL GET_VEC_MOD(RJI, TEMP_MOD)

            IF(TEMP_MOD .NE. 0 ) THEN

                    MU_J=0.D0
                    CALL GET_MU(LIST_I(J,1), MU_J)

                DO I=1,3

                    T_OSTATE=(-1.0D0*MU_J)*RJI(I)
                    T_OSTATE=T_OSTATE/(TEMP_MOD**3.0D0)

                    IF(PERTURBATION) THEN
                        PERT_ARRAY(I+3)=PERT_ARRAY(I+3)+T_OSTATE
                    END IF

                    OSTATE(I+3)=OSTATE(I+3)+T_OSTATE



                END DO

            END IF

        END IF
        END DO

        IF(PERTURBATION) THEN
            DO I=1,3
                PERT_ARRAY(I+3)=OSTATE(I+3)-PERT_ARRAY(I+3)
               ! WRITE(*,*) PERT_ARRAY(I+3)
            END DO
           ! PAUSE
        END IF


        RETURN
        END
