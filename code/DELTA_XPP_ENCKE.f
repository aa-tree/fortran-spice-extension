CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE DELTA_XPP_ENCKE(VTIME, VRHO, VSTATE,
     &VDIM, OSTATE)

        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VTIME
        REAL*8 VSTATE(VDIM), OSTATE(VDIM), VRHO(VDIM)

!
!       SUMMARY:
!       This routine implements a part of Encke's formulation of the
!       n-body problem. The routine only returns delta_r at a particular
!       time.
!
!       See Reference: Fundamentals of Astrodynamics, Bate, Mueller, and
!       White, Dover Publications, pp-390-396
!

!       VARIABLES:
!
!       NAME             TYPE               DESCRIPTION
!
!       VTIME            INPUT(REAL*8)      The time at which Delta_r
!                                           is to be calculated.
!
!       VRHO(6)          INPUT(REAL*8)      The state vector rho.
!
!       VSTATE(6)        INPUT(REAL*8)      The state vector r.
!
!       VDIM             INPUT(INTEGER)     The dimension of the system.
!                                           Cannot be any value other
!                                           than 6.
!
!       OSTATE(6)        OUTPUT(REAL*8)     State vector containing the
!                                           value of delta_r.
!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        INTEGER  I, J

        REAL*8 R_STATE(VDIM), TEMP_PERT(3)
        REAL*8 MOD_R, MOD_RHO, Q_VAR
        REAL*8 TEMP1, SUM_B, TEMP_MOD
        REAL*8 MU_P, MU_J

        REAL*8 PERT_GRAV(3,MMAX)

        REAL*8 RJ(VDIM), RJI(VDIM)

!
!       Overriding the value of VDIM
!
        VDIM=6
!
!       Assign zereos to the output vector.
!

        DO I=1,VDIM
          OSTATE(I)=0.00D0
          R_STATE(I)=VSTATE(I)+VRHO(I)
         ! WRITE(*,*) R_STATE(I),VRHO(I),VSTATE(I)
        END DO


        DO I=1,3
          OSTATE(I)=VSTATE(I+3)
        END DO

C
C       Calculate mods of vectors r and rho. And then calculate q.
C

        CALL GET_VEC_MOD(R_STATE, MOD_R)
        CALL GET_VEC_MOD(VRHO, MOD_RHO)
C        WRITE(*,*) 'MOD_R', MOD_R
C        WRITE(*,*) 'MOD_RHO', MOD_RHO
C        PAUSE


        IF(MOD_RHO .NE. 0) THEN



            TEMP1=(MOD_R/MOD_RHO)**2.0D0


            Q_VAR=(1.0D0-TEMP1) ! (Holds the value of 2q)

!            WRITE(*,*) 'QVAR', Q_VAR
!            PAUSE


            TEMP1=-1.50D0

            CALL GETBINOMIAL_SUM(TEMP1, Q_VAR, SUM_B)

            CALL GET_MU(IPB, MU_P)



C
C       The primary body term of the Encke's formulation.
C


            DO I=1,3
              TEMP1=((1.0D0-SUM_B)*R_STATE(I))
              TEMP1=TEMP1-VSTATE(I)
              OSTATE(I+3)=(MU_P*TEMP1)/(MOD_RHO**3.0D0)
              !PERT_GRAV(I,PRB_ID)=OSTATE(I) !Assign for analysis.
            END DO

!
!       To get the term a_p
!

!
!       Gravitational Perturbations from the model.
!

            DO I=1,3
                TEMP_PERT(I)=0.0D0
            END DO
            DO J=1,MMAX

            IF(LIST_I(J,1) .NE. IPB) THEN

              CALL GET_POS_WRT_IBC(VTIME, LIST_I(J,1), RJ)

              DO I=1,VDIM
                  RJI(I)= R_STATE(I)-RJ(I)
              END DO

              CALL GET_VEC_MOD(RJI, TEMP_MOD)

              IF(TEMP_MOD .NE. 0 ) THEN

                      MU_J=0.D0
                      CALL GET_MU(LIST_I(J,1), MU_J)

                  DO I=1,3
                      TEMP1=(-1.0D0*MU_J)*RJI(I)
                      TEMP1=TEMP1/(TEMP_MOD**3.0D0)
                      TEMP_PERT(I)=TEMP_PERT(I)+TEMP1
                    !  PERT_GRAV(I,J)=TEMP1 ! Saving Perturbations for analysis.
                  END DO

              END IF

          END IF
          END DO

            DO I=1,3
                OSTATE(I+3)=TEMP_PERT(I)+OSTATE(I+3)
            END DO

        ELSE
            WRITE(*,*) 'Error in DELTA_XPP_ENCKE. Mod of the vector rho'
            WRITE(*,*) ' is zero.'

        END IF


        RETURN
        END
