CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        SUBROUTINE GAUSS_CORR_RNANDP(VDIM, V_R_M4_4, V_R_M4_4_P,
     &VDELTAH,O_R_N_CORR)

        IMPLICIT NONE
        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VDELTAH
        REAL*8 V_R_M4_4(6,9), V_R_M4_4_P(6,9), O_R_N_CORR(6)
        REAL*8 V_RNP1(6)



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        REAL*8 O_S_N(3), O_CAP_S_N(3)
        INTEGER I,J,K
        REAL*8 G_bjk(10, 9), G_ajk(10,9)


        CALL GAUSS_GET_COEFF('b', G_bjk)
        CALL GAUSS_GET_COEFF('a', G_ajk)

!        CALL GAUSS_CALC_SN(VDIM, V_R_M4_4_P,
!     &VDELTAH, O_S_N)

        DO I=1,3
            O_S_N(I)=(V_R_M4_4(I+3,8)+V_R_M4_4_P(I+3,9))/2.0D0
            O_S_N(I) = O_S_N(I)+ GAUSS_LAST_S_N(I)
        END DO
C
C       Set output to zero to be safe.
C
        DO I=1,6
            O_R_N_CORR(I)=0.0D0
        END DO

C
C       Calc r_n'
C
        DO I=1,3
            DO K=-4,4
                O_R_N_CORR(I+3)=O_R_N_CORR(I+3)+(G_bjk(9,K+5)
     &*V_R_M4_4_P(I+3,9+K-4))
            END DO

            O_R_N_CORR(I+3)=(O_R_N_CORR(I+3)+O_S_N(I))*VDELTAH
        END DO


C
C       Calc r_n
C

        ! Calc new S_n
!        CALL GAUSS_CALC_CAP_SN(VDIM, V_R_M4_4, V_R_M4_4_P,
!     &VDELTAH, O_S_N, O_CAP_S_N)

        DO I=1,3
            O_CAP_S_N(I)=GAUSS_LAST_CAPS_N(I)+ GAUSS_LAST_S_N(I)
            O_CAP_S_N(I)=O_CAP_S_N(I)+(V_R_M4_4_P(I+3,8))/2.0D0
        END DO

        DO I=1,3
            DO K=-4,4
                O_R_N_CORR(I)=O_R_N_CORR(I)+(G_ajk(9,K+5)
     & *V_R_M4_4_P(I+3,9+K-4))
            END DO
            O_R_N_CORR(I)=(O_R_N_CORR(I)+O_CAP_S_N(I))
     & *VDELTAH*VDELTAH
        END DO




        RETURN
        END SUBROUTINE
