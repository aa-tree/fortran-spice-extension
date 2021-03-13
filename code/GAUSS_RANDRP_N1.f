CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GAUSS_RANDRP_N1(VDIM, V_R_M4_4, V_R_M4_4_P,
     &VDELTAH, O_R_N)

        IMPLICIT NONE
        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VDELTAH
        REAL*8 V_R_M4_4(6,9), V_R_M4_4_P(6,9), O_R_N(6)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INTEGER I,J
        REAL*8 O_S_N(3,9), G_ajk(10,9), G_bjk(10,9)
        REAL*8 O_CAP_S_N(3,9), O_CAP_S_NP1(3)

C        CALL GAUSS_CALC_SN(VDIM, V_R_M4_4_P, VDELTAH, O_S_N)

        CALL GAUSS_GET_COEFF('b', G_bjk)
        CALL GAUSS_GET_COEFF('a', G_ajk)

        DO I=1,6
            O_R_N(I)=0.0D0
        END DO

        DO I=4,6
            DO J=-4,4
                O_R_N(I)=O_R_N(I)+G_bjk(10,J+5)*V_R_M4_4_P(I,9+J-4)
            END DO

            O_R_N(I)=VDELTAH*(O_R_N(I)+GAUSS_LAST_S_N(I-3)+
     &(V_R_M4_4_P(I,9)/2.0D0))

        END DO

!        CALL GAUSS_CALC_CAP_SN(VDIM, V_R_M4_4, V_R_M4_4_P,
!     &VDELTAH, O_S_N, O_CAP_S_N)


C
C       Calc. S_n+1
C

        DO I=1,3
            O_CAP_S_NP1(I)=GAUSS_LAST_CAPS_N(I)+GAUSS_LAST_S_N(I)+
     &(V_R_M4_4_P(I+3,9)/2.0D0)
        END DO

        DO I=1,3
            DO J=-4,4
                O_R_N(I)=O_R_N(I)+
     & (G_ajk(10,J+5)*V_R_M4_4_P(I+3,9+J-4))
            END DO

            O_R_N(I)=VDELTAH*VDELTAH*(O_R_N(I)+O_CAP_S_NP1(I))
        END DO


        RETURN
        END SUBROUTINE





