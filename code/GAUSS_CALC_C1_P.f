CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GAUSS_CALC_C1_P(VDIM, V_R_M4_4_P,
     &VDELTAH, C_1_P)

        IMPLICIT NONE
        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 V_R_M4_4_P(6, 9)
        REAL*8 VDELTAH, C_1_P(3)


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        REAL*8 G_bjk(10, 9)
        INTEGER I,J

        CALL GAUSS_GET_COEFF('b', G_bjk)

        DO J=1,3
            C_1_P(J)=V_R_M4_4_P(J, 5)/VDELTAH
        END DO


        DO I=1,9
            DO J=1,3
                C_1_P(J)=C_1_P(J)-(G_bjk(5,I)*V_R_M4_4_P(J+3,I))
            END DO
        END DO


        RETURN
        END SUBROUTINE
