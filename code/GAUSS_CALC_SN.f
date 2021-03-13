CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GAUSS_CALC_SN(VDIM, V_R_M4_4_P,
     &VDELTAH, O_S_N)

        IMPLICIT NONE
        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 V_R_M4_4(6, 9), V_R_M4_4_P(6, 9)
        REAL*8 VDELTAH
        REAL*8 O_S_N(3,9)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        REAL*8 C_1_P(3)
        INTEGER I,J
C
C       For n=0.
C       The bias is 5.
C


        DO I=1,3
           DO J=1,9
            O_S_N(I,J)=0.0D0
           END DO
        END DO


        CALL GAUSS_CALC_C1_P(VDIM, V_R_M4_4_P,
     &VDELTAH, C_1_P)

        DO I=1,3
            O_S_N(I,5)=C_1_P(I)
        END DO


C
C       For n>0
C

        DO I=6,9
            DO J=1,3
                O_S_N(J,I)=(V_R_M4_4_P(J+3,I-1)+V_R_M4_4_P(J+3,I))
     &/2.0D0
                O_S_N(J,I)=O_S_N(J,I-1)+O_S_N(J,I)
            END DO
        END DO


C
C       For n<0
C

        DO I=4,1,-1
            DO J=1,3
                O_S_N(J,I)=(V_R_M4_4_P(J+3,I+1)+V_R_M4_4_P(J+3,I))
     &/2.0D0
                O_S_N(J,I)=O_S_N(J,I+1)-O_S_N(J,I)
            END DO

        END DO

        RETURN
        END SUBROUTINE
