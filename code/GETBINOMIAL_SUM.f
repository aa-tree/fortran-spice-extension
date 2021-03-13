CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GETBINOMIAL_SUM(K_POW, X_VAL, SUM_B)

        IMPLICIT NONE

        INCLUDE 'common.dat'

        REAL*8 K_POW, X_VAL, SUM_B


        INTEGER I, J
        REAL*8 COEFF, FAC_TEMP

C        WRITE(*,*) 'Inputs', K_POW, X_VAL


        IF(X_VAL .LT. 1) THEN

C
C       For n=0, we will assign the first coefficient (1).
C

        SUM_B=1.0D0

C
C       For n=1, we will calculate the coefficient. (k*x)
C

        SUM_B=SUM_B+(K_POW*X_VAL)


C
C       Now for the rest of the series.
C

        DO I=2,100

            COEFF=K_POW

            DO J=1,I-1
                COEFF=COEFF*(K_POW-J)
            END DO

            CALL CALC_FACTORIAL(I, FAC_TEMP)

            COEFF=COEFF/FAC_TEMP


            SUM_B=SUM_B+(COEFF*(X_VAL)**I)

        END DO

        ELSE
            !WRITE (*,*) 'Error in GETBINOMIAL_SUM.'
            !WRITE (*,*) 'X is not less than 1.'
!           STOP
        END IF


        RETURN
        END
