CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE CALC_FACTORIAL(NUM, FACTORIAL)


        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER NUM
        REAL*8 FACTORIAL

!       SUMMARY:
!
!       This routine computes factorial of a number.
!
!       VARIABLES:
!
!       NAME            TYPE            DESCRIPTION
!
!       NUM             Input(REAL*8)   The number that you want the
!                                        factorial of.
!
!       FACTORIAL       Output(REAL*8)  The factorial.
!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INTEGER I

        FACTORIAL=NUM

        DO I=1,NUM-1

            FACTORIAL=FACTORIAL*(NUM-I)

        END DO


        RETURN
        END
