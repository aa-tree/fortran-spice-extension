CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE CONVERGENCE_TEST_VECTOR(V_APPROX, V_EXACT, O_RESULT)

        REAL*8 V_APPROX(6), V_EXACT(6)
        REAL*8 O_RESULT(2)

!       SUMMARY:
!
!       This routine computes the convergence of two vectors. It
!       accepts two inputs, the vector for which you want the result,
!       and the vector you want to compare it to. The result
!       contains the absolute and relative errors between the two vectos.
!
!       VARIABLES:
!
!       NAME             TYPE            DESCRIPTION
!
!       V_APPROX(6)      Input(REAL*8)   The vector for which you want
!                                        to calculate the error values.
!
!       V_EXACT(6)       Input(REAL*8)   The vector which that target
!                                        vector is supposed to be
!                                        compared to.
!
!       O_RESULT(2)      Output(REAL*8)  The output containing error
!                                        values. O_RESULT(1) holds the
!                                        Absolute Error, and O_RESULT(2)
!                                        hold the Relative Error.

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INTEGER I
        REAL*8 V_DIFF(3)
        REAL*8 MOD_DIFF, MOD_EXACT

        DO I=1,3
            V_DIFF(I)=V_APPROX(I)-V_EXACT(I)
          !  write(*,*) 'Vec diff', V_APPROX(I), V_EXACT(I), v_diff(i)
        END DO

        DO I=1,2
            O_RESULT(I)=0.0D0
        END DO

C
C       Get two-norm of the vector. (Modulus)
C
        CALL GET_VEC_MOD(V_DIFF, MOD_DIFF)

        CALL GET_VEC_MOD(V_EXACT, MOD_EXACT)

C
C       Calc absolute error.
C
        O_RESULT(1)= ABS(MOD_DIFF)

C
C       Calc relative error.
C
        IF(MOD_EXACT .NE . 0) THEN
            O_RESULT(2)= ABS(MOD_DIFF/MOD_EXACT)
        ELSE
            O_RESULT(2)=-1.0D0
        END IF


        RETURN
        END SUBROUTINE
