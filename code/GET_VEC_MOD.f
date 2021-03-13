CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GET_VEC_MOD(I_VEC, O_MOD)
        IMPLICIT NONE

        INCLUDE 'common.dat'

        REAL*8 I_VEC(6)
        REAL*8 O_MOD

!
!       SUMMARY:
!
!       Returns the mod of a vector.
!
!       VARIABLES:
!
!       NAME        TYPE            DESCRIPTION
!
!       I_VEC(6)    INPUT(REAL*8)   The vector for which mod is to be
!                                   calculated.
!
!       O_MOD       OUTPUT(REAL*8)  Modulus of the vector.
!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        O_MOD=0.0D0

        O_MOD=SQRT(I_VEC(1)**2.0D0+ I_VEC(2)**2.0D0+ I_VEC(3)**2.0D0)

        RETURN
        END
