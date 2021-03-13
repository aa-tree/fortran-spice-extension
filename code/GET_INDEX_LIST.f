CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GET_INDEX_LIST(ID, R_INDEX)
        IMPLICIT NONE

        INCLUDE 'common.dat'


        INTEGER ID, R_INDEX, I

!       SUMMARY:
!
!       This routine gets the array index of a particular body from the
!       variable LIST_I (defined in common.dat).
!
!       VARIABLES:
!       NAME            TYPE              DESCRIPTION
!
!       V_ID           INPUT(INTEGER)     ID of the body to search.
!
!       R_INDEX        OUTPUT(INTEGER)    The index of the requested
!                                         body in the common variable
!                                         LIST_I.
!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        R_INDEX=0

        DO I=1,MMAX
            IF(LIST_I(I,1) .EQ. ID) THEN
                R_INDEX=I
            END IF
        END DO



        RETURN
        END SUBROUTINE
