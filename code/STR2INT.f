CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE STR2INT(STR_T, INT_T, V_DIR)
        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER V_DIR, INT_T
        CHARACTER*100 STR_T

        IF(V_DIR .EQ. 1) THEN
            READ(STR_T,*) INT_T  !Convert to Int
        ELSE
            WRITE(STR_T , *) INT_T !Convert to String
        END IF

        RETURN
        END
