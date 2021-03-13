CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Created by: SPICE_UI
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        
        PROGRAM PROGRAM_TEST_1942760373
        IMPLICIT NONE

        INCLUDE '../../code/common.dat'

        REAL*8  ET, O_POS(6)
        INTEGER I_ID, I

        CALL STARTSYS(10)

        CALL STR2ET ( '01/01/2020 ', ET)

        CALL GET_POS_WRT_IBC(ET, 1, O_POS)


        DO I=1,6
            WRITE(*,*) O_POS(I)
        END DO

        RETURN
        END PROGRAM
