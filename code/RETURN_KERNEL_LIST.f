CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE RETURN_KERNEL_LIST(V_LISTEPH, O_LIST, O_LENGTH)
        IMPLICIT NONE

        INCLUDE 'common.dat'


        INTEGER O_LENGTH

        CHARACTER*64 V_LISTEPH(MMAX+1), O_LIST(MMAX+1)


        INTEGER I, J, TEMP_PNT
        LOGICAL REP_UNIQUE

C
C       Populate output list with zeroes.
C

        DO I=1,MMAX+1
            O_LIST(I)= '0'
        END DO

        TEMP_PNT=2


        O_LIST(1)=V_LISTEPH(1)
        REP_UNIQUE= .TRUE.

        DO I=2,MMAX+1

            DO J=1,MMAX+1

                IF(O_LIST(J) .EQ. V_LISTEPH(I)) THEN

                REP_UNIQUE= .FALSE.

                END IF
            END DO

            IF(REP_UNIQUE) THEN

                O_LIST(TEMP_PNT)=V_LISTEPH(I)
                TEMP_PNT = TEMP_PNT + 1
                REP_UNIQUE= .TRUE.

            END IF

            REP_UNIQUE=.TRUE.
        END DO


        O_LENGTH=TEMP_PNT-1



        RETURN
        END



