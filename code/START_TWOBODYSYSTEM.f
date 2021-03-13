CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE START_TWOBODYSYSTEM(VBODYI)
        IMPLICIT NONE

        INCLUDE 'common.dat'


        INTEGER VBODYI
        LOGICAL BFOUND

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       SUMMARY:
C
C       This is the routine that must be called in case you are working
C       with a two body system.
C
C       VBODYI                  INTEGER INPUT. It is the SpiceLib ID of
C                               the body that is to be used as a primary
C                               body in a two body system. For example,
C                               for integration of a satellite around
C                               Earth, you must pass in 399.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INTEGER I


        BFOUND = .FALSE.


        DO I=1,MMAX


            IF (LIST_I(I,1) .EQ. VBODYI) THEN


                IPB = VBODYI            !Initiate common variable IPB.
                BFOUND=.TRUE.
                SYS_TYPE=2


            END IF
        END DO

        IF(BFOUND .EQV. .FALSE.) THEN
            WRITE (*,*) 'Error in START_TWOBODYSYSTEM:'
            WRITE (*,*) 'Two body system initialisation error!'
            WRITE (*,*) 'Primary body supplied is not in the model.'
            WRITE (*,*) 'Killing program.'

            STOP
         END IF


        RETURN
        END




