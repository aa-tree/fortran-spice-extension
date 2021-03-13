CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        SUBROUTINE GET_MU(V_ID, O_MU)

!       SUMMARY:
!
!       This routine gets the value of Mu (Gravitational Parameter (G*M)
!       for a body from the ephemeris. Returns 0 if the body is not in
!       the loaded ephemeris.
!
        IMPLICIT NONE

        INCLUDE 'common.dat'
        REAL*8 O_MU
        INTEGER V_ID

!       VARIABLES:
!
!       NAME            TYPE              DESCRIPTION
C
C
C       V_ID        INPUT [INTEGER]      ID of the body to search.
C
C       O_MU        OUTPUT [REAL*8]      The value of mu (GM) for the
C                                        requested body.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


C
C       Variables for reading GM(mu) from the ephemerides.
C

        CHARACTER*100 BODY_TOSEARCH_T, BODY_TOSEARCH
        INTEGER DIM_O(1)
        REAL*8 VALUES(1)

C
C       Variables for second pass.
C

        INTEGER MU_INTEGER(MU_LENGTH_2)
        REAL*8 MU_VALUES_FILE(MU_LENGTH_2)

        INTEGER FUNIT, I
        CHARACTER*200 FILENAME

        LOGICAL BODFND

C
C       Assign Mu as 0.
C
        O_MU=0.00D0

        IF( BODFND (V_ID, 'GM') ) THEN

          CALL STR2INT(BODY_TOSEARCH_T, V_ID, -1)

          BODY_TOSEARCH=TRIM(BODY_TOSEARCH_T)

          CALL BODVRD(BODY_TOSEARCH, 'GM', 1, DIM_O, VALUES)

          O_MU=VALUES(1)


        ELSE

          CALL GETLUN(FUNIT) !Routine from Toolkit.
          FILENAME ='../../gkernels/gm_remaining.csv'
          OPEN (UNIT = FUNIT, FILE = FILENAME, STATUS="OLD")

          DO I=1, MU_LENGTH_2

              MU_INTEGER(I)=0
              MU_VALUES_FILE(I)=0.00D0

          END DO


          DO I= 1,MU_LENGTH_2
              READ(FUNIT, *) MU_INTEGER(I), MU_VALUES_FILE(I)
          END DO

          DO I=1,MU_LENGTH_2


            IF(MU_INTEGER(I) .EQ. V_ID) THEN
              O_MU=MU_VALUES_FILE(I)
            END IF

          END DO

          CLOSE(FUNIT)

        END IF

        RETURN
        END SUBROUTINE GET_MU
