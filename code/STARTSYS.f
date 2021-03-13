CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE STARTSYS(V_IBC)
        IMPLICIT NONE

        INCLUDE 'common.dat'


        INTEGER V_IBC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SUMMARY:
C
C     This is the first routine that must be called. It initialises
C     common variables LIST_ID_REF and LIST_EPH. It also loads the
C     kernels into the memory.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        CHARACTER (LEN=100) :: FILENAME
        CHARACTER*64, DIMENSION(IMAX,4) :: TEMP_ALLBODIES
        CHARACTER*64 :: KERNEL_TEMP

        CHARACTER*64 T_EPH_PATH(MMAX+1)
        CHARACTER*64 T_LOADED_KERNEL(MMAX+1)

        LOGICAL REP_FOUND



        INTEGER I, J
        INTEGER O_LENGTH

        INTEGER TEMP, TEMP_2
        INTEGER FUNIT, FUNIT_2


C
C       Initialise some common variables to default values.
C
        IBC=V_IBC

        SYS_TYPE=0
        PERTURBATION=.FALSE.
C
C       Read the model IDs into the memory from 'model.dat'.
C       Get ephemerides file location, Reference Frame from
C       '/eph/list_of_bodies.csv'.
C



        CALL GETLUN(FUNIT) !Routine from Toolkit.
        OPEN (UNIT = FUNIT, FILE = 'model.dat', STATUS="OLD")
        CALL GETLUN(FUNIT_2)
        FILENAME ='../../eph/list_of_bodies.csv'
        OPEN (UNIT = FUNIT_2, FILE = FILENAME , STATUS="OLD")

        DO I= 1,MMAX
            READ(FUNIT, *) LIST_I(I,1)
        END DO

        DO I= 1,IMAX
            READ(FUNIT_2, *) TEMP_ALLBODIES(I,1) , TEMP_ALLBODIES(I,2)
     &   , TEMP_ALLBODIES(I,3), TEMP_ALLBODIES(I,4)
        END DO

        DO I=1,MMAX
            DO J=1,IMAX
                CALL STR2INT(TEMP_ALLBODIES(J,1), TEMP, 1)

                IF ( TEMP .EQ. LIST_I(I,1)) THEN

                    CALL STR2INT(TEMP_ALLBODIES(J,4), TEMP_2, 1)

C                    WRITE(*,*) TEMP_2
                    LIST_I(I,2) = TEMP_2
                    LIST_EPH(I) = TEMP_ALLBODIES(J,3)

                    T_EPH_PATH(I) = TEMP_ALLBODIES(J,3)

                END IF
            END DO
        END DO

        DO I=1,IMAX

            CALL STR2INT(TEMP_ALLBODIES(I,1), TEMP, 1)
            IF(TEMP .EQ. IBC) THEN


                LIST_EPH_IBC(1)=TEMP_ALLBODIES(I,3)
                T_EPH_PATH(MMAX+1)=TEMP_ALLBODIES(I,3)
            END IF
        END DO





C         DO I=1,MMAX
C            WRITE(*, *) LIST_I(I,1), LIST_I(I,2), LIST_EPH(I)
C         END DO


        CLOSE(FUNIT)
        CLOSE(FUNIT_2)


C
C      Load the required kernels into the memory.
C

        CALL RETURN_KERNEL_LIST(T_EPH_PATH,  T_LOADED_KERNEL,
     &O_LENGTH)

C       WRITE (*,*) "O_LENGTH", O_LENGTH

C        DO I=1,MMAX
C            WRITE (*,*) I, T_LOADED_KERNEL(I)
C        END DO


C
C       Load the kernel from the first of the list.
C

        DO I=1,O_LENGTH
            KERNEL_TEMP='../../eph/'//T_LOADED_KERNEL(I)
            CALL FURNSH(KERNEL_TEMP) !Routine from Toolkit.
        END DO



C
C       Leap second kernel for Time Conversion. Different Kernels
C       required for PC and Linux.
C

        IF(ISPC) THEN
                KERNEL_TEMP='../../gkernels/naif0012.tls.pc'
            ELSE
                KERNEL_TEMP='../../gkernels/naif0012.tls'

        END IF

        CALL FURNSH(KERNEL_TEMP) !Load Time Kernel.

C
C       Load kernels containing GM data for planets, satellites,
C       and barycentres.
C

        KERNEL_TEMP='../../gkernels/gm_de431.tpc'
        CALL FURNSH(KERNEL_TEMP) !Contains GM of

        RETURN
        END


