CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE TRANS_ORIGIN_FROMSSB(ET, TOFRAME, VIN, VOUT)




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IMPLICIT NONE
        INCLUDE 'common.dat'

        REAL*8 VIN(6), VOUT(6), ET
        INTEGER FROMF, TOFRAME

        REAL*8 R0(6), RIBC(6)
        REAL*8 LTIME
        INTEGER I

C
C       Intitialising output vector with zereos.
C
        DO I=1,6
          VOUT(I)=0.00D0
        END DO

C
C       Getting coordinates of the frame, with respect to Solar System
C       Barycentre.
C
        CALL SPKEZ(TOFRAME, ET, 'J2000', 'NONE', 0,
     &RIBC, LTIME)

        DO I=1,6
          VOUT(I)=VIN(I)-RIBC(I)
        END DO


        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 
