CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        SUBROUTINE RK89(V_T, VDIM, VSTATE, VDT, V_EXTERNAL, OSTATE)
        IMPLICIT NONE

        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 V_T, VDT, VSTATE(VDIM), OSTATE(VDIM)
        EXTERNAL V_EXTERNAL
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INTEGER I,J,K,L
        INTEGER COUNTER, COUNTER_MAX

        REAL*8 ALPHAK(12), GAMMA_KLAM(12, 11), C_K(12), CP_K(11)
        REAL*8 F_K(12,3), T_K(12)
        REAL*8 TEMP_X(VDIM), TEMP_T, TEMP_FK(6)

        REAL*8 TEMP_XBAR(VDIM), TEMP_XP(3)
        REAL*8 TEX_VAL(3), ERR_X(2)

        REAL*8 DELTAH, ERR_TOL
        REAL*8 ERR_X_TOL, TEMP_MOD_XO, ERR_PARAM, TEX_MOD
C
C       Define coefficients.
C

        DELTAH=VDT !Will be read from a config file later.

        CALL GET_VEC_MOD(VSTATE, TEMP_MOD_XO)

        ERR_X_TOL=1.0D-16
        ERR_TOL=(0.50D0)**9.0D0

        COUNTER_MAX=50
        VDIM=6 !Override VDIM.
C
C       Alpha_K. The vector has a bias of 1.
C

        ALPHAK(1)=0.D0
        ALPHAK(2)=7.0D0/80.0D0
        ALPHAK(3)=7.0D0/40.0D0
        ALPHAK(4)=5.0D0/12.0D0
        ALPHAK(5)=1.0D0/2.0D0
        ALPHAK(6)=1.0D0/6.0D0
        ALPHAK(7)=1.0D0/3.0D0
        ALPHAK(8)=2.0D0/3.0D0
        ALPHAK(9)=5.0D0/6.0D0
        ALPHAK(10)=1.0D0/12.0D0
        ALPHAK(11)=1.0D0
        ALPHAK(12)=1.0D0


C
C       C_K. The vector has a bias of 1.
C

        C_K(1)=121.0D0/4200.0D0
        C_K(2)=0.0D0
        C_K(3)=0.0D0
        C_K(4)=0.0D0
        C_K(5)=43.0D0/525.0D0
        C_K(6)=33.0D0/350.0D0
        C_K(7)=17.0D0/140.0D0
        C_K(8)=3.0D0/56.0D0
        C_K(9)=31.0D0/1050.0D0
        C_K(10)=512.0D0/5775.0D0
        C_K(11)=1.0D0/550.0D0
        C_K(12)=0.0D0


C
C       CP_K. The vector has a bias of 1.
C

        CP_K(1)=41.0D0/840.0D0
        CP_K(2)=0.0D0
        CP_K(3)=0.0D0
        CP_K(4)=0.0D0
        CP_K(5)=34.0D0/105.0D0
        CP_K(6)=9.0D0/35.0D0
        CP_K(7)=9.0D0/280.0D0
        CP_K(8)=9.0D0/280.0D0
        CP_K(9)=9.0D0/35.0D0
        CP_K(10)=0.0D0
        CP_K(11)=41.0D0/840.0D0

C
C       GAMMA_K-LAMDA. The vector has a bias of 1.
C

        GAMMA_KLAM(1,1)=0.0D0
        GAMMA_KLAM(2,1)=49.0D0/12800.0D0
        GAMMA_KLAM(3,1)=49.0D0/9600.0D0
        GAMMA_KLAM(4,1)=16825.0D0/381024.0D0
        GAMMA_KLAM(5,1)=23.0D0/840.0D0
        GAMMA_KLAM(6,1)=533.0D0/68040.0D0
        GAMMA_KLAM(7,1)=-4469.0D0/85050.0D0
        GAMMA_KLAM(8,1)=694.0D0/10125.0D0
        GAMMA_KLAM(9,1)=30203.0D0/691200.0D0
        GAMMA_KLAM(10,1)=1040381917.0D0/14863564800.0D0
        GAMMA_KLAM(11,1)=-33213637.0D0/179088000.0D0
        GAMMA_KLAM(12,1)=121.0D0/4200.0D0


        GAMMA_KLAM(1,2)=0.0D0
        GAMMA_KLAM(2,2)=0.0D0
        GAMMA_KLAM(3,2)=49.0D0/4800.0D0
        GAMMA_KLAM(4,2)=-625.0D0/11907.0D0
        GAMMA_KLAM(5,2)=0.0D0
        GAMMA_KLAM(6,2)=0.0D0
        GAMMA_KLAM(7,2)=0.0D0
        GAMMA_KLAM(8,2)=0.0D0
        GAMMA_KLAM(9,2)=0.0D0
        GAMMA_KLAM(10,2)=0.0D0
        GAMMA_KLAM(11,2)=0.0D0
        GAMMA_KLAM(12,2)=0.0D0

        GAMMA_KLAM(1,3)=0.0D0
        GAMMA_KLAM(2,3)=0.0D0
        GAMMA_KLAM(3,3)=0.0D0
        GAMMA_KLAM(4,3)=18125.0D0/190512.0D0
        GAMMA_KLAM(5,3)=50.0D0/609.0D0
        GAMMA_KLAM(6,3)=5050.0D0/641277.0D0
        GAMMA_KLAM(7,3)=-2384000/641277.0D0
        GAMMA_KLAM(8,3)=0.0D0
        GAMMA_KLAM(9,3)=0.0D0
        GAMMA_KLAM(10,3)=548042275.0D0/109444608.0D0
        GAMMA_KLAM(11,3)=604400.0D0/324597.0D0
        GAMMA_KLAM(12,3)=0.0D0


        GAMMA_KLAM(1,4)=0.0D0
        GAMMA_KLAM(2,4)=0.0D0
        GAMMA_KLAM(3,4)=0.0D0
        GAMMA_KLAM(4,4)=0.0D0
        GAMMA_KLAM(5,4)=9.0D0/580.0D0
        GAMMA_KLAM(6,4)=-19.0D0/5220.0D0
        GAMMA_KLAM(7,4)=3896.0D0/19575.0D0
        GAMMA_KLAM(8,4)=-5504.0D0/10125.0D0
        GAMMA_KLAM(9,4)=0.0D0
        GAMMA_KLAM(10,4)=242737.0D0/5345280.0D0
        GAMMA_KLAM(11,4)=63826.0D0/445875.0D0
        GAMMA_KLAM(12,4)=0.0D0

        GAMMA_KLAM(1,5)=0.0D0
        GAMMA_KLAM(2,5)=0.0D0
        GAMMA_KLAM(3,5)=0.0D0
        GAMMA_KLAM(4,5)=0.0D0
        GAMMA_KLAM(5,5)=0.0D0
        GAMMA_KLAM(6,5)=23.0D0/12636.0D0
        GAMMA_KLAM(7,5)=-1451.0D0/15795.0D0
        GAMMA_KLAM(8,5)=-424.0D0/2025.0D0
        GAMMA_KLAM(9,5)=9797.0D0/172800.0D0
        GAMMA_KLAM(10,5)=569927617.0D0/6900940800.0D0
        GAMMA_KLAM(11,5)=0.0D0
        GAMMA_KLAM(12,5)=43.0D0/525.0D0

        GAMMA_KLAM(1,6)=0.0D0
        GAMMA_KLAM(2,6)=0.0D0
        GAMMA_KLAM(3,6)=0.0D0
        GAMMA_KLAM(4,6)=0.0D0
        GAMMA_KLAM(5,6)=0.0D0
        GAMMA_KLAM(6,6)=0.0D0
        GAMMA_KLAM(7,6)=502.0D0/135.0D0
        GAMMA_KLAM(8,6)=-104.0D0/2025.0D0
        GAMMA_KLAM(9,6)=79391.0D0/518400.0D0
        GAMMA_KLAM(10,6)=-2559686731.0D0/530841600.0D0
        GAMMA_KLAM(11,6)=-6399863.0D0/2558400.0D0
        GAMMA_KLAM(12,6)=33.0D0/350.0D0


        GAMMA_KLAM(1,7)=0.0D0
        GAMMA_KLAM(2,7)=0.0D0
        GAMMA_KLAM(3,7)=0.0D0
        GAMMA_KLAM(4,7)=0.0D0
        GAMMA_KLAM(5,7)=0.0D0
        GAMMA_KLAM(6,7)=0.0D0
        GAMMA_KLAM(7,7)=0.0D0
        GAMMA_KLAM(8,7)=364.0D0/675.0D0
        GAMMA_KLAM(9,7)=20609.0D0/345600.0D0
        GAMMA_KLAM(10,7)=-127250389.0D0/353894400.0D0
        GAMMA_KLAM(11,7)=110723.0D0/511680.0D0
        GAMMA_KLAM(12,7)=17.0D0/140.0D0

        GAMMA_KLAM(1,8)=0.0D0
        GAMMA_KLAM(2,8)=0.0D0
        GAMMA_KLAM(3,8)=0.0D0
        GAMMA_KLAM(4,8)=0.0D0
        GAMMA_KLAM(5,8)=0.0D0
        GAMMA_KLAM(6,8)=0.0D0
        GAMMA_KLAM(7,8)=0.0D0
        GAMMA_KLAM(8,8)=0.0D0
        GAMMA_KLAM(9,8)=70609.0D0/2073600.0D0
        GAMMA_KLAM(10,8)=-53056229.0D0/2123366400.0D0
        GAMMA_KLAM(11,8)=559511.0D0/359817600.0D0
        GAMMA_KLAM(12,8)=3.0D0/56.0D0


        GAMMA_KLAM(1,9)=0.0D0
        GAMMA_KLAM(2,9)=0.0D0
        GAMMA_KLAM(3,9)=0.0D0
        GAMMA_KLAM(4,9)=0.0D0
        GAMMA_KLAM(5,9)=0.0D0
        GAMMA_KLAM(6,9)=0.0D0
        GAMMA_KLAM(7,9)=0.0D0
        GAMMA_KLAM(8,9)=0.0D0
        GAMMA_KLAM(9,9)=0.0D0
        GAMMA_KLAM(10,9)=23.0D0/5120.0D0
        GAMMA_KLAM(11,9)=372449.0D0/7675200.0D0
        GAMMA_KLAM(12,9)=31.0D0/1050.0D0

        GAMMA_KLAM(1,10)=0.0D0
        GAMMA_KLAM(2,10)=0.0D0
        GAMMA_KLAM(3,10)=0.0D0
        GAMMA_KLAM(4,10)=0.0D0
        GAMMA_KLAM(5,10)=0.0D0
        GAMMA_KLAM(6,10)=0.0D0
        GAMMA_KLAM(7,10)=0.0D0
        GAMMA_KLAM(8,10)=0.0D0
        GAMMA_KLAM(9,10)=0.0D0
        GAMMA_KLAM(10,10)=0.0D0
        GAMMA_KLAM(11,10)=756604.0D0/839475.0D0
        GAMMA_KLAM(12,10)=512.0D0/5775.0D0

        GAMMA_KLAM(1,11)=0.0D0
        GAMMA_KLAM(2,11)=0.0D0
        GAMMA_KLAM(3,11)=0.0D0
        GAMMA_KLAM(4,11)=0.0D0
        GAMMA_KLAM(5,11)=0.0D0
        GAMMA_KLAM(6,11)=0.0D0
        GAMMA_KLAM(7,11)=0.0D0
        GAMMA_KLAM(8,11)=0.0D0
        GAMMA_KLAM(9,11)=0.0D0
        GAMMA_KLAM(10,11)=0.0D0
        GAMMA_KLAM(11,11)=0.0D0
        GAMMA_KLAM(12,11)=1.0D0/550.0D0

C
C       Calc f0 and save it in the vector F_K.
C
        COUNTER=0

10      COUNTER=COUNTER+1

        DO I=1,12
            DO J=1,3
                F_K(I,J)=0.0D0
            END DO
            T_K(I)=0.0D0
        END DO

        T_K(1)=V_T


        CALL V_EXTERNAL(V_T, VSTATE, VDIM, TEMP_X)


        DO J=1,3
            F_K(1,J)=TEMP_X(J+3)
        END DO

        DO J=1,3
            TEMP_X(J)=0.0D0
        END DO

        DO K=2,12
            T_K(K)=T_K(1)+(ALPHAK(K)*DELTAH)
            DO J=1,3
                TEMP_X(J)=0.0D0
            END DO
            DO L=1,K-1
                DO J=1,3
                TEMP_X(J)=TEMP_X(J)+F_K(L,J)*GAMMA_KLAM(K,L)
                END DO
            END DO
            DO J=1,3
                TEMP_X(J)=TEMP_X(J)*DELTAH*DELTAH
                TEMP_X(J)=(VSTATE(J+3)*ALPHAK(K)*DELTAH)+TEMP_X(J)
                TEMP_X(J)=TEMP_X(J)+VSTATE(J)
            END DO

            TEMP_T=T_K(K)
            DO J=4,6
                TEMP_X(J)=0.0D0
            END DO

            CALL V_EXTERNAL(TEMP_T,TEMP_X, VDIM, TEMP_FK)

            DO J=1,3
                F_K(K,J)=TEMP_FK(J+3)
            END DO


        END DO

!        DO I=1,12
!            DO J=1,3
!                WRITE(*,*) I,J, F_K(I,J)
!            END DO
!        END DO

C
C       Move on if max no. iterations reached.
C
        IF(COUNTER .GT. COUNTER_MAX) THEN
            WRITE(*,*) 'MAX ITERATIONS REACHED. f_x. ET=', V_T
            WRITE (*,*) ERR_PARAM, TEX_MOD
            GOTO 30
        END IF
C
C       Calculate the truncation error T_Ex
C
        DO J=1,3
            TEX_VAL(J)=(F_K(11,J)-F_K(12,J))*(DELTAH**7.0D0)/550.0D0
        END DO

        TEX_MOD=0.0D0
        CALL GET_VEC_MOD(TEX_VAL, TEX_MOD)
!        ERR_PARAM=ABS(TEX_MOD*ERR_X_TOL)
!
!        IF( ERR_PARAM .LT. ERR_TOL) THEN
!            DELTAH=DELTAH*2.0D0
!            GOTO 10
!        END IF
!
!        IF( ERR_PARAM .GT. 1.0D0) THEN
!            DELTAH=DELTAH*0.50D0
!            GOTO 10
!        END IF

        IF(TEX_MOD .GT. ERR_X_TOL) THEN
            DELTAH=DELTAH*0.50D0
            GOTO 10
        END IF



C
C       Now to calc x, x_^, and x_p
C
30      COUNTER=0
20      COUNTER=COUNTER+1
        DO J=1,3
                TEMP_X(J)=0.0D0
                TEMP_XBAR(J)=0.0D0
        END DO

        DO J=1,3

            TEMP_X(J)=0.0D0

            DO K=1,11
                TEMP_X(J)=TEMP_X(J)+C_K(K)*F_K(K,J)
            END DO
            TEMP_X(J)=TEMP_X(J)*DELTAH*DELTAH
            TEMP_X(J)=TEMP_X(J)+VSTATE(J)+(VSTATE(J+3)*DELTAH)
            !TEMP_X(J)=TEMP_X(J)+0.0D0*(DELTAH**9.0D0)
        END DO

!        DO J=1,3
!
!            TEMP_XBAR(J)=0.0D0
!
!            DO K=1,12
!            IF (K.LT. 10) THEN
!                TEMP_XBAR(J)=TEMP_XBAR(J)+C_K(K)*F_K(K,J)
!            ELSE IF (K .EQ. 12) THEN
!                TEMP_XBAR(J)=TEMP_XBAR(J)+C_K(K-1)*F_K(K,J)
!            END IF
!            END DO
!            TEMP_XBAR(J)=TEMP_XBAR(J)*DELTAH*DELTAH
!            TEMP_XBAR(J)=TEMP_XBAR(J)+VSTATE(J)+(VSTATE(J+3)*DELTAH)
!            !TEMP_XBAR(J)=TEMP_XBAR(J)+0.0D0*(DELTAH**10.0D0)
!        END DO

C
C       Calc x_p.
C
        DO I=1,3
            TEMP_XP(J)=0.0D0
        END DO

        DO J=1,3

            TEMP_XP(J)=0.0D0

            DO K=1,11
                TEMP_XP(J)=TEMP_XP(J)+CP_K(K)*F_K(K,J)
            END DO
            TEMP_XP(J)=TEMP_XP(J)*DELTAH
            TEMP_XP(J)=TEMP_XP(J)+VSTATE(J+3)
        END DO

C
C       Move on if max no. iterations reached.
C

!        IF (COUNTER .GT. COUNTER_MAX) THEN
!            WRITE(*,*) 'MAX ITERATIONS REACHED. X_p. ET=', V_T
!            !PAUSE
!            GOTO 40
!        END IF
!
!        CALL CONVERGENCE_TEST_VECTOR(TEMP_XBAR, TEMP_X, ERR_X)
!
!        ERR_PARAM=ERR_TOL*1.0D-03
!
!        IF(ERR_X(2) .GT. ERR_PARAM) THEN
!            DELTAH=DELTAH*0.500D0
!            GOTO 20
!        END IF


40      DO J=1,3
            OSTATE(J)=TEMP_X(J)
            OSTATE(J+3)=TEMP_XP(J)
        END DO


        VDT=DELTAH

        RETURN
        END SUBROUTINE
