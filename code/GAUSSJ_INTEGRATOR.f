CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GAUSSJ_INTEGRATOR(VTIME, VSTATE, VDIM, DELTAH,
     & V_TFINAL, STARTUP_ROUTINE, VECTOR_ROUTINE, OSTATE)

        IMPLICIT NONE
        INCLUDE 'common.dat'

        INTEGER VDIM
        REAL*8 VTIME, V_TFINAL, DELTAH
        REAL*8 VSTATE(6), OSTATE(6)

        EXTERNAL STARTUP_ROUTINE, VECTOR_ROUTINE


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        REAL*8  ERROR_VAL(2), MAX_ERROR(2)
        REAL*8 RK_T_VS(6), RK_T_DELTAH !Temp variables for startup.
        REAL*8 RK_VTIME  !Temp variables for startup.

        REAL*8 R_M4_4(6, 9), R_M4_4_TEMP(6)
        REAL*8 R_M4_4_P(6, 9), R_M4_4_P_TEMP(6)

C
C       CORRECTOR VARIABLES
C
        REAL*8 NEW_R4(6,9), NEW_R4_P(6,9)

        REAL*8 CORR_R_M4(6)

        INTEGER I,J,K
        INTEGER COUNTER, COUNTER_RP, MAX_COUNTER
C
C       Output Variables
C
        INTEGER FREE_FP1, FREE_FP2
        REAL*8 X_EPH(6), LET, X_EPH_DIFF(6), X_EPH_DIFF_ERR(2)


        REAL*8 CURRENT_TIME, TEMP_T, TEMP_DET
        REAL*8 R_N_P(3,9), R_N(3,9)


        REAL*8 T_S_N(3)

C
C       Convergence Variables
C
        REAL*8 T_VEXACT_CONV(6), T_VAPPROX_COV(6), T_DIFFERENCE(9)
        REAL*8 T_ERR_CONV(2), ERROR_VALUE, R_O_MOD

        REAL*8 O_R_NP1(6)
C
C       Override VDIM's value.
C

        VDIM=6

C
C       Initialise some integrator/function constants variables
C
        ERROR_VAL(1)=1.0D0 !Absolute error limit.
        ERROR_VAL(2)=1.0D-16 !Relative error limit.
        MAX_ERROR(1)=10.0D05
        MAX_ERROR(2)=1.0D00
        MAX_COUNTER=100
        COUNTER=0
        COUNTER_RP=0

C
C       Set common variables to zero.
C
        DO I=1,3
            GAUSS_LAST_S_N(I)=0.0D0
            GAUSS_LAST_CAPS_N(I)=0.0D0
        END DO
C
C       Open File to write.
C

        CALL GETLUN(FREE_FP1)
        CALL GETLUN(FREE_FP2)

        OPEN(FREE_FP1, FILE='out_gauss.dat')
        OPEN(FREE_FP2, FILE='out_EPH.dat')

        !Write Initial value to file.
        WRITE(FREE_FP1,*) VTIME, (VSTATE(I),I=1,6)
        WRITE(FREE_FP2,*) VTIME, (VSTATE(I),I=1,6)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       STARTUP STEPS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        CURRENT_TIME=VTIME
C
C       Initilse r and r' vector with zeroes.
C

        DO J=1,9
            DO I=1,6
                R_M4_4(I,J)=0.0D0
                R_M4_4_P(I,J)=0.0D0
            END DO
        END DO

C
C       Get the eight required startup values r4, r3...r1, and r-4..r-1
C       The vector R_M4_4 has a bias of 5. So r-3 would be in the 2 col.
C
        DO J=1,6 !Move r_0 into the common vector.

            R_M4_4(J,5)=VSTATE(J)

        END DO


C
C       Now calculating r_-n..r_n, and r'_-n..r'_n
C

        RK_VTIME=CURRENT_TIME
        RK_T_DELTAH= -1.0D0*DELTAH


        DO I=4,1,-1 !Calc r_-1 to r_-4

            DO J=1,6
                RK_T_VS(J)=R_M4_4(J,I+1)
            END DO

            CALL STARTUP_ROUTINE(RK_VTIME, VDIM, RK_T_VS, RK_T_DELTAH,
     & VECTOR_ROUTINE, R_M4_4_TEMP)

            DO J=1,6
                R_M4_4(J,I)=R_M4_4_TEMP(J)
            END DO

            RK_VTIME=RK_VTIME-DELTAH

        END DO



        RK_VTIME=CURRENT_TIME

        DO I=6,9 !r1 to r4

            DO J=1,6
                RK_T_VS(J)=R_M4_4(J,I-1)
            END DO

            CALL STARTUP_ROUTINE(RK_VTIME, VDIM, RK_T_VS, DELTAH,
     & VECTOR_ROUTINE, R_M4_4_TEMP)

            DO J=1,6
                R_M4_4(J, I)=R_M4_4_TEMP(J)
            END DO

            RK_VTIME=RK_VTIME+DELTAH

        END DO



C
C       Get the accelerations at r-4, r-3....r4
C

5       TEMP_T=CURRENT_TIME-(4.0D0*DELTAH)

        DO I=1,9

            DO J=1,6
                R_M4_4_TEMP(J)=R_M4_4(J,I)
            END DO

            CALL VECTOR_ROUTINE(TEMP_T, R_M4_4_TEMP, VDIM,
     &R_M4_4_P_TEMP)

            DO J=1,6
                R_M4_4_P(J,I)=R_M4_4_P_TEMP(J)
            END DO

            TEMP_T=TEMP_T+DELTAH

        END DO

C
C       Test convergence of accelerations.
C

10      COUNTER=COUNTER+1

        DO I=1,3
            T_VEXACT_CONV(I)=R_M4_4_P(I+3,5)
        END DO

        CALL GET_VEC_MOD(T_VEXACT_CONV,R_O_MOD)

        T_DIFFERENCE(5)=0.0D0

        DO J=6,9

            DO I=1,3
            T_VAPPROX_COV(I)=R_M4_4_P(I+3,J)-R_M4_4_P(I+3,J-1)
            END DO

            CALL GET_VEC_MOD(T_VAPPROX_COV,ERROR_VALUE)
            T_DIFFERENCE(J)=ABS(ERROR_VALUE/R_O_MOD)


        END DO

        DO J=1,4
            DO I=1,3

            T_VAPPROX_COV(I)=R_M4_4_P(I+3,J+1)-R_M4_4_P(I+3,J)
            END DO
            CALL GET_VEC_MOD(T_VAPPROX_COV,ERROR_VALUE)
            T_DIFFERENCE(J)=ABS(ERROR_VALUE/R_O_MOD)

        END DO


        DO I=1,9
            IF(T_DIFFERENCE(I) .GT. ERROR_VAL(2)
     & .AND. COUNTER .LT. MAX_COUNTER) THEN

                GOTO 20
            ELSE IF ( COUNTER .GT. MAX_COUNTER) THEN
                GOTO 30
            END IF


        END DO


C
C       If the acceleration have converged, move on.
C

        GOTO 30



C
C       Calc r_n & r_n', for n=-4..4, n != 0
C




20       CALL GAUSS_CALC_RN (VDIM, R_M4_4, R_M4_4_P,
     & DELTAH, R_N_P, R_N)

!        WRITE (*,*) '================='
!        DO I=1,3
!            WRITE(*,*) GAUSS_LAST_S_N(I), GAUSS_LAST_CAPS_N(I)
!        END DO
!        PAUSE
!
        DO J=1,9
            DO I=1,6
                R_M4_4=0.0D0
                R_M4_4_P=0.0D0
            END DO
        END DO

        DO J=1,9
            DO I=1,3
                R_M4_4(I,J)=R_N(I,J)
                R_M4_4(I+3,J)=R_N_P(I,J)
            END DO
        END DO

        DO J=1,9
            DO I=1,3
                R_M4_4_P(I,J)=R_N_P(I,J)
                R_M4_4_P(I+3,J)=0.0D0
            END DO
        END DO







        GOTO 5



30       TEMP_T=VTIME+4.0D0*DELTAH


40      TEMP_T=TEMP_T+DELTAH
C
C       Now calculate the Predicted value.
C
        CALL GAUSS_RANDRP_N1(VDIM, R_M4_4, R_M4_4_P,
     &DELTAH, O_R_NP1)

        CALL VECTOR_ROUTINE(TEMP_T, O_R_NP1, VDIM,
     &R_M4_4_P_TEMP)


C
C       Now use the corrector forumla to calc Corrected Value.
C

C
C       Set new R4 vector to zero.
C
        COUNTER=1
        COUNTER_RP=1




50      DO I=1,9
            DO J=1,6
                NEW_R4(I,J)=0.0D0
                NEW_R4_P(I,J)=0.0D0
            END DO
        END DO

        DO I=1,8
            DO J=1,6
                NEW_R4(J,I)=R_M4_4(J,I+1)
                NEW_R4_P(J,I)=R_M4_4_P(J,I+1)
            END DO
        END DO


        DO I=1,6
            NEW_R4(I,9)=O_R_NP1(I)
            NEW_R4_P(I,9)=R_M4_4_P_TEMP(I)
        END DO


        CALL GAUSS_CORR_RNANDP(VDIM, NEW_R4, NEW_R4_P,
     &DELTAH,CORR_R_M4)

C
C       Check convergence of r_n
C
        DO I=1,3
            T_VEXACT_CONV(I)=NEW_R4(I,9) !Predicted Value
            T_VAPPROX_COV(I)=CORR_R_M4(I) !Corrected Value
        END DO

        DO I=4,6
            T_VEXACT_CONV(I)=0.0D0
            T_VAPPROX_COV(I)=0.0D0
        END DO

        CALL CONVERGENCE_TEST_VECTOR(T_VAPPROX_COV, T_VEXACT_CONV,
     & T_ERR_CONV)

        IF(T_ERR_CONV(2) .GT. ERROR_VAL(2)) THEN

            IF( COUNTER .GT. MAX_COUNTER) THEN
                GOTO 60
            END IF
            IF(T_ERR_CONV(2) .GT. MAX_ERROR(2) .OR. T_ERR_CONV(1) .GT.
     &MAX_ERROR(1)) THEN
             WRITE(*,*) 'RELATIVE ERROR in: r_n', T_ERR_CONV(2),
     & 'ABSOLUTE ERROR in: r_n',T_ERR_CONV(2)
            END IF

            !If not converged, set the value of r_n'
            CALL VECTOR_ROUTINE(TEMP_T, CORR_R_M4, VDIM,
     &R_M4_4_P_TEMP)
            COUNTER=COUNTER+1
            GOTO 50

        END IF

C
C       Check the convergence of r_n'
C



        DO I=1,3
            T_VEXACT_CONV(I)=NEW_R4_P(I,9)
            T_VAPPROX_COV(I)=CORR_R_M4(I+3)
        END DO

        DO I=4,6
            T_VEXACT_CONV(I)=0.0D0
            T_VAPPROX_COV(I)=0.0D0
        END DO

        CALL CONVERGENCE_TEST_VECTOR(T_VEXACT_CONV, T_VAPPROX_COV,
     & T_ERR_CONV)


        IF(T_ERR_CONV(2) .GT. ERROR_VAL(2)) THEN

            IF( COUNTER_RP .GT. MAX_COUNTER) THEN
                GOTO 60
            END IF

            IF(T_ERR_CONV(2) .GT. MAX_ERROR(2) .OR. T_ERR_CONV(1) .GT.
     &MAX_ERROR(1)) THEN
             WRITE(*,*) 'RELATIVE ERROR in: r_n_p', T_ERR_CONV(2),
     & 'ABSOLUTE ERROR in: r_n_p',T_ERR_CONV(2)
             END IF

            !If not converged, set the value of r_n'
            CALL VECTOR_ROUTINE(TEMP_T, CORR_R_M4, VDIM,
     &R_M4_4_P_TEMP)
            COUNTER_RP=COUNTER_RP+1
            GOTO 50

        END IF

60      WRITE(FREE_FP1,*) TEMP_T, (CORR_R_M4(I),I=1,6)

C
C       For Comparison - TEMP
C
        CALL SPKEZ ( 301, TEMP_T, 'J2000', 'NONE',IBC, X_EPH, LET )
        X_EPH_DIFF_ERR(1)=0.0D0
        X_EPH_DIFF_ERR(2)=0.0D0
        CALL CONVERGENCE_TEST_VECTOR(CORR_R_M4, X_EPH, X_EPH_DIFF_ERR)

        WRITE(FREE_FP2,*) TEMP_T, (X_EPH(I),I=1,6),
     &X_EPH_DIFF_ERR(1), X_EPH_DIFF_ERR(2), COUNTER, COUNTER_RP



        WRITE (*,*) 'TEMP_T', TEMP_T, (V_TFINAL-TEMP_T)/86400.0D0
        IF (TEMP_T .LT. V_TFINAL) THEN
         CALL VECTOR_ROUTINE(TEMP_T, CORR_R_M4, VDIM,
     &R_M4_4_P_TEMP)
            DO K=1,8
                DO I=1,6
                    R_M4_4(I,K)=NEW_R4(I,K)
                    R_M4_4_P(I,K)=NEW_R4_P(I,K)
                END DO
            END DO

            DO I=1,6
                R_M4_4(I,9)=CORR_R_M4(I)
                R_M4_4_P(I,9)=R_M4_4_P_TEMP(I)
            END DO

            !Set value of s_n, S_n
            DO I=1,3
                T_S_N(I)=GAUSS_LAST_S_N(I)
            END DO

            DO I=1,3
                GAUSS_LAST_S_N(I)=T_S_N(I)+(R_M4_4_P(I+3,8)+
     &R_M4_4_P(I+3,9))/2.0D0
                GAUSS_LAST_CAPS_N(I)=GAUSS_LAST_CAPS_N(I)+T_S_N(I)+
     &(R_M4_4_P(I+3,8))/2.0D0
            END DO

!            DO I=1,3
!                WRITE(*,*) GAUSS_LAST_S_N(I), GAUSS_LAST_CAPS_N(I)
!            END DO
!           PAUSE




            GOTO 40
        END IF

        CLOSE(FREE_FP1)
        CLOSE(FREE_FP2)


        RETURN
        END

