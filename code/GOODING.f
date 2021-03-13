!
!        Source:
!        On universal elements, and conversion procedures to and from
!        position and velocity, Gooding, R. H, Celestial Mechanics,
!        Volume 44, Issue 3, pp.283-298

        SUBROUTINE ELS2PV (GM, AL, Q, OM, TAU, R, U, VR, VT)

!        ALGORITHM FOR TWO-DIMENSIONAL CONVERSION FROM ORBITAL ELEMENTS
!        TO POSITION AND VELOCITY. INPUT ARGUMENTS ARE: GM (G*M), AL(PHA)
!        (GM/A), Q (PERI DISTANCE), OM (EGA) (ARG-PERI RELATIVE TO
!        ASSUMED REFERENCE DIRECTION) AND TAU (TIME FROM PERI).
!
!        OUTPUT ARGUMENTS ARE: R (RADIAL DISTANCE),
!        U (ANGLE FROM REFERENCE DIRECTION), VR (RADIAL VELOCIY)
!        AND VT (TRANSVERSE VELOCITY: .GE.O).

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER (PI = 3.14159265358979323846264338328D0,FOURPI =
     &4D0*PI)

        IF (AL.EQ.OD0) THEN
!        (PARABOLA - GM CANNOT BE ZERO)
            D = DCBSOL(0.5D0/GM, Q, 1.5D0*GM*TAU)
            R = Q + 0.5D0*D*D/GM
            H = DSQRT (2D0*GM*Q)
            V = 2D0*DATAN2(D,H)

        ELSE
!        (ELLIPSE OR HYPERBOLA)
            E1 = AL*Q
            E = GM - E1
            EP1 = GM + E
            H = DSQRT (QEP1)
            ALP = DABS (AL)
            RTAL = DSQRT (ALP)

!        (LAST 6 ITEMS COULD BE SAVED IF REPEATING GM, AL & Q)

            EM = TAU*ALP*RTAL

            IF (AL.GT. OD0) THEN

!        (ELLIPSE - GM CANNOT BE ZERO)

            EE2 = 0.5D0*EKEPL(EM/GM, E1/GM)
            S2 = DSIN (EE2)
            C2 = DCOS (EE2)
            R = Q + 2D0*E*S2*S2/AL
            D = 2D0*E*S2*C2/RTAL
            V=2D0*DATAN2(EP1 *S2, H*RTAL*C2)
            EMV = EM/GM - V
            V = V + FOURPI*DSIGN (DINT (DABS (EMV/FOURPI) + 0.5D0), EMV)

            ELSE
!        (HYPERBOLA)

            S = SHKEPL(EM/E, -E1/E)
            S2 = S*S
            C = DSQRT (1 D0 + S2)
            S2 = S2/(C + 1D0)
            R = Q - E*S2/AL
            D = E*S/RTAL
            V = DATAN2 (S*H*RTAL, -GM*S2 - E1)
            END IF
        END IF

!        (ALL ORBITS)

        U = OM + V
        VR = D/R
        VT = H/R
        RETURN
        END



        SUBROUTINE ELS3PV (GM, AL, Q, EI, BOM, OM, TAU, X, Y, Z, XDOT,
     &YDOT, ZDOT)

!        ALGORITHM FOR THREE-DIMENSIONAL CONVERSION
!        FROM ORBITAL ELEMENTS TO POSITION AND VELOCITY.

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)

        CALL ELS2PV (GM, AL, Q, OM, TAU, R, U, VR, VT)
        C = DCOS (U)
        S = DSIN (U)
        X1 = R*C
        Y1 = R*S
        X2 = VR*C - VT*S
        Y2 = VR*S + VT*C
        C = DCOS (EI)
        S = DSIN (EI)
        Z = Y1*S
        Y1 = Y1*C
        ZDOT = Y2*S
        Y2 = Y2*C
        C = DCOS (BOM)
        S = DSIN (BOM)
        X = X1*C - Y1*S
        Y = X1*S + Y1*C
        XDOT = X2*C - Y2*S
        YDOT = X2*S + Y2*C
        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!        Source:
!        The hyperbolic Kepler equation (and the elliptic equation
!        revisited), Gooding, R. H. & Odell, A. W., Celestial Mechanics,
!        Volume 44, Issue 3, pp.267-282
!
!

        DOUBLE PRECISION FUNCTION SHKEPL (EL, G1)

!        EQUATION EL = SHKEPL + (G1 - 1) *DASINH (SHKEPL),
!        WITH G1 IN RANGE O TO 1 INCLUSIVE, SOLVED ACCURATELY.
!
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)

        INTEGER I

        PARAMETER (SW=0.5D0, AHALF=0.5D0, ASIXTH=AHALF/3D0,
     & ATHIRD=ASIXTH* 2 D0)


        S = EL

        IF (EL. EQ.OD0) THEN
            GOTO 2
        END IF

!        STARTER BASED ON LAGRANGE'S THEOREM

        G = 1D0 - G1
        CL = DSQRT (1D0 + EL**2)
        AL = DASINH (EL)
        W = G**2*AL/CL**3
        S = 1D0 - G/CL
        S = EL + G*AL/DCUBRT(S**3 + W*EL* (1.5D0 - G/0.75D0))

!        TWO ITERATIONS (AT MOST) OF HALLEY-THEN-NEWTON PROCESS

        DO I=1,2
            S0 = S*S
            S1 = SO + 1D0
            S2 = DSQRT (S1)
            S3 = S1*S2
            FDD = G*S/S3
            FDDD = G+(1D0 - 2D0*S0)/(S1*S3)

            IF (ASIXTH*S0 + G1 .GE. SW) THEN
                F = (S - G*DASINH(S)) - EL
                FD = 1 D0 - G/S2
            ELSE
                F = SHMKEP(G1, S) - EL
                FD = (S0/ (S2 + 1D0) + GI)/S2
            END IF

            DS = F*FD/ (AHALF*F*FDD - FD*FD)
            STEMP = S + DS

            IF (STEMP. EQ. S) THEN
                GOTO 2
            END IF

            F = F + DS + (FD + AHALF*DS+(FDD + ATHIRD*DS *FDDD))
            FD = FD + DS*(FDD + AHALF*DS *FDDD)
            S = STEMP - F/FD

        END DO
2       SHKEPL = S
        RETURN
        END

        DOUBLE PRECISION FUNCTION EKEPL(EM, E1)

!        KEPLER'S EQUATION, EM = EKEPL - (1 - El) *DSIN (EKEPL),
!        WITH E1 IN RANGE 1 TO O INCLUSIVE, SOLVED ACCURATELY
!        (BASED ON EKEPL3, BUT ENTERING El NOT E)
!
        IMPLICIT DOUBLE PRECISION (A-H, O-Z)

        PARAMETER (PI=3.14159265358979323846264338328D0, TWOPI=2D0*PI,
     & PINEG=-PI, SW=0.25D0, AHALF=0.5D0, ATHIRD=AHALF/1.5D0)

!        RANGE-REDUCE EM TO LIE IN RANGE -PI TO PI

        EMR = DMOD (EM, TWOPI)

        IF (EMR. LT. PINEG) THEN
            EMR = EMR + TWOPI
        END IF
        IF (EMR.GT.PI) THEN
            EMR = EMR - TWOPI
        END IF

        EE = EMR
        IF (EE .NE. 0) THEN

            IF(EE .LT. 0) THEN
                EE = -EE
            END IF

!        (EMR IS RANGE-REDUCED EM & EE IS ABSOLUTE VALUE OF EMR)
!        STARTER BY FIRST SOLVING CUBIC EQUATION
 2      E =1D0 - E1
        W = DCBSOL(E, 2 D0*E1, 3D0*EE)

!        EFFECTIVELY INTERPOLATE IN EMR (ABSOLUTE VALUE)

        EE = (EE*EE + (PI - EE) *W)/PI

        IF (EMR. LT. OD0) THEN
            EE = -EE
        END IF

!        DO TWO ITERATIONS OF HALLEY, EACH FOLLOWED BY NEWTON

            DO ITER=1,2
            FDD = E*DSIN(EE)
            FDDD = E*DCOS(EE)
            IF (EE*EE/6D0 + E1 .GE. SW) THEN
                F = (EE - FDD) - EMR
                FD = 1D0 - FDDD
            ELSE
                F = EMKEP (E1, EE) - EMR
                FD = 2D0*E*DSIN (AHALF*EE)**2 + E1
            END IF

            DEE = F*FD/ (AHALF*F*FDD - FD*FD)
            F = F + DEE* (FD + AHALF*DEE* (FDD + ATHIRD*DEE*FDDD))

    !        TO REDUCE THE DANGER OF UNDERFLOW REPLACE THE LAST LINE BY
    !        W = FD + AHALF DEE* (FDD + ATHIRD DEE*FDDD)
    !
            FD = FD + DEE* (FDD + AHALF*DEE*FDDD)

3           EE = EE + DEE - F/FD
            END DO
    !        IF REPLACING AS ABOVE, THEN ALSO REPLACE THE LAST LINE BY 3 EE = EE - (F - DEE* (FD - W))/FD
    !        RANGE-EXPAND
    !

        END IF

4       EKEPL = EE + (EM - EMR)

        RETURN
        END

        DOUBLE PRECISION FUNCTION SHMKEP (G1, S)

!        ACCURATE COMPUTATION OF S - (1 - GI) *DASINH(S)
!        WHEN (G1, S) IS CLOSE TO (0, 0)

        IMPLICIT DOUBLE PRECISION (A-H, O-Z)
        G = 1D0 - G1
        T = S/ (1D0 + DSQRT (1 D0 + S*S))
        TSQ = T*T
        X = S* (G1 + G*TSQ)
        TERM = 2D0*G*T
        TWOI1 = 1D0
1       TWOI1 = TWOI1 + 2 D0
        TERM = TERM*TSQ
        X0 = X
        X = X - TERM/TWOI1
        IF (X.NE.X0) GO TO 1

        SHMKEP = X
        END






ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!*****************************************************************************************
!>
!  Cube root computed accurately, by incorporating
!  one Newton-Raphson iteration.

!        Source: https://github.com/jacobwilliams/Fortran-Astrodynamics-
!        Toolkit/blob/master/src/gooding_module.f90
!
        function dcubrt(x) result(c)

        implicit none
        REAL*8 AHALF, ATHIRD

        real*8            :: c
        real*8 ,intent(in) :: x

        real*8 :: y

        AHALF=0.5D0
        ATHIRD=AHALF/1.5D0

        if (x .EQ. 0.D0) then
            c = 0.D0
        else
            y = abs(x)
            c = y**athird
            c = c - athird*(c - y/c**2)
            c = sign(c,x)
        end if

        end function dcubrt

!*****************************************************************************************
!>
!  Solution to `a*x**3 + 3*b*x - 2c = 0`, where
!  `a` and `b**3 + a*c**2` are both non-negative
!  (zero generated, in lieu of infinity, if `a = b = 0`)

        DOUBLE PRECISION function dcbsol (a, b, c) result(x)

        implicit none

        real*8,intent(in)    :: a
        real*8,intent(in)    :: b
        real*8,intent(in)    :: c
        REAL*8 AHALF, ATHIRD
        real*8 bsq,d, dtemp
        real*8 dcubrt

        AHALF=0.5D0
        ATHIRD=AHALF/1.5D0
        if (a .EQ. 0.0D0  .and. b .EQ. 0.0D0  .or. c .EQ. 0.0D0 ) then
            x = 0.0D0
        else
            bsq = b*b
            d = sqrt(a) * abs(c)
            dtemp=d + sqrt(b*bsq + d*d)
            d = dcubrt(dtemp)**2.0D0
            x = 2.0D0 * c / (d + b + bsq / d)
        end if

        end function dcbsol

!>
!  Similar to emkepl, except input is `1-e`.
        DOUBLE PRECISION function emkep(e1,ee)

        implicit none

!        real*8            :: emkep
        real*8,intent(in) :: e1
        real*8,intent(in) :: ee

        real*8 :: x, ee2, term, d, x0

        x    = e1*sin(ee)
        ee2  = -ee*ee
        term = ee
        d    = 0.0D0

        do

            d = d + 2.0D0
            term = term*ee2/(d*(d + 1.0D0))
            x0 = x
            x = x - term
            if (x .EQ. x0) exit

        end do

        emkep = x

        end function emkep
