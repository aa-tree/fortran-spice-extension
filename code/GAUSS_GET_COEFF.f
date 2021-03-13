CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       Written by: Anshuk Attri
C
C       Contact: contact@anshukattri.in
C       Website: www.anshukattri.in/research
C       GITHUB: github.com/aa-tree/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


        SUBROUTINE GAUSS_GET_COEFF(COEFF, G_COEFF)

C
C       COEFF   INPUT, CHARACTER
C
C
C
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IMPLICIT NONE
        INCLUDE 'common.dat'

        CHARACTER*(*) COEFF
        REAL*8 G_COEFF(10,9)


        IF(COEFF .EQ. 'b') THEN

C
C       Return b_jk coefficients for Gauss Jackson.
C       Source: Matthew M. Berry and Liam M. Healy, Implementation of
C       Gauss-Jackson Integration for Orbit Propagation,
C       The Journal of the Astronautical Sciences, Vol. 52, No. 3,
C       July–September 2004, pp. 331–357
C

C
C       Index has a bias of 5 so G(-4,-4) will be G(1,1)
C

C       (-4,-4)
        G_COEFF(1,1)= 19087.0D0/89600.0D0

C       (-3,-4)
        G_COEFF(2,1)= 8183.0D0/1036800.0D0

C       (-2,-4)
        G_COEFF(3,1)= -425.0D0/290304.0D0


C       (-1,-4)
        G_COEFF(4,1)= 7.0D0/12800.0D0


C       (0,-4)
        G_COEFF(5,1)= -2497.0D0/7257600.0D0

C       (1,-4)
        G_COEFF(6,1)= 2497.0D0/7257600.0D0

C       (2,-4)
        G_COEFF(7,1)= -7.0D0/12800.0D0

C       (3,-4)
        G_COEFF(8,1)=425.0D0/290304.0D0

C       (4,-4)
        G_COEFF(9,1)= -8183.0D0/1036800.0D0

C       (5,-4)
        G_COEFF(10,1)= 25713.0D0/89600.0D0

C       (-4,-3)
        G_COEFF(1,2)= -427487.0D0/725760.0D0

C       (-3,-3)
        G_COEFF(2,2)= 57251.0D0/403200.0D0

C       (-2,-3)
        G_COEFF(3,2)= 76453.0D0/3628800.0D0

C       (-1,-3)
        G_COEFF(4,2)= -23173.0D0/3628800.0D0

C       (0,-3)
        G_COEFF(5,2)= 1469.0D0/403200.0D0

C       (1,-3)
        G_COEFF(6,2)= -2497.0D0/725760.0D0

C       (2,-3)
        G_COEFF(7,2)= 19109.0D0/3628800.0D0

C       (3,-3)
        G_COEFF(8,2)= -5533.0D0/403200.0D0

C       (4,-3)
        G_COEFF(9,2)= 263077.0D0/3628800.0D0

C       (5,-3)
        G_COEFF(10,2)= -9401029.0D0/3628800.0D0



C       (-4,-2)
        G_COEFF(1,3)= 3498217.0D0/3628800.0D0


C       (-3,-2)
        G_COEFF(2,3)= -1106377.0D0/3628800.0D0


C       (-2,-2)
        G_COEFF(3,3)= 5143.0D0/57600.0D0

C       (-1,-2)
        G_COEFF(4,3)= 29579.0D0/725760.0D0

C       (0,-2)
        G_COEFF(5,3)= -68119.0D0/3628800.0D0

C       (1,-2)
        G_COEFF(6,3)= 6463.0D0/403200.0D0

C       (2,-2)
        G_COEFF(7,3)= -83927.0D0/3628800.0D0

C       (3,-2)
        G_COEFF(8,3)= 210359.0D0/3628800.0D0

C       (4,-2)
        G_COEFF(9,3)= -24019.0D0/80640.0D0

C       (5,-2)
        G_COEFF(10,3)= 5393233.0D0/518400.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       (-4,-1)
        G_COEFF(1,4)= -500327.0D0/403200.0D0

C       (-3,-1)
        G_COEFF(2,4)= 218483.0D0/725760.0D0

C       (-2,-1)
        G_COEFF(3,4)= -660127.0D0/3628800.0D0

C       (-1,-1)
        G_COEFF(4,4)= 2497.0D0/57600.0D0

C       (0,-1)
        G_COEFF(5,4)= 252769.0D0/3628800.0D0

C       (1,-1)
        G_COEFF(6,4)= -172993.0D0/3628800.0D0

C       (2,-1)
        G_COEFF(7,4)= 4997.0D0/80640.0D0

C       (3,-1)
        G_COEFF(8,4)= -530177.0D0/3628800.0D0

C       (4,-1)
        G_COEFF(9,4)= 2616161.0D0/3628800.0D0

C       (5,-1)
        G_COEFF(10,4)= -9839609.0D0/403200.0D0

CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       (-4,0)
        G_COEFF(1,5)= 6467.0D0/5670.0D0

C       (-3,0)
        G_COEFF(2,5)= -69.0D0/280.0D0

C       (-2,0)
        G_COEFF(3,5)= 661.0D0/5670.0D0

C       (-1,0)
        G_COEFF(4,5)= -2563.0D0/22680.0D0

C       (0,0)
        G_COEFF(5,5)= 0.0D0

C       (1,0)
        G_COEFF(6,5)= 2563.0D0/22680.0D0

C       (2,0)
        G_COEFF(7,5)= -661.0D0/5670.0D0

C       (3,0)
        G_COEFF(8,5)= 69.0D0/280.0D0

C       (4,0)
        G_COEFF(9,5)= -6467.0D0/5670.0D0

C       (5,0)
        G_COEFF(10,5)= 167287.0D0/4536.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       (-4,1)
        G_COEFF(1,6)= -2616161.0D0/3628800.0D0

C       (-3,1)
        G_COEFF(2,6)= 530177.0D0/3628800.0D0

C       (-2,1)
        G_COEFF(3,6)= -4997.0D0/80640.0D0

C       (-1,1)
        G_COEFF(4,6)= 172993.0D0/3628800.0D0

C       (0,1)
        G_COEFF(5,6)= -252769.0D0/3628800.0D0

C       (1,1)
        G_COEFF(6,6)= -2497.0D0/57600.0D0

C       (2,1)
        G_COEFF(7,6)= 660127.0D0/3628800.0D0

C       (3,1)
        G_COEFF(8,6)= -218483.0D0/725760.0D0

C       (4,1)
        G_COEFF(9,6)= 500327.0D0/403200.0D0

C       (5,1)
        G_COEFF(10,6)= -135352319.0D0/3628800.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       (-4,2)
        G_COEFF(1,7)= 24019.0D0/80640.0D0

C       (-3,2)
        G_COEFF(2,7)= -210359.0D0/3628800.0D0

C       (-2,2)
        G_COEFF(3,7)= 83927.0D0/3628800.0D0

C       (-1,2)
        G_COEFF(4,7)= -6463.0D0/403200.0D0

C       (0,2)
        G_COEFF(5,7)= 68119.0D0/3628800.0D0

C       (1,2)
        G_COEFF(6,7)= -29579.0D0/725760.0D0

C       (2,2)
        G_COEFF(7,7)= -5143.0D0/57600.0D0

C       (3,2)
        G_COEFF(8,7)= 1106377.0D0/3628800.0D0

C       (4,2)
        G_COEFF(9,7)= -3498217.0D0/3628800.0D0

C       (5,2)
        G_COEFF(10,7)= 10219841.0D0/403200.0D0

CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,3)
        G_COEFF(1,8)= -263077.0D0/3628800.0D0

C       (-3,3)
        G_COEFF(2,8)= 5533.0D0/403200.0D0

C       (-2,3)
        G_COEFF(3,8)= -19109.0D0/3628800.0D0

C       (-1,3)
        G_COEFF(4,8)= 2497.0D0/725760.0D0

C       (0,3)
        G_COEFF(5,8)= -1469.0D0/403200.0D0

C       (1,3)
        G_COEFF(6,8)= 23173.0D0/3628800.0D0

C       (2,3)
        G_COEFF(7,8)= -76453.0D0/3628800.0D0

C       (3,3)
        G_COEFF(8,8)= -57251.0D0/403200.0D0

C       (4,3)
        G_COEFF(9,8)= 427487.0D0/725760.0D0

C       (5,3)
        G_COEFF(10,8)= -40987771.0D0/3628800.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,4)
        G_COEFF(1,9)= 8183.0D0/1036800.0D0

C       (-3,4)
        G_COEFF(2 ,9)= -425.0D0/290304.0D0

C       (-2,4)
        G_COEFF(3 ,9)= 7.0D0/12800.0D0

C       (-1,4)
        G_COEFF(4 ,9)= -2497.0D0/7257600.0D0

C       (0,4)
        G_COEFF(5 ,9)= 2497.0D0/7257600.0D0

C       (1,4)
        G_COEFF(6 ,9)= -7.0D0/12800.0D0

C       (2,4)
        G_COEFF(7 ,9)= 425.0D0/290304.0D0

C       (3,4)
        G_COEFF(8,9)= -8183.0D0/1036800.0D0

C       (4,4)
        G_COEFF(9,9)= -19087.0D0/89600.0D0

C       (5,4)
        G_COEFF(10,9)= 3288521.0D0/1036800.0D0

        END IF
C
C       Return a_jk coefficients for Gauss Jackson.
C       Source: Matthew M. Berry and Liam M. Healy, Implementation of
C       Gauss-Jackson Integration for Orbit Propagation,
C       The Journal of the Astronautical Sciences, Vol. 52, No. 3,
C       July–September 2004, pp. 331–357
C

C
C       Index has a bias of 5 so G(-4,-4) will be G(1,1)
C


        IF(COEFF .EQ. 'a') THEN

C
C       Return a_jk coefficients for Gauss Jackson.
C       Source:
C

C
C       Index has a bias of 5 so G(-4,-4) will be G(1,1)
C

C       (-4,-4)
        G_COEFF(1,1)= 3250433.0D0/53222400.0D0

C       (-3,-4)
        G_COEFF(2,1)= -330157.0D0/159667200.0D0

C       (-2,-4)
        G_COEFF(3,1)= 45911.0D0/159667200.0D0


C       (-1,-4)
        G_COEFF(4,1)= -3499.0D0/53222400.0D0


C       (0,-4)
        G_COEFF(5,1)= 317.0D0/22809600.0D0

C       (1,-4)
        G_COEFF(6,1)= 317.0D0/22809600.0D0

C       (2,-4)
        G_COEFF(7,1)= -3499.0D0/53222400.0D0

C       (3,-4)
        G_COEFF(8,1)= 45911.0D0/159667200.0D0

C       (4,-4)
        G_COEFF(9,1)= -330157.0D0/159667200.0D0

C       (5,-4)
        G_COEFF(10,1)= 3250433.0D0/53222400.0D0

C       (-4,-3)
        G_COEFF(1,2)= 572741.0D0/5702400.0D0

C       (-3,-3)
        G_COEFF(2,2)= 530113.0D0/6652800.0D0

C       (-2,-3)
        G_COEFF(3,2)= -185839.0D0/39916800.0D0

C       (-1,-3)
        G_COEFF(4,2)= 4387.0D0/4989600.0D0

C       (0,-3)
        G_COEFF(5,2)= -2539.0D0/13305600.0D0

C       (1,-3)
        G_COEFF(6,2)= -317.0D0/2851200.0D0

C       (2,-3)
        G_COEFF(7,2)= 24173.0D0/39916800.0D0

C       (3,-3)
        G_COEFF(8,2)= -1261.0D0/475200.0D0

C       (4,-3)
        G_COEFF(9,2)= 754331.0D0/39916800.0D0

C       (5,-3)
        G_COEFF(10,2)= -11011481.0D0/19958400.0D0



C       (-4,-2)
        G_COEFF(1,3)= -8701681.0D0/39916800.0D0


C       (-3,-2)
        G_COEFF(2,3)= 518887.0D0/19958400.0D0


C       (-2,-2)
        G_COEFF(3,3)= 171137.0D0/1900800.0D0

C       (-1,-2)
        G_COEFF(4,3)= -35039.0D0/4989600.0D0

C       (0,-2)
        G_COEFF(5,3)= 55067.0D0/39916800.0D0

C       (1,-2)
        G_COEFF(6,3)= 2059.0D0/6652800.0D0

C       (2,-2)
        G_COEFF(7,3)= -98911.0D0/39916800.0D0

C       (3,-2)
        G_COEFF(8,3)= 109343.0D0/9979200.0D0

C       (4,-2)
        G_COEFF(9,3)= -1025779.0D0/13305600.0D0

C       (5,-2)
        G_COEFF(10,3)= 6322573.0D0/2851200.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,-1)
        G_COEFF(1,4)= 4026311.0D0/13305600.0D0

C       (-3,-1)
        G_COEFF(2,4)= -27631.0D0/623700.0D0

C       (-2,-1)
        G_COEFF(3,4)= 73643.0D0/39916800.0D0

C       (-1,-1)
        G_COEFF(4,4)= 90817.0D0/950400.0D0

C       (0,-1)
        G_COEFF(5,4)= -326911.0D0/39916800.0D0

C       (1,-1)
        G_COEFF(6,4)= 2117.0D0/9979200.0D0

C       (2,-1)
        G_COEFF(7,4)= 77597.0D0/13305600.0D0

C       (3,-1)
        G_COEFF(8,4)= -531521.0D0/19958400.0D0

C       (4,-1)
        G_COEFF(9,4)= 7370669.0D0/39916800.0D0

C       (5,-1)
        G_COEFF(10,4)= -8660609.0D0/1663200.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,0)
        G_COEFF(1,5)= -917039.0D0/3193344.0D0

C       (-3,0)
        G_COEFF(2,5)= 44773.0D0/1064448.0D0

C       (-2,0)
        G_COEFF(3,5)= -25775.0D0/3193344.0D0

C       (-1,0)
        G_COEFF(4,5)= -20561.0D0/3193344.0D0

C       (0,0)
        G_COEFF(5,5)= 14797.0D0/152064.0D0

C       (1,0)
        G_COEFF(6,5)= -20561.0D0/3193344.0D0

C       (2,0)
        G_COEFF(7,5)= -25775.0D0/3193344.0D0

C       (3,0)
        G_COEFF(8,5)= 44773.0D0/1064448.0D0

C       (4,0)
        G_COEFF(9,5)= -917039.0D0/3193344.0D0

C       (5,0)
        G_COEFF(10,5)= 25162927.0D0/3193344.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,1)
        G_COEFF(1,6)= 7370669.0D0/39916800.0D0

C       (-3,1)
        G_COEFF(2,6)= -531521.0D0/19958400.0D0

C       (-2,1)
        G_COEFF(3,6)= 77597.0D0/13305600.0D0

C       (-1,1)
        G_COEFF(4,6)= 2117.0D0/9979200.0D0

C       (0,1)
        G_COEFF(5,6)= -326911.0D0/39916800.0D0

C       (1,1)
        G_COEFF(6,6)= 90817.0D0/950400.0D0

C       (2,1)
        G_COEFF(7,6)= 73643.0D0/39916800.0D0

C       (3,1)
        G_COEFF(8,6)= -27631.0D0/623700.0D0

C       (4,1)
        G_COEFF(9,6)= 4026311.0D0/13305600.0D0

C       (5,1)
        G_COEFF(10,6)= -159314453.0D0/19958400.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,2)
        G_COEFF(1,7)= -1025779.0D0/13305600.0D0

C       (-3,2)
        G_COEFF(2,7)= 109343.0D0/9979200.0D0

C       (-2,2)
        G_COEFF(3,7)= -98911.0D0/39916800.0D0

C       (-1,2)
        G_COEFF(4,7)= 2059.0D0/6652800.0D0

C       (0,2)
        G_COEFF(5,7)= 55067.0D0/39916800.0D0

C       (1,2)
        G_COEFF(6,7)= -35039.0D0/4989600.0D0

C       (2,2)
        G_COEFF(7,7)= 171137.0D0/1900800.0D0

C       (3,2)
        G_COEFF(8,7)= 518887.0D0/19958400.0D0

C       (4,2)
        G_COEFF(9,7)= -8701681.0D0/39916800.0D0

C       (5,2)
        G_COEFF(10,7)= 18071351.0D0/3326400.0D0

CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,3)
        G_COEFF(1,8)= 754331.0D0/39916800.0D0

C       (-3,3)
        G_COEFF(2,8)= -1261.0D0/475200.0D0

C       (-2,3)
        G_COEFF(3,8)= 24173.0D0/39916800.0D0

C       (-1,3)
        G_COEFF(4,8)= -317.0D0/2851200.0D0

C       (0,3)
        G_COEFF(5,8)= -2539.0D0/13305600.0D0

C       (1,3)
        G_COEFF(6,8)= 4387.0D0/4989600.0D0

C       (2,3)
        G_COEFF(7,8)= -185839.0D0/39916800.0D0

C       (3,3)
        G_COEFF(8,8)= 530113.0D0/6652800.0D0

C       (4,3)
        G_COEFF(9,8)= 572741.0D0/5702400.0D0

C       (5,3)
        G_COEFF(10,8)= -24115843.0D0/9979200.0D0


CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       (-4,4)
        G_COEFF(1,9)= -330157.0D0/159667200.0D0

C       (-3,4)
        G_COEFF(2 ,9)= 45911.0D0/159667200.0D0

C       (-2,4)
        G_COEFF(3 ,9)= -3499.0D0/53222400.0D0

C       (-1,4)
        G_COEFF(4 ,9)= 317.0D0/22809600.0D0

C       (0,4)
        G_COEFF(5 ,9)= 317.0D0/22809600.0D0

C       (1,4)
        G_COEFF(6 ,9)= -3499.0D0/53222400.0D0

C       (2,4)
        G_COEFF(7 ,9)= 45911.0D0/159667200.0D0

C       (3,4)
        G_COEFF(8,9)= -330157.0D0/159667200.0D0

C       (4,4)
        G_COEFF(9,9)= 3250433.0D0/53222400.0D0

C       (5,4)
        G_COEFF(10,9)= 103798439.0D0/159667200.0D0

        END IF

        RETURN
        END SUBROUTINE
