      MODULE MEX2
      IMPLICIT NONE
      integer*2  num1_2, num2_2
      integer*4  num1_4, num2_4
      real*4  pi_4,e32_4,root2_4,e21_4
      real*8  pi_8,e32_8,root2_8,e21_8
      END MODULE MEX2





      PROGRAM EX2
      USE MEX2
      IMPLICIT NONE
      root2_4 = SQRT(2.)
      root2_8 = SQRT(2.D0)
      pi_4 = ACOS(-1.)
      pi_8 = ACOS(-1.D0)
      e32_4 = 10e32
      e21_4 = 10e21
      e32_8 = 10d32
      e21_8 = 10d21
      num1_2=2000000
      num2_2=1
      num1_4=2000000
      num2_4=1

      print*, "Using integer*2, 2000000+1=", num1_2+num2_2
      print*, "Using integer*4, 2000000+1=", num1_4+num2_4
      print*, "Using real*4, pie32+sqrt(2)e21=",pi_4*e32_4+root2_4*e21_4
      print*, "Using real*8, pie32+sqrt(2)e21=",pi_8*e32_8+root2_8*e21_8
      STOP
      END PROGRAM EX2
