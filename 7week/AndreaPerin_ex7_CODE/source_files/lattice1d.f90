MODULE LATTICE1D

  use, intrinsic :: iso_c_binding
  USE FFTW3

  IMPLICIT NONE


  TYPE LTC1D
     INTEGER*4 :: numpoints
     REAL*8 :: stepsize, lower, upper
     REAL*8, DIMENSION(:), ALLOCATABLE :: grid
  END TYPE LTC1D

CONTAINS

  FUNCTION LINSPACE(npts,lower,upper)RESULT(ltc)
    REAL*8, DIMENSION(npts) :: ltc
    INTEGER*4 :: npts
    REAL*8 :: lower,upper,step

    INTEGER*4 :: ii

    step = (upper-lower)/(npts-1)
    DO ii=1,npts
       ltc(ii)=lower+(ii-1)*step
    END DO
  END FUNCTION LINSPACE

  
  FUNCTION DHP(ltc,center)
    REAL*8, DIMENSION(:) :: ltc
    REAL*8 :: center
    REAL*8, DIMENSION(SIZE(ltc)) :: DHP

    DHP=(ltc-center)**2
  END FUNCTION DHP

  
  FUNCTION FT_1D(func)RESULT(res)
    COMPLEX*16, DIMENSION(:) :: func
    COMPLEX*16, DIMENSION(SIZE(func)) :: res
    INTEGER*8 :: plan,veclen

    veclen=SIZE(func)
    
    CALL dfftw_plan_dft_1d(plan,veclen,func,res,FFTW_FORWARD,FFTW_ESTIMATE)
    CALL dfftw_execute_dft(plan, func, res)
    CALL dfftw_destroy_plan(plan)
    res=res/SQRT(REAL(veclen))
    RETURN
  END FUNCTION FT_1D
  
  
  FUNCTION AFT_1D(func)RESULT(res)
    COMPLEX*16, DIMENSION(:) :: func
    COMPLEX*16, DIMENSION(SIZE(func)) :: res
    INTEGER*8 :: plan,veclen

    veclen=SIZE(func)
    
    CALL dfftw_plan_dft_1d(plan,veclen,func,res,FFTW_BACKWARD,FFTW_ESTIMATE)
    CALL dfftw_execute_dft(plan, func, res)
    CALL dfftw_destroy_plan(plan)
    res=res/SQRT(REAL(veclen))
    RETURN
  END FUNCTION AFT_1D
  

END MODULE LATTICE1D


  
    
