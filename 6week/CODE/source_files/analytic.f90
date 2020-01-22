PROGRAM TRUESOL
  USE SOLVE_SCHR
  USE HERMITE
  USE DEBUGGER
  IMPLICIT NONE

  
  ! INPUT PARAMETERS
!!$  --------------------------------------------------------
  ! the number of user-defined parameters, and a dictionary
  INTEGER*4, PARAMETER :: num_params=4
  REAL*8, DIMENSION(num_params) :: params
!!$  --------------------------------------------------------


  
  ! VARIABLES DEFINITION
!!$  --------------------------------------------------------
  ! stuff for computations
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: egfunc
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: probs
  REAL*8, DIMENSION(:), ALLOCATABLE :: xpts
  INTEGER*4 :: cnt_args, N, ii, kk
  INTEGER*8 :: p_ord
  REAL*8 :: fact_const,func_const
  CHARACTER*10 :: tmp_arg
  
  ! all the physical variables
  REAL*8 :: omega, x_low, x_high, mass, eps
!!$  --------------------------------------------------------


  
  ! DEBUG FLAG
!!$  --------------------------------------------------------
  LOGICAL :: deb = .FALSE.
!!$  --------------------------------------------------------

  
  
  ! INPUT PARAMTERS MANAGEMENT
!!$  --------------------------------------------------------
  ! creating a dictionary
  EQUIVALENCE (params(1), omega)
  EQUIVALENCE (params(2), x_low)
  EQUIVALENCE (params(3), x_high)
  EQUIVALENCE (params(4), mass)


  ! setting default values of the dictionary
  N=101
  kk=5
  omega=1
  x_low=-1.D0
  x_high=1.D0
  mass=(hbar**2)
  

  ! getting command line parameters
  CALL GET_COMMAND_ARGUMENT(1, tmp_arg)
  IF(LEN_TRIM(tmp_arg).NE.0) READ(tmp_arg,*) N

  ! also getting the number of theoretical eigenfunction
  CALL GET_COMMAND_ARGUMENT(2, tmp_arg)
  IF(LEN_TRIM(tmp_arg).NE.0) READ(tmp_arg,*) kk

  ! all physical parameters that can be stored in REAL variables
  DO ii=1+2, COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) params(ii-2)
     END IF
  END DO

  ! some checks
  CALL DEBUG(deb,N,N.LT.0,"N is negative!")
  CALL DEBUG(deb,kk,kk.LT.0,"k is negative!")
  CALL DEBUG(deb,omega,omega.LT.0,"omega is negative!")
  CALL DEBUG(deb,x_high-x_low,x_high-x_low.LT.0,"Extremes not ordered!")  

  ! creating the grid of points
  eps=(x_high-x_low)/N
  
  ALLOCATE(xpts(N))
  DO ii=1,N
     xpts(ii)=x_low+eps*(ii-1)
  END DO
!!$  --------------------------------------------------------

  

  ! COMPUTING THE THEORETICAL EIGENFUNCTIONS
!!$  --------------------------------------------------------

  ! allocating the necessary arrays
  ALLOCATE(egfunc(N,kk))

  
  ! defining the constants and the values for the computation
  ! of the Hermite polynomials. A subroutine I found on the internet
  ! was used to compute the polynomials themselves.
  
  func_const=(mass*omega/hbar)**0.5 
  DO p_ord=1,kk
     fact_const= FLOAT(FACTORIAL(p_ord-1))
     egfunc(:,p_ord)=(func_const/SQRT(pi))**0.5/SQRT(fact_const*(2**(p_ord-1)))
     egfunc(:,p_ord)=egfunc(:,p_ord)*evalHerm_Poly(func_const*xpts, p_ord-1)*&
          EXP(-0.5*(func_const*xpts)**2)
  END DO

  
  !computing the related probabilities
  ALLOCATE(probs(N,kk))
  CALL COMPUTE_PROBABILITIES(egfunc,probs)
!!$  --------------------------------------------------------


  
  ! WRITING EVERYTHING ON FILE
!!$  --------------------------------------------------------  
  CALL WRITE_EIGVECS('exact_probs.dat', xpts,probs)
  CALL WRITE_EIGVECS('hermite.dat', xpts,REAL(egfunc))
!!$  --------------------------------------------------------  

  
END PROGRAM TRUESOL
