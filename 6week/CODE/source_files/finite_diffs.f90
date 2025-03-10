PROGRAM SCHRODINGER
  USE SOLVE_SCHR
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
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: T,V,H
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: probs
  REAL*8, DIMENSION(:), ALLOCATABLE :: egvals,xpts
  INTEGER*4 :: info,cnt_args, ii, jj, N
  CHARACTER*10 :: tmp_arg
  
  ! all the physical variables
  REAL*8 :: omega, x_low, x_high, mass, eps
!!$  --------------------------------------------------------


  ! DEBUG FLAG
!!$  --------------------------------------------------------
  LOGICAL :: deb = .FALSE.
!!$  --------------------------------------------------------
  
  
  ! INPUT PARAMETERS MANAGEMENT
!!$  --------------------------------------------------------
  ! creating a dictionary for real variables
  EQUIVALENCE (params(1), omega)
  EQUIVALENCE (params(2), x_low)
  EQUIVALENCE (params(3), x_high)
  EQUIVALENCE (params(4), mass)

  ! setting default values of the dictionary
  N=101
  omega=1
  x_low=-1.D0
  x_high=1.D0
  mass=(hbar**2)
  
  ! getting command line parameters
  CALL GET_COMMAND_ARGUMENT(1, tmp_arg)
  IF(LEN_TRIM(tmp_arg).NE.0) READ(tmp_arg,*) N
  
  DO ii=2, COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) params(ii-1)
     END IF
  END DO

  ! some checks
  CALL DEBUG(deb,N,N.LT.0,"N is negative!")
  CALL DEBUG(deb,omega,omega.LT.0,"omega is negative!")
  CALL DEBUG(deb,x_high-x_low,x_high-x_low.LT.0,"Extremes not ordered!")

  ! allocating the variables
  ALLOCATE(H(N,N))
  ALLOCATE(egvals(N))
  ALLOCATE(probs(N,N))
  ALLOCATE(xpts(N))

  ! defining the grid over which to compute everything
  eps=(x_high-x_low)/N
  DO ii=1,N
     xpts(ii)=x_low+eps*(ii-1)
  END DO
!!$  --------------------------------------------------------


  
  ! DEFINING THE OPERATORS
!!$  --------------------------------------------------------
  ! creating the laplacian and the potential
  CALL LAPLACIAN(N, T, is_pbc=.FALSE.)
  CALL HARM_POTENTIAL(N, V, omega,x_low,x_high)

  ! writing the hamiltonian
  H=-((hbar/eps)**2/(2*mass))*T+mass*V/2
!!$  --------------------------------------------------------


  ! DIAGONALIZE AND CHECK
!!$  --------------------------------------------------------
  ! getting the eigenvalues and eigenvectors
  CALL GET_EGV(H,egvals,info)

  !checking if successful diagonalization
  IF(INFO.NE.0)THEN
     
     PRINT*, 'ERROR: FAILED DIAGONALIATION!'

  ELSE     

     ! normalizing the eigenvectors
     H=H/SQRT(eps)

     ! computing the probabilities
     CALL COMPUTE_PROBABILITIES(H,probs)
!!$  --------------------------------------------------------



     ! SAVING THE PRODUCED DATA (IF SUCCESSFUL DIAG)
!!$  --------------------------------------------------------  
     CALL WRITE_EIGVALS('eigvals.dat', egvals)
     CALL WRITE_EIGVECS('eigvecs.dat', xpts, REAL(H))
     CALL WRITE_EIGVECS('probs.dat', xpts,probs)
!!$  --------------------------------------------------------

  END IF
  
END PROGRAM SCHRODINGER
