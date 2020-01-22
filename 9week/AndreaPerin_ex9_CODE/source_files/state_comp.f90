PROGRAM SPINCHAIN_STATES
  USE DEBUGGER
  USE UTILS


  IMPLICIT NONE


  ! VARIABLE DECLARATION
!!$  --------------------------------------------------------
  ! input management parameters
  INTEGER*4, PARAMETER :: num_int_params=1, num_real_params=1
  INTEGER*4, DIMENSION(num_int_params) :: intparams
  REAL*8, DIMENSION(num_real_params) :: realparams
  
  !physical quantities
  INTEGER*4, DIMENSION(:), ALLOCATABLE :: diag
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: hamiltonian, egvecs, dmat 
  COMPLEX*16, DIMENSION(:), ALLOCATABLE :: ground_state
  REAL*8, DIMENSION(:)  , ALLOCATABLE :: egvals
  COMPLEX*16, DIMENSION(2,2), PARAMETER ::&
       id = (reshape((/ &
             CMPLX(1d0,0d0),CMPLX(0d0,0d0),&
             CMPLX(0d0,0d0),CMPLX(1d0,0d0)&
             /), shape(id))),&
       sigx = reshape((/ &
             CMPLX(0d0,0d0),CMPLX(1d0,0d0),&
             CMPLX(1d0,0d0),CMPLX(0d0,0d0)&
             /), shape(sigx)),&
       sigz = reshape((/ &
             CMPLX(1d0,0d0),CMPLX(0d0,0d0),&
             CMPLX(0d0,0d0),CMPLX(-1d0,0d0)&
             /), shape(sigz))
  INTEGER*4 :: NN, step
  REAL*8 :: lambda

  !utility variables
  CHARACTER*10 :: tmp_arg
  INTEGER*4 :: ii, jj, kk, info
!!$  --------------------------------------------------------

  
! DEBUG FLAG
!!$  --------------------------------------------------------
  LOGICAL :: deb = .TRUE.
!!$  --------------------------------------------------------

  
  ! INPUT PARAMETERS MANAGEMENT
!!$  --------------------------------------------------------
  ! creating a dictionary for int variables
  EQUIVALENCE (intparams(1), NN)
  ! creating a dictionary for int variables
  EQUIVALENCE (realparams(1), lambda)
  NN=2 !number of subsystems (Hilbert spaces)
  lambda=0.5d0
  ! getting command line parameters
  DO ii=1, COMMAND_ARGUMENT_COUNT()-num_real_params
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) intparams(ii)
     END IF
  END DO
  DO ii=num_int_params+1, COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) realparams(ii-num_int_params)
     END IF
  END DO
!!$  --------------------------------------------------------

  
  !CREATING THE TOTAL HAMILTONIAN
!!$  --------------------------------------------------------
  !allocating both the hamiltonian and the dd-dimensional identity
  kk=5
  ALLOCATE(hamiltonian(2**NN,2**NN))
  ALLOCATE(diag(2**NN))
  ALLOCATE(egvals(2**NN))
  ALLOCATE(egvecs(2**NN,kk))
!!$  --------------------------------------------------------


  ! CONSTRUCTING THE DIAG
!!$  --------------------------------------------------------
  !creating the diagonal part of the hamiltonian;
  !These diagonal terms, which come from the non-interacting part
  !of the hamiltonian, are computed one at a time. The formula is
  !derived from the matrix element expression.
  diag=NN
  DO ii=0,2**NN-1
     DO jj=0,NN-1
        diag(ii+1)=diag(ii+1)-2*MOD(ii/(2**jj),2)
     END DO
  END DO
!!$  --------------------------------------------------------

  
  ! CONSTRUCTING THE INTERACTION PART
!!$  --------------------------------------------------------
  !creating the interaction part of the hamiltonian
  !Also this part comes from direct computation.
  DO ii=0,2**NN-1
     DO jj=0,2**NN-1
        hamiltonian(jj+1,ii+1)=XOR(jj,ii,NN)
     END DO
  END DO
!!$  --------------------------------------------------------

 
  !COMPOSING INTO A SINGLE MATRIX
!!$  --------------------------------------------------------
  !Joining the diagonal and the off-diagonal parts
  DO ii=1,2**NN
     hamiltonian(ii,ii)=hamiltonian(ii,ii)+lambda*diag(ii)
  END DO
!!$  --------------------------------------------------------

  
  !DIAGONALIZING
!!$  --------------------------------------------------------  
  CALL GET_EGV(hamiltonian,egvals,info)
  CALL DEBUG(deb,info,info.NE.0,"ERROR: diagonalization failed.")
  PRINT*, lambda, ABS(hamiltonian(:,1))**2
!!$  --------------------------------------------------------

    
END PROGRAM SPINCHAIN_STATES
