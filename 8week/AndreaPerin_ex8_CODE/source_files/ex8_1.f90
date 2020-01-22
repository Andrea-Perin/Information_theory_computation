PROGRAM MAIN
  
  USE DEBUGGER
  USE UTILS
  IMPLICIT NONE

  ! VARIABLE DECLARATION
!!$  --------------------------------------------------------
  ! input management parameters
  INTEGER*4, PARAMETER :: num_int_params=2
  INTEGER*4, DIMENSION(num_int_params) :: intparams
  !physical quantities
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: sep_comp,rhosep,rhonsep
  COMPLEX*16, DIMENSION(:), ALLOCATABLE :: psi_nsep, psi_sep
  INTEGER*4 :: dd, NN
  !utility variables
  CHARACTER*10 :: tmp_arg
  INTEGER*4 :: ii, jj, kk
  INTEGER*2, DIMENSION(4) :: seed
!!$  --------------------------------------------------------
  
! DEBUG FLAG
!!$  --------------------------------------------------------
  LOGICAL :: deb = .TRUE.
!!$  --------------------------------------------------------
  
  ! INPUT PARAMETERS MANAGEMENT
!!$  --------------------------------------------------------
  ! creating a dictionary for int variables
  EQUIVALENCE (intparams(1), NN)
  EQUIVALENCE (intparams(2), dd)  
  !number of subsystems (Hilbert spaces)
  NN=2
  !number of dimension for each Hilbert space
  dd=2 
  ! getting command line parameters
  DO ii=1, COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) intparams(ii)
     END IF
  END DO
!!$  --------------------------------------------------------




  
  !ALLOCATING FOR SEPARABLE STATE
!!$  --------------------------------------------------------
  !A pure separable state with N components can be written
  !as the tensor product of N pure states. As a consequence,
  !in order to store it, only a (ddxNN) matrix is needed.
  ALLOCATE(sep_comp(dd,NN))
!!$  --------------------------------------------------------
  

  !INITIALIZING THE STATE RANDOMLY
!!$  --------------------------------------------------------
  !randomly initialize each column, and normalizing
  DO ii=1,NN
     CALL RNDINIT(sep_comp(:,ii))
     sep_comp(:,ii)=ZNORMALIZE(sep_comp(:,ii))
  END DO
  !checking the normalization
  DO ii=1,NN
     CALL DEBUG(deb,sep_comp(:,ii),&
          ABS(SUM(ABS(sep_comp(:,ii))**2)-1 ).GE.1d-14,&
          "A state is not normalized.")
  END DO
!!$  --------------------------------------------------------

  
  !ALLOCATING FOR NOT SEPARABLE STATE
!!$  --------------------------------------------------------  
  !Things are more complicated when one considers a general
  !pure wave function; in this case, mixing can indeed
  !happen, and so a rank-NN tensor must be used, with each
  !of the sides having in general dd elements.
  !However, instead of using a matrix representation,
  !an appropriately lon vector will be employed.
  ALLOCATE(psi_nsep(dd**NN))
!!$  --------------------------------------------------------  

  
  !INITIALIZING AND NORMALIZING
!!$  --------------------------------------------------------  
  !randomly intialize all of pure_notsep
  CALL RNDINIT(psi_nsep)
  psi_nsep = ZNORMALIZE(psi_nsep)
  !checking the normalization
  CALL DEBUG(deb,psi_nsep,ABS( SUM(ABS(psi_nsep)**2)-1 ).GE.1d-14,&
       "Pure not separable state is not normalized.")
!!$  --------------------------------------------------------  

  
  !CREATING A STATE FROM THE COMPRESSED REPRESENTATION OF THE
  !PURE SEPARABLE STATE
!!$  --------------------------------------------------------
  !once the state is computed, it requires dd**NN elements
  ALLOCATE(psi_sep(dd**NN))
  !mapping the compressed representation on a dd*NN vector
  psi_sep=SEP_TO_STATE(sep_comp)
  !checking if the state is already normalized
  CALL DEBUG(deb,psi_sep,ABS( SUM(ABS(psi_sep)**2)-1 ).GE.1d-5,&
       "Pure separable state is not normalized.")
!!$  --------------------------------------------------------

  !the following is commented: memory errors are likely due to
  !excessive memory requirements!
  
!!$  !CREATING THE RESPECTIVE DENSITY MATRICES AND CHECKING THE TRACE
 !--------------------------------------------------------
!!$  !both matrices need (dd**NN)**2 elements
!!$  ALLOCATE(rhosep(dd**NN,dd**NN))
!!$  ALLOCATE(rhonsep(dd**NN,dd**NN))
!!$  !outer complex product for the matrices
!!$  rhosep=ZOUTER_PROD(psi_sep,psi_sep)
!!$  rhonsep=ZOUTER_PROD(psi_nsep,psi_nsep)
!!$  !checking if the traces are unitary
!!$  CALL DEBUG(deb,rhosep,ABS( ZTRACE(rhosep)-1 ).GE.1d-14,&
!!$       "rhosep trace is not normalized.")
!!$  CALL DEBUG(deb,rhonsep,ABS( ZTRACE(rhonsep)-1 ).GE.1d-14,&
!!$       "rhonsep trace is not normalized.")
 !--------------------------------------------------------



END PROGRAM MAIN
