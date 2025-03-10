PROGRAM MAIN_2

  USE DEBUGGER
  USE UTILS
  IMPLICIT NONE

! VARIABLE DECLARATION
!!$  --------------------------------------------------------
  ! input management parameters
  INTEGER*4, PARAMETER :: num_int_params=2
  INTEGER*4, DIMENSION(num_int_params) :: intparams

  !physical quantities
  COMPLEX*16, DIMENSION(:), ALLOCATABLE :: psi, psi_sep
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: rho, red_rho, r1,r2,r3
  INTEGER*4 :: dd, NN, out_idx

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
  
  NN=2 !number of subsystems (Hilbert spaces)
  dd=2 !number of dimension for each Hilbert space

  ! getting command line parameters
  DO ii=1, COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) intparams(ii)
     END IF
  END DO
!!$  --------------------------------------------------------

  
  !CREATING THE DENSITY MATRIX FROM A GENERIC PURE STATE
!!$  --------------------------------------------------------
  !creating the density matrix from a state
  ALLOCATE(psi(dd**NN))
  ALLOCATE(rho(dd**NN,dd**NN))

  !creating a (random) normalized pure state
  seed=(/1,2,3,5/)
  CALL RNDINIT(psi)
  psi = ZNORMALIZE( psi )
  CALL DEBUG(deb,psi,(ABS(SUM(ABS(psi)**2)-1).GE.1d-5),&
       "Wavefunction not normalized!")

  !creating the respective density matrix
  rho= ZOUTER_PROD(psi,psi)
  CALL DEBUG(deb,rho,ABS(ZTRACE(rho)-1).GE.1d-5,&
       "Density matrix has trace neq 1!")
!!$  --------------------------------------------------------


  !TRACING OUT ON ONE INDEX
!!$  --------------------------------------------------------
  out_idx=1
  ALLOCATE(red_rho(dd**(NN-1),dd**(NN-1)))
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  CALL WRITE_MAT("random_rho.dat",red_rho)
!!$  --------------------------------------------------------


  !AN EXAMPLE FOR CHECKING IF THIS THING WORKS
!!$  --------------------------------------------------------
  out_idx=1 !corresponding to the left subsystem
  rho=reshape((/ &
       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),&
       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),&
       CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),&
       CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0)&
       /), shape(rho))

  PRINT*, "FULL DENSITY MATRIX:"
  DO ii=1,SIZE(rho,1)
     WRITE(*,*) rho(ii,:)
  END DO
  
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX FOR THE FIRST SUBSYSTEM"
  DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO
  
  out_idx=2
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX FOR THE SECOND SUBSYSTEM"
  DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO
!!$  --------------------------------------------------------
  

  
  !TRYING SOME OTHER STUFF
!!$  --------------------------------------------------------
  !deallocating and reallocating all the needed variables
  DEALLOCATE(rho,red_rho)
  NN=2
  dd=3
  ALLOCATE(r1(dd,dd))
  ALLOCATE(r2(dd,dd))
  ALLOCATE(r3(dd,dd))
  ALLOCATE(rho(dd**NN,dd**NN))
  ALLOCATE(red_rho(dd*(NN-1),dd**(NN-1)))
  !defining block matrices
  r1=reshape((/ &
       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),CMPLX(0d0,0d0),&
       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),CMPLX(0d0,0d0),&
       CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0)&
       /), shape(r1))
  r2=reshape((/ &
       CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),&
       CMPLX(0d0,0d0),CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),&
       CMPLX(0d0,0d0),CMPLX(5d-1,0d0),CMPLX(5d-1,0d0)&
       /), shape(r2))
!!$  r3=reshape((/ &
!!$       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),CMPLX(0d0,0d0),&
!!$       CMPLX(0d0,0d0),CMPLX(0d0,0d0),CMPLX(0d0,0d0),&
!!$       CMPLX(0d0,0d0),CMPLX(5d-1,0d0),CMPLX(5d-1,0d0)&
!!$       /), shape(r3))
  !some separation in the output...
  PRINT*, ""
  PRINT*, "NEW TRIAL: NN=2, dd=3"
  PRINT*, "" 
  !defining the density matrix as a kronecker product 
  rho=ZKRONECKER( r1,r2 )
  PRINT*, "FULL DENSITY MATRIX"
  DO ii=1,SIZE(rho,1)
     WRITE(*,*) rho(ii,:)
  END DO
  CALL DEBUG(deb,rho,ABS(ZTRACE(rho)-1).GE.1d-10,&
       "RHO HAS NOT UNITARY TRACE!")
  !removing the first index
  out_idx=1
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX FOR THE FIRST SUBSYSTEM"
  DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO
  !removing the second index
  out_idx=2
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX FOR THE SECOND SUBSYSTEM"
  DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO  
!!$  --------------------------------------------------------



  !YET ANOTHER TRIAL
!!$  --------------------------------------------------------
  !deallocating and reallocating all the needed variables
  DEALLOCATE(rho,red_rho,r1,r2,r3)
  NN=3
  dd=2
  ALLOCATE(r1(dd,dd))
  ALLOCATE(r2(dd,dd))
  ALLOCATE(r3(dd,dd))
  ALLOCATE(rho(dd**NN,dd**NN))
  ALLOCATE(red_rho(dd*(NN-1),dd**(NN-1)))
  !defining block matrices
  r1=reshape((/ &
       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0),&
       CMPLX(5d-1,0d0),CMPLX(5d-1,0d0)&
       /), shape(r1))
  r2=reshape((/ &
       CMPLX(1d0,0d0),CMPLX(0d0,0d0),&
       CMPLX(0d0,0d0),CMPLX(0d0,0d0)&
       /), shape(r2))
  r3=reshape((/ &
       CMPLX(0d0,0d0),CMPLX(0d0,0d0),&
       CMPLX(0d0,0d0),CMPLX(1d0,0d0)&
       /), shape(r3)) 
  PRINT*, ""
  PRINT*, "NEW TRIAL: NN=3, dd=2"
  PRINT*, "" 
  !defining the density matrix as a kronecker product 
  rho=ZKRONECKER( r1, ZKRONECKER( r2,r3 ))
  PRINT*, "FULL DENSITY MATRIX"
  DO ii=1,SIZE(rho,1)
     WRITE(*,*) rho(ii,:)
  END DO
  CALL DEBUG(deb,rho,ABS(ZTRACE(rho)-1).GE.1d-10,&
       "RHO HAS NOT UNITARY TRACE!")
  !removing the first subsystem
  out_idx=1
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX: REMOVED THE FIRST SUBSYSTEM"
  DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO
  !removing the second subsystem
  out_idx=2
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX: REMOVED THE SECOND SUBSYSTEM"
  DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO
  !removing the third subsystem
  out_idx=3
  red_rho=TRACE_OUT(out_idx,rho,dd,NN)
  CALL DEBUG(deb,red_rho,ABS(ZTRACE(red_rho)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX: REMOVED THE THIRD SUBSYSTEM"
   DO ii=1,SIZE(red_rho,1)
     WRITE(*,*) red_rho(ii,:)
  END DO
  !removing also the second subsystem? It works!
  out_idx=2
  r1=TRACE_OUT(out_idx,red_rho,dd,NN-1)
  CALL DEBUG(deb,r1,ABS(ZTRACE(r1)-1).GE.1d-5,&
       "Reduced trace is not normalized!")
  PRINT*, "REDUCED DENSITY MATRIX: REMOVED ALSO THE SECOND SUBSYSTEM"
   DO ii=1,SIZE(r1,1)
     WRITE(*,*) r1(ii,:)
  END DO
 
!!$  --------------------------------------------------------

  
  
  
END PROGRAM MAIN_2


  
