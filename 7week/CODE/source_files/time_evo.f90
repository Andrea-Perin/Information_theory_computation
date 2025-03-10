PROGRAM TRY
  USE LATTICE1D
  USE SOLVE_SCHR
  USE DEBUGGER

  USE, INTRINSIC :: iso_c_binding
  USE FFTW3
  
  IMPLICIT NONE

  
  ! VARIABLES DECLARATION 
!!$  -------------------------------------------------------- 
  ! variables for input management
  INTEGER*4, PARAMETER :: num_intparams=3,num_realparams=4
  INTEGER*4, DIMENSION(num_intparams) :: intparams
  REAL*8, DIMENSION(num_realparams) :: realparams
  
  ! physical variables
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: T,V,H,egvecs
  COMPLEX*16, DIMENSION(:), ALLOCATABLE :: evoperatorV,evoperatorT
  REAL*8, DIMENSION(:), ALLOCATABLE :: grid, egvals, pgrid
  INTEGER*4 :: NN, kk, num_tsteps
  REAL*8 :: omega, TT, x_low, x_high, step, time_inst, center, tstep
  
  ! history logs
  COMPLEX*16, DIMENSION(:,:,:), ALLOCATABLE :: wave_evo
  REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: prob_evo
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: pot_evo
  
  ! utility variables
  INTEGER*4 :: ii, jj, INFO
  CHARACTER*10 :: tmp_arg
  CHARACTER*3 :: int_to_str

!!$  --------------------------------------------------------

  
  ! DEBUG FLAG
!!$  --------------------------------------------------------
  LOGICAL :: deb = .TRUE.
!!$  --------------------------------------------------------
  
  
  ! INPUT PARAMETERS MANAGEMENT
!!$  --------------------------------------------------------
  ! creating a dictionary for int variables
  EQUIVALENCE (intparams(1), NN)
  EQUIVALENCE (intparams(2), kk)
  EQUIVALENCE (intparams(3), num_tsteps)

  ! creating a dictionary for real variables
  EQUIVALENCE (realparams(1), x_low)
  EQUIVALENCE (realparams(2), x_high)
  EQUIVALENCE (realparams(3), omega)
  EQUIVALENCE (realparams(4), TT)

  !integers
  NN=200
  kk=1
  num_tsteps=1000

  !real
  x_low=-10.D0
  x_high=10.D0
  omega=0.5D0
  TT=3D0
  
  ! getting command line parameters
  DO ii=1, COMMAND_ARGUMENT_COUNT()-num_realparams
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) intparams(ii)
     END IF
  END DO
  DO ii=num_intparams+1, COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) realparams(ii-num_intparams)
     END IF
  END DO

!!$  --------------------------------------------------------



  ! CREATING THE INTIAL CONDITION
!!$  --------------------------------------------------------

  ! allocating the variables
  ALLOCATE(grid(NN))
  ALLOCATE(pgrid(NN))
  ALLOCATE(H(NN,NN))
  ALLOCATE(egvals(NN))
  ALLOCATE(evoperatorV(NN))
  ALLOCATE(evoperatorT(NN))
  ALLOCATE(egvecs(NN,kk))
  ALLOCATE(wave_evo(NN,kk,num_tsteps+1))
  ALLOCATE(prob_evo(NN,kk,num_tsteps+1))
  ALLOCATE(pot_evo(NN,num_tsteps+1))
  
  ! creating the grid (and the reciprocal grid)
  grid = LINSPACE(NN,x_low,x_high)
  step = grid(2)-grid(1)
  pgrid= LINSPACE(NN, 0.d0, (2*pi)/step*(NN-1)/NN )
  pgrid(NN/2:)=pgrid(NN/2:)-pgrid(NN)-pgrid(2)
  
  ! creating the laplacian and the potential
  CALL LAPLACIAN(NN, T, is_pbc=.FALSE.)
  CALL HARM_POTENTIAL(NN, V, omega, x_low, x_high)

  ! writing the hamiltonian
  H=-(1/step**2)*T/2+V/2
  DEALLOCATE(V,T)
  
  ! getting the eigenvalues and eigenvectors
  CALL GET_K_EGV(H,kk,egvals,egvecs,INFO)
  DEALLOCATE(H)
  
  !checking if successful diagonalization
  IF(INFO.NE.0)THEN
     PRINT*, 'ERROR: FAILED DIAGONALIZATION!'
  ELSE     
     ! if successful, normalizing the eigenvectors
     egvecs=egvecs/SQRT(step)
  END IF

  !saving the initial condition
  CALL COMPUTE_PROBABILITIES(egvecs,prob_evo(:,:,1))
  wave_evo(:,:,1)=egvecs
  pot_evo(:,1)=0D0
     
!!$  --------------------------------------
  

  
  ! DESCRIBING THE EVOLUTION
!!$  --------------------------------------------------------

  ! checking if the inital norm is equal to 1
  CALL DEBUG(deb,egvecs(:,1),ABS(SUM(ABS(egvecs(:,1)*SQRT(step))**2)-1)&
       .GE.1d-10,'no norma iniziale')
  
  !defining the tstep
  tstep=TT/num_tsteps

  !computin the kinetic operator (in p representation) once
  evoperatorT=EXP( -(tstep*0.5)*CMPLX(0.D0, (pgrid)**2) )

  ! performing the num_tsteps evolution steps
  DO ii=0,num_tsteps-1

     !computing the time instant
     time_inst=ii*tstep

     !computig the evolution operator
     center=time_inst
     evoperatorV=EXP(-(tstep*0.25)*CMPLX(0.D0,(omega**2)*DHP(grid,center)))
     
     !computing the effect of the operator on the state
     DO jj=1,kk
        egvecs(:,jj)=egvecs(:,jj)*evoperatorV
     END DO
     
     !transforming each eigenvector
     DO jj=1,kk
        egvecs(:,jj)=FT_1D(egvecs(:,jj))
     END DO

     !computing the effect of the kinetic operator on the state
     DO jj=1,kk
        egvecs(:,jj)=egvecs(:,jj)*evoperatorT
     END DO

     !transform back to x representation
     DO jj=1,kk
        egvecs(:,jj)=AFT_1D(egvecs(:,jj))
     END DO

     !reapplying the potential
     DO jj=1,kk
        egvecs(:,jj)=egvecs(:,jj)*evoperatorV
     END DO

     ! checking if the norm is preserved (up to some finite precision)
     WRITE(int_to_str,'(i3.3)') ii
     CALL DEBUG(deb,egvecs(:,1),ABS(SUM(ABS(egvecs(:,1)*SQRT(step))**2)&
          -1).GE.1d-4,&
          'no norm after '//TRIM(int_to_str)//' unitary(?) operator')
     
     !logging the results
     pot_evo(:,ii+1) = (omega**2)*DHP(grid,center)
     wave_evo(:,:,ii+1) = egvecs
     CALL COMPUTE_PROBABILITIES(egvecs,prob_evo(:,:,ii+1))
     
  END DO
!!$  --------------------------------------------------------

  
  ! SAVING ON FILE
!!$  --------------------------------------------------------  
  ! Saving the probabilities
  DO ii=1,kk
     !setting the index for the name
     WRITE(int_to_str,'(i3.3)') ii
     !printing the probabilities to file
     CALL WRITE_EIGVECS('probs_'//TRIM(int_to_str)//'.dat',&
          grid,prob_evo(:,ii,:))
  END DO

  ! Saving the real and imaginary part of each wave
  DO ii=1,kk
     !setting the index for the name
     WRITE(int_to_str,'(i3.3)') ii
     !printing the real part to file
     CALL WRITE_EIGVECS('real_'//TRIM(int_to_str)//'.dat',&
          grid,REALPART(wave_evo(:,ii,:)))
     !printing the img part to file
     CALL WRITE_EIGVECS('img_'//TRIM(int_to_str)//'.dat',&
          grid,IMAGPART(wave_evo(:,ii,:)))
  END DO

  ! Saving the potential, too
  CALL WRITE_EIGVECS('potential.dat',grid,pot_evo)
  
!!$  --------------------------------------------------------
  
END PROGRAM TRY

  
