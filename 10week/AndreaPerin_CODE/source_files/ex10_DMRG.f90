PROGRAM DMRG
  USE DEBUGGER
  USE UTILS


  IMPLICIT NONE


!!$  --------------------------------------------------------
  ! input management parameters
  INTEGER*4, PARAMETER :: num_real_params=2
  REAL*8, DIMENSION(num_real_params) :: realparams
  
  !physical quantities
  INTEGER*4 :: NN, numrep=10, mm, dd
  INTEGER*4, DIMENSION(:), ALLOCATABLE :: diag
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: Hsmall,Hbig,proj,egvecs,&
       LL,RR,hr,rho,rho_left
  REAL*8, DIMENSION(:), ALLOCATABLE :: populations,egvals
  REAL*8, DIMENSION(2,2), PARAMETER ::&
       id = (reshape((/ &
             1d0, 0d0, 0d0, 1d0/), shape(id))),&
       sigx = reshape((/ &
             0d0, 1d0, 1d0, 0d0/), shape(sigx)),&
       sigz = reshape((/ &
             1d0, 0d0, -1d0, 0d0/), shape(sigz))
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
  ! creating a dictionary for real variables
  EQUIVALENCE (realparams(1), lambda)
  lambda=0.5d0
  NN=10
  mm=2**(NN/2-1)
  ! getting command line parameters
  DO ii=1,COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) realparams(ii)
     END IF
  END DO
!!$  --------------------------------------------------------

  
  !CREATING THE TOTAL HAMILTONIAN
!!$  --------------------------------------------------------
  !allocating
  dd=2**(NN/2)
  ALLOCATE(hbig(2**NN,2**NN)) !for the whole system
  ALLOCATE(hsmall(dd,dd)) !for just one half of the system
  ALLOCATE(LL(dd,dd)) !the interaction part for the left subsystem
  ALLOCATE(RR(dd,dd)) !the interaction part for the right subsystem
  ALLOCATE(hr(2**NN,2**NN)) !a copy of the total hamiltonian
  ALLOCATE(rho(2**NN,2**NN)) !density matrix for the whole system
  ALLOCATE(rho_left(dd,dd)) !reduced density matrix
  ALLOCATE(diag(dd)) !for the diagonal part of the subsystem hamiltonian
  ALLOCATE(populations(dd)) !eigvals of the reduced density matrix
  ALLOCATE(egvals(2**NN)) !eigvals of the reduced density matrix
  ALLOCATE(egvecs(dd,dd)) !eigvecs of the whole system
  ALLOCATE(proj(dd,mm)) !the projector from the reduced density matrix
!!$  --------------------------------------------------------


  ! CONSTRUCTING THE DIAG
!!$  --------------------------------------------------------
  !creating the diagonal part of the hamiltonian;
  !These diagonal terms, which come from the non-interacting part
  !of the hamiltonian, are computed one at a time. The formula is
  !derived from the matrix element expression.
  diag=NN/2
  DO ii=0,dd-1
     DO jj=0,(NN/2)-1
        diag(ii+1)=diag(ii+1)-2*MOD(ii/(2**jj),2)
     END DO
  END DO
!!$  --------------------------------------------------------

  
  ! CONSTRUCTING THE INTERACTION PART
!!$  --------------------------------------------------------
  !creating the interaction part of the hamiltonian
  !Also this part comes from direct computation.
  DO ii=0,dd-1
     DO jj=0,dd-1
        hsmall(jj+1,ii+1)=XOR(jj,ii,NN/2)
     END DO
  END DO
!!$  --------------------------------------------------------

 
  !COMPOSING INTO A SINGLE MATRIX
!!$  --------------------------------------------------------
  !Joining the diagonal and the off-diagonal parts
  DO ii=1,dd
     hsmall(ii,ii)=hsmall(ii,ii)+lambda*diag(ii)
  END DO
!!$  --------------------------------------------------------

  
  !WRITING THE GLOBAL HAMILTONIAN FOR THE WHOLE SYSTEM
!!$  --------------------------------------------------------
  ! the interaction part of the 2N sites hamiltonian
  LL=DKRONECKER(DEYE(dd/2),sigx)
  RR=DKRONECKER(sigx,DEYE(dd/2))
  ! the non-interaction part of the 2N sites hamiltonian
  hbig=DKRONECKER(hsmall,DEYE(dd))+&
       DKRONECKER(DEYE(dd),hsmall)+&
       DKRONECKER(LL,RR)
!!$  --------------------------------------------------------

  
  !MAIN LOOP
!!$  --------------------------------------------------------  
  DO ii=1,numrep
     !diagonalizing the global hamiltonian
     CALL DGET_EGV(hbig,egvals,info)
     CALL DEBUG(deb,info,info.NE.0,"FAILED DIAG")
     !computing the density matrix of the ground state
     rho=DOUTER_PROD(hbig(:,1),hbig(:,1))
     !computing the reduced density matrix
     rho_left=DPARTIAL_TRACE(rho,'R',dd)
     !diagonalizing the partial trace
     CALL DGET_EGV(rho_left,populations,info)
     CALL DEBUG(deb,info,info.NE.0,"FAILED DIAG")
     !keeping only the largest mm eigenvectors
     proj=rho_left(:,SIZE(proj)-mm+1:SIZE(proj))
     !computing the projections of the various pieces
     hsmall=PROJECT(proj,hsmall)
     LL=PROJECT(proj,LL)
     RR=PROJECT(proj,RR)
     !recomposing the new hbig
     hbig=DKRONECKER(hsmall,DEYE(dd))+&
       DKRONECKER(DEYE(dd),hsmall)+&
       DKRONECKER(LL,RR)
  END DO
!!$  --------------------------------------------------------  
     
  !GETTING THE GROUND STATE
!!$  --------------------------------------------------------
  CALL DGET_EGV(hbig,egvals,info)
  CALL DEBUG(deb,info,info.NE.0,"FAILED DIAG")
  PRINT*, lambda, egvals(1)/(NN+numrep*2)
!!$  --------------------------------------------------------

     
END PROGRAM DMRG
