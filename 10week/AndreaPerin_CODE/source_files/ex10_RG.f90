PROGRAM RG
  USE DEBUGGER
  USE UTILS


  IMPLICIT NONE


!!$  --------------------------------------------------------
  ! input management parameters
  INTEGER*4, PARAMETER :: num_real_params=1, num_int_params=1
  REAL*8, DIMENSION(num_real_params) :: realparams
  INTEGER*4, DIMENSION(num_int_params) :: intparams
  
  !physical quantities
  INTEGER*4 :: NN=2, numrep=100
  INTEGER*4, DIMENSION(:), ALLOCATABLE :: diag
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: Hsub,Htot,proj,egvecs,LL,RR,hr
  REAL*8, DIMENSION(:), ALLOCATABLE :: egvals
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
  ! creating a dictionary for variables
  EQUIVALENCE (realparams(1), lambda)
  EQUIVALENCE (intparams(1), NN)
  lambda=0.5d0
  NN=2
  ! getting command line parameters
  DO ii=1,num_real_params
     CALL GET_COMMAND_ARGUMENT(ii, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) realparams(ii)
     END IF
  END DO
  DO ii=1,num_int_params
     CALL GET_COMMAND_ARGUMENT(ii+num_real_params, tmp_arg)
     IF(LEN_TRIM(tmp_arg).NE.0)THEN
        READ(tmp_arg,*) intparams(ii)
     END IF
  END DO
!!$  --------------------------------------------------------

  
  !CREATING THE TOTAL HAMILTONIAN
!!$  --------------------------------------------------------
  !allocating
  ALLOCATE(hsub(2**NN,2**NN))
  ALLOCATE(LL(2**NN,2**NN))
  ALLOCATE(RR(2**NN,2**NN))
  ALLOCATE(htot(2**(2*NN),2**(2*NN)))
  ALLOCATE(hr(2**(2*NN),2**(2*NN)))
  ALLOCATE(diag(2**NN))
  ALLOCATE(egvals(2**(2*NN)))
  ALLOCATE(egvecs(2**(2*NN),2**(2*NN)))
  ALLOCATE(proj(2**(2*NN),2**NN))
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
        hsub(jj+1,ii+1)=XOR(jj,ii,NN)
     END DO
  END DO
!!$  --------------------------------------------------------

 
  !COMPOSING INTO A SINGLE MATRIX
!!$  --------------------------------------------------------
  !Joining the diagonal and the off-diagonal parts
  DO ii=1,2**NN
     hsub(ii,ii)=hsub(ii,ii)+lambda*diag(ii)
  END DO
!!$  --------------------------------------------------------

  
  !CREATING THE HAMILTONIAN FOR 2N SITES
!!$  --------------------------------------------------------  
  ! the interaction part of the 2N sites hamiltonian
  LL=DKRONECKER(DEYE(2**(NN-1)),sigx)
  RR=DKRONECKER(sigx,DEYE(2**(NN-1)))
  ! the non-interaction part of the 2N sites hamiltonian
  htot=DKRONECKER(hsub,DEYE(2**NN))+&
       DKRONECKER(DEYE(2**NN),hsub)+&
       DKRONECKER(LL,RR)
!!$  --------------------------------------------------------

  
  !MAIN LOOP
!!$  --------------------------------------------------------  
  DO ii=1,numrep
     hr=htot
     CALL DGET_EGV(htot,egvals,info)
     CALL DEBUG(deb,info,info.NE.0,"FAILED DIAG")
     egvecs=htot
     htot=hr
     CALL DEBUG(deb,info,SIZE(htot,1).NE.2**(2*NN),"Wrong dims")
     CALL DEBUG(deb,info,SIZE(htot,2).NE.2**(2*NN),"Wrong dims")
     !building the projector
     proj=egvecs(:,1:2**NN)
     !projecting the hamiltonian, getting the new hsub
     hsub=PROJECT(proj,htot)
     CALL DEBUG(deb,info,SIZE(hsub,1).NE.2**NN,"Wrong dims")
     CALL DEBUG(deb,info,SIZE(hsub,2).NE.2**NN,"Wrong dims")
     !projecting the interactions
     LL=PROJECT(proj,DKRONECKER(DEYE(2**NN),LL))
     CALL DEBUG(deb,info,SIZE(LL,1).NE.2**NN,"Wrong dims")
     CALL DEBUG(deb,info,SIZE(LL,2).NE.2**NN,"Wrong dims")
     RR=PROJECT(proj,DKRONECKER(RR,DEYE(2**NN)))
     CALL DEBUG(deb,info,SIZE(RR,1).NE.2**NN,"Wrong dims")
     CALL DEBUG(deb,info,SIZE(RR,2).NE.2**NN,"Wrong dims")
     !total hamiltonian
     htot=DKRONECKER(hsub,DEYE(2**NN))+&
       DKRONECKER(DEYE(2**NN),hsub)+&
       DKRONECKER(LL,RR)
     CALL DEBUG(deb,info,SIZE(htot,1).NE.2**(2*NN),"Wrong dims")
     CALL DEBUG(deb,info,SIZE(htot,2).NE.2**(2*NN),"Wrong dims")
  END DO
!!$  --------------------------------------------------------

  
  !GETTING THE GROUND STATE
!!$  --------------------------------------------------------
  CALL DGET_EGV(htot,egvals,info)
  CALL DEBUG(deb,info,info.NE.0,"FAILED DIAG")
  PRINT*, lambda, egvals(1)/(REAL(NN)*(2.0**(numrep+1)))
!!$  --------------------------------------------------------

END PROGRAM RG
