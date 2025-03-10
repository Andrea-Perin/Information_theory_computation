MODULE RANDOM_MATRIX
  IMPLICIT NONE
  
!!$  DESCRIPTION OF THE MODULE CONTENTS
!!$  This module contains some utilities for the creation of random normal
!!$  hermitian matrices. For instance, the generation of independent
!!$  random gaussian variables of null mean and unitary variance, the
!!$  generation of random hermitian matrices, and the computation of
!!$  the (locally or globally) normalized spacings of eigenvalues of
!!$  hermitian matrices.

CONTAINS

  SUBROUTINE INIT_RANDOM_SEED()
!!$  DESCRIPTION:
!!$  This subroutine initializes the random seed based on the system
!!$  time.
!!$  ARGUMENTS: NO
     
    INTEGER*4 :: ii, nn, clock
    INTEGER*4, DIMENSION(:), ALLOCATABLE :: seed
    
    CALL RANDOM_SEED(size = nn)
    ALLOCATE(seed(nn))
    CALL SYSTEM_CLOCK(COUNT=clock)
    seed = clock+37*(/ (ii-1, ii=1,nn) /)
    CALL RANDOM_SEED(PUT=seed)
    DEALLOCATE(seed)
  END SUBROUTINE

  
  FUNCTION BOX_MULLER(val_a,val_b) RESULT(res_gauss) 
!!$  DESCRIPTION:
!!$  Given two real*4 numbers, val_a and val_b, the function BOX_MULLER
!!$    performs a Box-Muller transformation. If the arguments, as it should
!!$    be, are uniform random in the interval [0,1], the result of this
!!$    function is a pair of independent gaussian variables, with mean
!!$    zero and variance one.
!!$    ARGUMENTS:
!!$    - val_a, val_b : two REAL*4 numbers. Ideally, both ~U([0,1])
!!$    RETURNS:
!!$    - res_gauss    : an array of dimension 2, containing a pair of
!!$                     numbers obtained from the Box-Muller transformation.

    REAL*4, INTENT(IN) :: val_a,val_b
    REAL*4 :: pi
    REAL*4, DIMENSION(2) :: res_gauss
    pi=ACOS(-1.e0)
    res_gauss(1)=SQRT(-2*LOG(val_a))*COS(2*pi*val_b)
    res_gauss(2)=SQRT(-2*LOG(val_a))*SIN(2*pi*val_b)
    RETURN  
  END FUNCTION BOX_MULLER

  
  FUNCTION NORM_AVERAGE(vector) RESULT(mean)
    REAL*4, DIMENSION(:) :: vector
    REAL*4, DIMENSION(SIZE(vector)) :: mean
    mean = vector/(SUM(vector)/SIZE(vector))
    RETURN
  END FUNCTION NORM_AVERAGE

  
  FUNCTION DIFF(vector)
    REAL*4, DIMENSION(:) :: vector
    REAL*4, DIMENSION(SIZE(vector)-1) :: diff
    INTEGER*4 :: ii
    DO ii=1,SIZE(diff)
       diff(ii)=vector(ii+1)-vector(ii)
    END DO
    RETURN
  END FUNCTION DIFF


  FUNCTION LOCAL_AVERAGE(vector,window) RESULT(loc_avg)
    REAL*4, DIMENSION(:) :: vector
    REAL*4, DIMENSION(SIZE(vector)) :: loc_avg
    INTEGER*4 :: ii, window, vecsize

    vecsize=SIZE(vector)
    DO ii=1,vecsize
       loc_avg(ii)=SUM(vector(MAX(1,ii-window):MIN(ii+window,vecsize)))
       loc_avg(ii)=loc_avg(ii)/(MIN(ii+window,vecsize)-MAX(1,ii-window))
    END DO
    RETURN
  END FUNCTION LOCAL_AVERAGE
  
    
  SUBROUTINE GET_EIGVALS(matr,eigvals,INFO)
    CHARACTER*1 :: JOBZ,UPLO
    INTEGER*4 :: N,LDA,LWORK,INFO
    COMPLEX*8, DIMENSION(:,:) :: matr
    REAL*4, DIMENSION(SIZE(matr,1)) :: eigvals
    REAL*4, DIMENSION(:), ALLOCATABLE :: RWORK
    COMPLEX*8, DIMENSION(:), ALLOCATABLE :: WORK
    ! only eigenvalues
    JOBZ='N'
    UPLO='V'
    N=SIZE(matr,1)
    LDA=N
    LWORK=2*N-1
    ALLOCATE(RWORK(3*N-2))
    ALLOCATE(WORK(LWORK))
    CALL CHEEV(JOBZ,UPLO,N,matr,LDA,eigvals,WORK,LWORK,RWORK,INFO)
    DEALLOCATE(RWORK,WORK)
    RETURN
  END SUBROUTINE GET_EIGVALS
  
  
  FUNCTION RAND_INIT(nn,diag) RESULT(rand_matr)
!!$    DESCRIPTION:
!!$    This function is used to create a random hermitian matrix of size nn.
!!$    If diag is set to True, then a diagonal real random matrix is
!!$    returned.
!!$      ARGUMENTS:
!!$      - nn          : an INTEGER*4, containing the size of the matrix
!!$      - diag        : (OPT, default=False) a LOGICAL variable. If set to
!!$                      True, a diagonal hermitian is returned.
!!$      RETURNS:
!!$      - rand_matr   : a COMPLEX*8 matrix of dimensions (n,n). It is
!!$                      hermitian.

    INTEGER*4 :: nn, ii, jj
    COMPLEX*8, DIMENSION(nn,nn) :: rand_matr
    REAL*4 :: repart, impart
    REAL*4, DIMENSION(2) :: gauss_rnd
    LOGICAL, OPTIONAL :: diag

    CALL INIT_RANDOM_SEED()
    IF (PRESENT(diag).AND.(diag)) THEN
       rand_matr=CMPLX(0.d0)
       DO jj=1,nn
          CALL RANDOM_NUMBER(repart)
          CALL RANDOM_NUMBER(impart)
          gauss_rnd = BOX_MULLER(repart,impart)
          rand_matr(jj,jj)=gauss_rnd(1)
       END DO       
    ELSE
       DO jj=1,nn
          DO ii=jj+1,nn
             CALL RANDOM_NUMBER(repart)
             CALL RANDOM_NUMBER(impart)
             gauss_rnd= BOX_MULLER(repart,impart)
             rand_matr(ii,jj)=CMPLX(gauss_rnd(1),gauss_rnd(2))
             rand_matr(jj,ii)=CONJG(rand_matr(ii,jj))
          END DO
          CALL RANDOM_NUMBER(repart)
          CALL RANDOM_NUMBER(impart)
          gauss_rnd= BOX_MULLER(repart,impart)
          rand_matr(jj,jj)=CMPLX(gauss_rnd(1))   
       END DO
    END IF
    RETURN
  END FUNCTION RAND_INIT

  FUNCTION R_CALC(vector) RESULT(rr)
    REAL*4, DIMENSION(:) :: vector
    REAL*4, DIMENSION(SIZE(vector)-1) :: all_r
    REAL*4 :: rr
    INTEGER*4 :: ii

    DO ii=1,SIZE(all_r)
       all_r(ii)=MIN(vector(ii),vector(ii+1))/&
            MAX(vector(ii),vector(ii+1))
    END DO
    rr=SUM(all_r)/SIZE(all_r)
    RETURN
  END FUNCTION R_CALC
  
    
END MODULE RANDOM_MATRIX





PROGRAM SPACING
  USE RANDOM_MATRIX
  IMPLICIT NONE

  INTEGER*4, PARAMETER :: matsize=2000, num_trials=60, wind=5
  REAL*4, DIMENSION(:), ALLOCATABLE :: egvls, spacin, norm_spacin
  REAL*4, DIMENSION(:,:), ALLOCATABLE :: loc_spacin
  COMPLEX*8, DIMENSION(:,:), ALLOCATABLE :: hermat
  INTEGER*4 ::  ii, jj, count_args, INFO, kk
  INTEGER*4, DIMENSION(wind) :: window
  CHARACTER :: is_diag
  LOGICAL :: en_diag
  
  ! initializing the windows
  window = (/ 100 , 50 , 10 , 5 , 1 /)
  
  ! allocating
  ALLOCATE(hermat(matsize,matsize))
  ALLOCATE(egvls(matsize))
  ALLOCATE(spacin(matsize-1))
  ALLOCATE(norm_spacin(matsize-1))
  ALLOCATE(loc_spacin(matsize-1,wind))
  
  ! deciding the kind of matrix
  count_args = COMMAND_ARGUMENT_COUNT()
  CALL GET_COMMAND_ARGUMENT(1,is_diag)
  en_diag=.TRUE.
  IF ((count_args.EQ.0).OR.(is_diag.NE.'D')) THEN
    en_diag=.FALSE.
  END IF

  DO ii=1,num_trials

     ! repeating the diagonalization until we get a right result
     INFO=1
     DO WHILE(INFO.NE.0)
        ! initializing a random matrix 
        hermat=RAND_INIT(matsize,en_diag)
        ! computing the eigenvalues
        CALL GET_EIGVALS(hermat,egvls,INFO)
     END DO
     ! if here, the eigvals were properly computed
     ! computing the spacings
     spacin=DIFF(egvls)
     ! computing the norm. average of the spacings
     norm_spacin=NORM_AVERAGE(spacin)
     ! computing local avgs for the different windows
     DO jj=1,wind
        loc_spacin(:,jj)=LOCAL_AVERAGE(spacin,matsize/window(jj))
     END DO
     ! printing everything on screen, col by col
     DO jj=1,matsize-1
        WRITE(*,*) norm_spacin(jj), spacin(jj)/loc_spacin(jj,4), &
             spacin(jj), (loc_spacin(jj,kk), kk=1,wind)
     END DO
  END DO
  ! deallocating
  DEALLOCATE(hermat,egvls,spacin,norm_spacin,loc_spacin) 
  
END PROGRAM SPACING
