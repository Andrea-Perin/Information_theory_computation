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
    UPLO='U'
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
    rr=SUM(all_r(2:))/(SIZE(all_r)-1)
    RETURN
  END FUNCTION R_CALC
  
    
END MODULE RANDOM_MATRIX


PROGRAM R_CALCULATE
  USE RANDOM_MATRIX
  IMPLICIT NONE
  
  INTEGER*4, PARAMETER :: col_size=1999, num_w=6, trials=60
  CHARACTER(LEN=100) :: filename
  REAL*4, DIMENSION(col_size*trials,num_w) :: delta_lambdas
  REAL*4, DIMENSION(trials,num_w) :: avg_r
  INTEGER*4 :: ii,jj

  CALL GET_COMMAND_ARGUMENT(1, filename)

  OPEN(15,FILE=TRIM(filename))
  READ(15,*) delta_lambdas
  CLOSE(15)

  DO ii=1,trials
     DO jj=1,num_w
        avg_r(ii,jj)=R_CALC(delta_lambdas(&
             (ii-1)*col_size:ii*col_size,jj))
     END DO
     WRITE(*,*) avg_r(ii,:) 
  END DO

END PROGRAM R_CALCULATE
