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

  
  FUNCTION LOCAL_AVERAGE(vector,window) RESULT(mean)
!!$  DESCRIPTION:
!!$  Given a vector, this function creates a vector of differences, so that
!!$    the i-th element in the resulting vector is the difference between the
!!$    (i+1)-th and the i-th of the input vector. Then, if no window is
!!$    provided, it calculates the mean of the vector just created and
!!$    normalizes all its entries by that mean value.
!!$    If window is passed, after the vector of differences has been created,
!!$    a 'local average' is performed, such that the i-th entry is divided by
!!$    the mean of the elements in the interval
!!$    [MIN(i-window,1),MAX(i+window,SIZE(vector)]. 
!!$    ARGUMENTS:
!!$    - vector        : a REAL*4 vector of arbitrary size
!!$    - (opt) window  : an INTEGER*4, which is used to compute the interval
!!$                      over which to compute the local average. If no
!!$                      value is passed, the function performs the average
!!$                      over all elements. Setting window>=SIZE(vector) is
!!$                      equal to not setting it
!!$      RETURNS:
!!$      - mean        : a REAL*4 array, containing the computed results
    
    INTEGER*4 :: ii, jj, idx_window
    INTEGER*4, OPTIONAL :: window
    REAL*4 :: avg_delta
    REAL*4, DIMENSION(:) :: vector
    REAL*4, DIMENSION(SIZE(vector)-1) :: mean, mean_tmp
    DO ii=1,SIZE(mean)
       mean(ii)=vector(ii+1)-vector(ii)
    END DO
    IF (PRESENT(window)) THEN
       idx_window=window
       mean_tmp=mean
       DO ii=1,SIZE(MEAN)
!!$          avg_delta=0.d0
!!$          DO jj=MAX(1,ii-idx_window),MIN(ii+idx_window,SIZE(mean))
!!$             avg_delta = avg_delta+mean_tmp(jj)
!!$          END DO
          avg_delta=SUM(mean_tmp(MAX(1,ii-idx_window):&
               MIN(ii+idx_window,SIZE(mean))))
          avg_delta=avg_delta/MIN((2*idx_window+1),SIZE(mean))
          mean(ii)=mean(ii)/avg_delta
       END DO      
    ELSE
       avg_delta=SUM(mean)
       avg_delta=avg_delta/SIZE(mean)
       mean = mean/avg_delta
    END IF
    RETURN
  END FUNCTION LOCAL_AVERAGE

  
  FUNCTION GET_EIGVALS(matr) RESULT(eigvals)
!!$    DESCRIPTION:
!!$    A wrapper for the LAPACK subroutine cheev, which computes the
!!$      eigenvalues of a given hermitian matrix.
!!$      ARGUMENTS:
!!$      - matr     : a COMPLEX*8, hermitian matrix of dimension N
!!$      RETURNS:
!!$      - eigvals  : a REAL*4 array of dimension N

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
  END FUNCTION GET_EIGVALS

  
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

  
END MODULE RANDOM_MATRIX

