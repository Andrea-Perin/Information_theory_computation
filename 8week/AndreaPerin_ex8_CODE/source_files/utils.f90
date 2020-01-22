MODULE UTILS

  IMPLICIT NONE
  REAL*8, PARAMETER :: pi=ACOS(-1d0)

CONTAINS


  FUNCTION ZTRACE(mat)
!!$  --------------------------------------------------------
!!$    This func computes the trace of a given double
!!$    complex square matrix.
!!$    ARGUMENTS
!!$    - mat    : a COMPLEX*16 square matrix
!!$    RETURNS
!!$    - ztrace   : a COMPLEX*16 number
!!$  --------------------------------------------------------
    COMPLEX*16, DIMENSION(:,:) :: mat
    COMPLEX*16 :: ztrace
    INTEGER*4 :: ii

    IF (SIZE(mat,2).EQ.SIZE(mat,1)) THEN
       ztrace=0d0
       DO ii=1,SIZE(mat,1)
          ztrace=ztrace+mat(ii,ii)
       END DO
    ELSE
       PRINT*, "ERROR: MATRIX IS NOT SQUARE!"
    END IF
    RETURN
  END FUNCTION ZTRACE

  
  
  FUNCTION ZOUTER_PROD(vec1,vec2)RESULT(out)
!!$  --------------------------------------------------------
!!$    This func computes the complex outer product of a 
!!$    two given complex vectors. Copied by Numerical Recipes
!!$    in Fortran 90.
!!$    ARGUMENTS
!!$    - vec1    : a COMPLEX*16 vector, size a    
!!$    - vec2    : a COMPLEX*16 vector, size b
!!$    RETURNS
!!$    - out     : a COMPLEX*16 matrix, shape (a x b)
!!$  --------------------------------------------------------
    COMPLEX*16, DIMENSION(:) :: vec1,vec2
    COMPLEX*16, DIMENSION(SIZE(vec1),SIZE(vec2)) :: out
    INTEGER*4 :: ii,jj
    out=SPREAD(vec1,dim=2,ncopies=SIZE(vec2))*&
         SPREAD(CONJG(vec2),dim=1,ncopies=SIZE(vec1))
    RETURN
  END FUNCTION ZOUTER_PROD

  
  
  SUBROUTINE RNDINIT(state)
!!$  --------------------------------------------------------
!!$    This func randomly initializes a complex vector.
!!$    The distribution is U[0,1]. 
!!$    ARGUMENTS
!!$    -(I,O) state    : a COMPLEX*16 vector
!!$  --------------------------------------------------------
    INTEGER*4 :: ii,jj
    COMPLEX*16, DIMENSION(:) :: state
    REAL*8 :: repart,impart

    DO ii=1,SIZE(state)
       CALL RANDOM_NUMBER(repart)
       CALL RANDOM_NUMBER(impart)
       state(ii)=CMPLX(repart,impart)
    END DO
    RETURN
  END SUBROUTINE RNDINIT

  
  
  FUNCTION ZNORMALIZE(vec)
!!$  --------------------------------------------------------
!!$    This func returns the normalized version of the given
!!$    complex vector.
!!$    ARGUMENTS
!!$    - vec          : a COMPLEX*16 vector    
!!$    RETURNS
!!$    - znormalize   : a COMPLEX*16 vector
!!$  --------------------------------------------------------
    COMPLEX*16, DIMENSION(:) :: vec
    COMPLEX*16, DIMENSION(SIZE(vec)) :: znormalize

    znormalize=vec/SQRT(SUM(ABS(vec)**2))
    RETURN
  END FUNCTION ZNORMALIZE
  

  
  FUNCTION ZKRONECKER(mat1,mat2)
!!$  --------------------------------------------------------
!!$    This func computes the matrix-matrix Kronecker product,
!!$    the matrix representation of the tensor product, between
!!$    two complex*16 matrices.
!!$    ARGUMENTS
!!$    - mat1        : a COMPLEX*16 matrix, shape (a x b)    
!!$    - mat2        : a COMPLEX*16 matrix, shape (c x d)
!!$    RETURNS
!!$    - zkronecker  : a COMPLEX*16 matrix, shape (ac x bd)
!!$  --------------------------------------------------------
    COMPLEX*16, DIMENSION(:,:) :: mat1,mat2
    COMPLEX*16, DIMENSION(SIZE(mat1,1)*SIZE(mat2,1),&
         SIZE(mat1,2)*SIZE(mat2,2)) :: zkronecker
    INTEGER*4 :: ii,jj,rr,cc
    
    rr=SIZE(mat2,1)
    cc=SIZE(mat2,2)
    DO ii=1,SIZE(mat1,1)
       DO jj=1,SIZE(mat1,2)
          zkronecker((ii-1)*rr+1:ii*rr,(jj-1)*cc+1:jj*cc)=&
               mat1(ii,jj)*mat2
       END DO
    END DO
    RETURN
  END FUNCTION ZKRONECKER



  FUNCTION SEP_TO_STATE(mat)RESULT(state)
!!$  --------------------------------------------------------
!!$    This func maps the compressed representation of a
!!$    separable composed state (that is, an (ddxNN) matrix)
!!$    into its state (that is, a dd**NN long vector).
!!$    ARGUMENTS
!!$    - mat     : a COMPLEX*16 matrix, shape (dd x NN)
!!$    RETURNS
!!$    - state   : a COMPLEX*16 vector, shape dd**NN
!!$  --------------------------------------------------------
    COMPLEX*16, DIMENSION(:,:) :: mat
    COMPLEX*16, DIMENSION(SIZE(mat,1)**SIZE(mat,2)) :: state
    INTEGER*4 :: dd, NN, ii, jj
    INTEGER*4, DIMENSION(SIZE(mat,2)) :: div

    dd=SIZE(mat,1)
    NN=SIZE(mat,2)
    state=CMPLX(1d0)
    DO ii=1,NN
       div(ii)=dd**(NN-ii)
    END DO
    DO ii=1,dd**NN
       DO jj=1,NN
          state(ii)=state(ii)*mat( MOD((ii-1)/div(jj),dd)+1,jj )
       END DO
    END DO
    RETURN
  END FUNCTION SEP_TO_STATE
  

        
  FUNCTION TRACE_OUT(idx,mat,dd,NN)RESULT(red_dmat)
!!$  --------------------------------------------------------
!!$    This func traces out the (idx)-th subsystem, where
!!$    TOT=S_1 X S_2 X S_2 X ... X S_NN,
!!$    and returns a reduced density matrix.
!!$    ARGUMENTS
!!$    - idx    : an INTEGER*4 number, index of subsyst. to trace out
!!$    - mat    : a COMPLEX*16 matrix, shape (dd**NN) x (dd**NN)
!!$    - dd     : an INTEGER*4 number, dimension of Hilbert spaces
!!$    - NN     : an INTEGER*4 number, number of subsystems
!!$    RETURNS
!!$    - red_dmat   : a COMPLEX*16 matrix, shape dd**(NN-1) x dd**(NN-1)
!!$  --------------------------------------------------------
    INTEGER*4 :: idx, dd, NN, ii, jj, kk, jump, cnt
    COMPLEX*16, DIMENSION(:,:) :: mat
    INTEGER*4, DIMENSION(SIZE(mat,1)/dd) :: start_idx,range
    COMPLEX*16, DIMENSION(SIZE(mat,1)/dd,SIZE(mat,2)/dd) :: red_dmat

    cnt=0
    DO ii=0,dd**NN-1
       IF (MOD(ii/(dd**(NN-idx)),dd).EQ.0) THEN
          cnt=cnt+1
          start_idx(cnt)=ii
       END IF
    END DO
    start_idx=start_idx+1
    jump=dd**(NN-idx)
    red_dmat=0.d0
    DO ii=1,SIZE(mat,1)/dd
       DO jj=1,SIZE(mat,2)/dd
          DO kk=0,dd-1
             red_dmat(ii,jj)=red_dmat(ii,jj)+&
                  mat(start_idx(ii)+kk*jump,start_idx(jj)+kk*jump)
          END DO
       END DO
    END DO
    RETURN
  END FUNCTION TRACE_OUT


  
  SUBROUTINE WRITE_MAT(fname, mat) 
!!$  --------------------------------------------------------
!!$    This subroutine writes the contents of a matrix on file.
!!$    ARGUMENTS
!!$    - fname  : a CHARACTER(LEN=*), the name of the file
!!$    - mat    : a COMPLEX*16 matrix
!!$  --------------------------------------------------------
    CHARACTER(LEN=*) :: fname
    COMPLEX*16, DIMENSION(:,:) :: mat
    INTEGER*4 :: ii
    OPEN(12, file=fname, status="REPLACE", action="WRITE")
    DO ii=1,SIZE(mat,1)
       WRITE(12,*) mat(ii,:)
    END DO    
    RETURN       
  END SUBROUTINE WRITE_MAT
    

END MODULE UTILS
