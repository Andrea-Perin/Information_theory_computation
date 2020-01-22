MODULE SOLVE_SCHR

  ! BRIEF OUTLINE
!!$  --------------------------------------------------------
!!$  This module implements a series of functions and subroutines
!!$  for solving the 1D time-independent Schrodinger equation through
!!$  the finite difference method. 
!!$  --------------------------------------------------------


  IMPLICIT NONE
  
  ! PARAMETERS
!!$  --------------------------------------------------------
!!$  These values are used across the programs. The value of hbar
!!$  is set to 1,  but the true value is commented. Double precision
!!$  is employed.
!!$  --------------------------------------------------------
  REAL*8, PARAMETER :: pi=ACOS(-1.D0)

CONTAINS


  SUBROUTINE LAPLACIAN(N, matr, is_pbc)
!!$  --------------------------------------------------------
!!$  This subroutine computes the discrete laplacian for a 1D grid of
!!$  size N.
!!$  ARGUMENTS:
!!$  (I) N      : an INTEGER*4, the size of the grid
!!$  (O) matr   : a COMPLEX*16, the matrix that is to be returned
!!$  (I) is_pbc : (optional) whether to use periodic boundary conditions
!!$  --------------------------------------------------------

    COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: matr
    INTEGER*4 :: N,ii
    LOGICAL, OPTIONAL :: is_pbc
    
    ALLOCATE(matr(N,N))
    matr(1,1)=-2
    DO ii=2,N
       matr(ii,ii)=-2
       matr(ii-1,ii)=1
       matr(ii,ii-1)=1
    END DO

    ! setting boundary conditions if necessary
    IF(PRESENT(is_pbc).AND.(is_pbc))THEN
       matr(1,N)=1
       matr(N,1)=1
    END IF
    RETURN
  END SUBROUTINE LAPLACIAN


  SUBROUTINE HARM_POTENTIAL(N, matr, omega, x_low, x_high)
!!$  --------------------------------------------------------
!!$  This subroutine computes the 1D harmonic potential for a grid
!!$  of points, based on the value of omega.
!!$  ARGUMENTS:
!!$  (I) N      : an INTEGER*4, the size of the grid
!!$  (O) matr   : a COMPLEX*16 matrix, the matrix that is to be returned
!!$  (I) omega  : a REAL*8, the value of omega in the potential
!!$  (I) x_low  : a REAL*8, the lower value in the grid
!!$  (I) x_up   : a REAL*8, the upper value in the grid
!!$  --------------------------------------------------------
    
    COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: matr
    INTEGER*4 :: N,ii
    REAL*8 :: omega, x_low, x_high, step
    
    step=(x_high-x_low)/N
    ALLOCATE(matr(N,N))
    DO ii=1,N
       matr(ii,ii)=(omega*(x_low+step*(ii-1)))**2
    END DO
  END SUBROUTINE HARM_POTENTIAL


  SUBROUTINE GET_EGV(matr,egvals,info)
!!$  --------------------------------------------------------
!!$  This subroutine is a wrapper for the diagonalization
!!$  subroutine ZHEEV provided by LAPACK. 
!!$  ARGUMENTS:
!!$  (I/O) matr : a COMPLEX*16 matrix, the matrixto be diagonalized.
!!$               On return, it contains the eigenvectors.   
!!$  (O) egvals : a REAL*8 array, contains the eigenvalues
!!$  (O) info   : an INTEGER*4, communicating the status of
!!$               the diagonalization. If =0, all good.
!!$  --------------------------------------------------------

    COMPLEX*16, DIMENSION(:,:) :: matr
    REAL*8, DIMENSION(SIZE(matr,1)) :: egvals
    INTEGER*4 :: info
    ! utilities for zheev
    INTEGER*4 :: N,LWORK
    COMPLEX*16, DIMENSION(:), ALLOCATABLE :: WORK
    REAL*8, DIMENSION(:), ALLOCATABLE :: RWORK
    ! preparing for optimization
    LWORK=-1
    N=SIZE(matr,1)
    ALLOCATE(RWORK(3*N-2))
    ALLOCATE(WORK(1))
    ! querying optimal workspace
    CALL ZHEEV('V','U',N,matr,N,egvals,WORK,LWORK,RWORK,info)
    ! preparing to get the results
    LWORK=INT(WORK(1))
    DEALLOCATE(WORK)
    ALLOCATE(WORK(LWORK))
    CALL ZHEEV('V','U',N,matr,N,egvals,WORK,LWORK,RWORK,info)
    ! removing exhausted variables 
    DEALLOCATE(WORK,RWORK)
  END SUBROUTINE GET_EGV


  SUBROUTINE COMPUTE_PROBABILITIES(matr,probs)
!!$  --------------------------------------------------------
!!$  This subroutine computes the probabilities corresponding
!!$  to a given eigenvector.
!!$  ARGUMENTS:
!!$  (I) matr   : a COMPLEX*16 matrix, the matrix containing
!!$               the eigenvectors (by column!)   
!!$  (O) probs  : a REAL*8 matrix, its columns contain the
!!$               probabilities corresponding to the eigenvectors
!!$  --------------------------------------------------------
    COMPLEX*16, DIMENSION(:,:) :: matr
    REAL*8, DIMENSION(SIZE(matr,1),SIZE(matr,2)) :: probs
    INTEGER*4 :: ii

    DO ii=1,SIZE(matr,2)
       probs(:,ii)=ABS(matr(:,ii))**2
    END DO
    RETURN
  END SUBROUTINE COMPUTE_PROBABILITIES


  FUNCTION FACTORIAL(n)
!!$  --------------------------------------------------------
!!$  This function computes the factorial of the given input.
!!$  ARGUMENTS:
!!$  (I) n          : an INTEGER*8, the number of which to
!!$                   compute the factorial
!!$    RETURNS:
!!$  (O) factorial  : an INTEGER*8, the result
!!$  --------------------------------------------------------

    INTEGER*8 :: n,ii,factorial
    factorial=1
    DO ii=1,n
       factorial=factorial*ii
    END DO
    RETURN
  END FUNCTION FACTORIAL


  SUBROUTINE WRITE_EIGVALS(filename, array)
!!$  --------------------------------------------------------
!!$  This subroutine saves a 1D array on file. Useful for eigvals.
!!$  ARGUMENTS:
!!$  (I) filename  : a CHARACTER, of any length, the name of
!!$                  the output file
!!$  (I) array     : a REAL*8 array, that is to be saved
!!$  --------------------------------------------------------

    REAL*8, DIMENSION(:) :: array
    CHARACTER(LEN=*) :: filename
    INTEGER*4 :: ii
    
    OPEN(UNIT=43, FILE=filename, STATUS="replace", ACTION="write")
    
    DO ii=1,SIZE(array,1)
       WRITE(43,*) array(ii)
    END DO
    CLOSE(43)
    RETURN
  END SUBROUTINE WRITE_EIGVALS


  SUBROUTINE WRITE_EIGVECS(filename, xvals, array)
!!$  --------------------------------------------------------
!!$  This subroutine saves a 1D and a 2D array on file. Useful for
!!$  eigenvectors.
!!$  ARGUMENTS:
!!$  (I) filename  : a CHARACTER, of any length, the name of
!!$                  the output file
!!$  (I) xvals     : a REAL*8 array, that is to be saved as the
!!$                  first column 9usually, the x points in the grid
!!$  (I) array     : a REAL*8 matrix, which is stored column by column.
!!$                  Usually, it should be composed of eigenvector or
!!$                  other functions.
!!$  --------------------------------------------------------

    REAL*8, DIMENSION(:) :: xvals
    REAL*8, DIMENSION(:,:) :: array
    CHARACTER(LEN=*) :: filename
    INTEGER*4 :: ii, jj
    
    OPEN(UNIT=43, FILE=filename, STATUS="replace", ACTION="write")
    
    DO ii=1,SIZE(array,1)
       WRITE(43,*) xvals(ii), (array(ii,jj), jj=1,SIZE(array,2))
    END DO
    CLOSE(43)
    RETURN
  END SUBROUTINE WRITE_EIGVECS


  SUBROUTINE GET_K_EGV(matr,kk,egvals,egvecs,info)
!!$  --------------------------------------------------------
!!$  This subroutine is a wrapper for the diagonalization
!!$  subroutine ZHEEVX provided by LAPACK. 
!!$  ARGUMENTS:
!!$  (I) matr   : a COMPLEX*16 matrix, the matrix to be diagonalized.  
!!$  (I) kk     : an INTEGER*4, the number of eigvals/eigvecs to return 
!!$  (O) egvals : a REAL*8 array, contains the first kk eigenvalues
!!$  (O) egvecs : a COMPLEX*16 matrix, contains the first kk eigvectors
!!$  (O) info   : an INTEGER*4, communicating the status of
!!$               the diagonalization. If =0, all good.
!!$  --------------------------------------------------------

    COMPLEX*16, DIMENSION(:,:) :: matr
    REAL*8, DIMENSION(SIZE(matr,1)) :: egvals
    COMPLEX*16, DIMENSION(SIZE(matr,1),kk) :: egvecs
    INTEGER*4 :: info, kk
    
    ! utilities for zheevx
    INTEGER*4 :: N,LWORK,M
    COMPLEX*16, DIMENSION(:), ALLOCATABLE :: WORK
    REAL*8, DIMENSION(7*SIZE(matr,1)) :: RWORK
    INTEGER*4, DIMENSION(5*SIZE(matr,1)) :: IWORK
    INTEGER*4, DIMENSION(SIZE(matr,1)) :: IFAIL
    REAL*8 :: abstol
    
    ! preparing for optimization
    LWORK=-1
    N=SIZE(matr,1)
    ALLOCATE(WORK(1))

    abstol = 2D-323
    !abstol=2D-1023 !theoretical optimal abstol
    ! querying optimal workspace
    CALL ZHEEVX('V','I','U',N,matr,N,0.D0,0.D0,1,kk,abstol,&
         M,egvals,egvecs,N,WORK,LWORK,RWORK,IWORK,IFAIL,info)
   
    ! preparing to get the results
    LWORK=INT(WORK(1))
    DEALLOCATE(WORK)
    ALLOCATE(WORK(LWORK))

    ! getting the results
    CALL ZHEEVX('V','I','U',N,matr,N,0.D0,0.D0,1,kk,abstol,&
         M,egvals,egvecs,N,WORK,LWORK,RWORK,IWORK,IFAIL,info)
    ! removing exhausted variables 
    DEALLOCATE(WORK)
  END SUBROUTINE GET_K_EGV
    



    
    
END MODULE SOLVE_SCHR
