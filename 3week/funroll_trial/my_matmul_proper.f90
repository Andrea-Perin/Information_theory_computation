MODULE MYMATMUL
  ! The MYMATMUL module contains different implementations of the
  ! matrix-matrix multiplication algorithm. These are based on the
  ! different use of indexes, as to employ different orderings.
  ! In particular, (ii) loops over the first dimension of the
  ! first matrix, (jj) loops over the second dimension of the
  ! second matrix, and (kk) loops over the common (if existing)
  ! dimension. In the names, the order of these indices is related
  ! to their 'speed of variation', meaning that in MATMUL_IJK, k
  ! is the fastest varying among all indices.
  IMPLICIT NONE

  INTEGER*4 :: rows1,cols1,rows2,cols2  
  INTEGER*4 :: ii,jj,kk
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: mat1,mat2,resu1,resu2,resu3,resu4

CONTAINS

  SUBROUTINE READ_DIMENSION(file,sizes)
    ! This subroutine opens the file containing the dimensions
    ! of the matrices. It then saves those values in an array,
    ! so that the program does not need any internal assignment
    ! for those variables.
    ! If the file does not exist, a message is printed and the
    ! program closes.
    ! - file : the name of the file containing the sizes
    ! - sizes : an integer array, containing the sizes read from
    !           the external file
    CHARACTER(LEN=*) :: file
    INTEGER*4, DIMENSION(4) :: sizes
    INTEGER*4 :: stat
    OPEN (2, file = file, status = 'old', IOSTAT=stat)
    IF (stat .NE. 0) THEN
            WRITE(*,*) file,  " cannot be opened"
            STOP 1
    ENDIF
    DO ii = 1,4
       READ(2,*) sizes(ii)
    ENDDO
    CLOSE(2)
  END SUBROUTINE READ_DIMENSION
    
  SUBROUTINE INSERT_DIMENSION(rows1,cols1,rows2,cols2)
    ! This subroutine is used to ask the user for the dimensions
    ! of the matrices to be multiplied.
    ! - rows1 : number of rows of the first matrix
    ! - cols1 : number of columns of the first matrix
    ! - rows2 : number of rows of the second matrix
    ! - cols2 : number of columns of the second matrix
    INTEGER*4 :: rows1,cols1,rows2,cols2
    rows1=0
    cols1=0
    rows2=0
    cols2=0   
    DO WHILE (rows1 .le. 0)
       PRINT*, "Insert the number of rows of the first matrix:"
       READ*, rows1
       IF (rows1 .le. 0) PRINT*, "Insert positive value!"
    ENDDO
    DO WHILE (cols1 .le. 0)
       PRINT*, "Insert the number of columns of the first matrix"
       READ*, cols1
       IF (cols1 .le. 0) PRINT*, "Insert positive value!"
    ENDDO
    DO WHILE (rows2 .le. 0)
       PRINT*, "Insert the number of rows of the second matrix"
       READ*, rows2
       IF (rows2 .le. 0) PRINT*, "Insert positive value!"
    ENDDO
    DO WHILE (cols2 .le. 0)
       PRINT*, "Insert the number of columns of the second matrix"
       READ*, cols2
       IF (cols2 .le. 0) PRINT*, "Insert positive value!"
    ENDDO
  END SUBROUTINE INSERT_DIMENSION
  
  FUNCTION CHECK_DIMS(size1,size2)
    ! This function checks whether the two vector in input,
    ! representing the dimensions of two matrices, allow for the
    ! computation of a matrix-matrix product.
    ! -check_dims : a logical value, is True when the dimensions
    !               are compatible, False otherwise
    ! -size1,size2 : two integer arrays, containing the dimensions
    !                of the two matrices
    LOGICAL :: check_dims
    INTEGER*4, DIMENSION(2) :: size1, size2
    IF ((size1(2)) .eq. (size2(1))) THEN
       check_dims = .TRUE.
    ELSE
       check_dims = .FALSE.
    ENDIF
    RETURN
  END FUNCTION CHECK_DIMS
  
  FUNCTION MATMUL_IJK(matrix1,matrix2)
    ! This function computes the matrix-matrix multiplication
    ! by simply using the index formula one usually employs,
    ! so that the (ii,jj) element of the product is computed
    ! as the row-column product of the ii-th row of the first
    ! matrix and the jj-th column of the second.
    ! - matrix1, matrix2 : two REAL*8 matrices; it should be
    !                      matrix1=(n,m) and matrix2=(m,l),
    !                      otherwise a warning is printed and
    !                      no computation is carried out.
    INTEGER*4 :: ii,jj,kk
    REAL*8, DIMENSION(:,:) :: matrix1,matrix2
    REAL*8, DIMENSION(SIZE(matrix1,1),SIZE(matrix2,2)) :: matmul_ijk
    INTEGER*4, DIMENSION(2) :: size1,size2 
    matmul_ijk = 0.d0
    size1 = SHAPE(matrix1)
    size2 = SHAPE(matrix2)
    IF (CHECK_DIMS(size1,size2)) THEN
       DO ii=1,size1(1)
          DO jj=1,size2(2)
             DO kk=1,size1(2)
                matmul_ijk(ii,jj) = matmul_ijk(ii,jj)+&
                     matrix1(ii,kk)*matrix2(kk,jj)
             ENDDO
          ENDDO
       ENDDO
       RETURN
    ELSE
       PRINT*, "Wrong dimensions. (n,m)x(k,l)=>(n,l),&
            & all elements set to 0."
       RETURN
    ENDIF
  END FUNCTION MATMUL_IJK

  FUNCTION MATMUL_JKI(matrix1,matrix2)
    ! This function computes the matrix-matrix multiplication
    ! by delaying column switching as much as possible. The end
    ! result is computed by accessing each column multiple times
    ! - matrix1, matrix2 : two REAL*8 matrices; it should be
    !                      matrix1=(n,m) and matrix2=(m,l),
    !                      otherwise a warning is printed and
    !                      no computation is carried out.
    INTEGER*4 :: ii,jj,kk
    REAL*8, DIMENSION(:,:) :: matrix1,matrix2
    REAL*8, DIMENSION(SIZE(matrix1,1),SIZE(matrix2,2)) :: matmul_jki
    INTEGER*4, DIMENSION(2) :: size1,size2 
    matmul_jki = 0.d0
    size1 = SHAPE(matrix1)
    size2 = SHAPE(matrix2)
    IF (CHECK_DIMS(size1,size2)) THEN
       DO jj=1,size2(2)
          DO kk=1,size1(2)
             DO ii=1,size1(1)
                matmul_jki(ii,jj) = matmul_jki(ii,jj)+&
                     matrix1(ii,kk)*matrix2(kk,jj)
             ENDDO
          ENDDO
       ENDDO
       RETURN
    ELSE
       PRINT*, "Wrong dimensions. (n,m)x(k,l)=>(n,l),&
            & all elements set to 0."
       RETURN
    ENDIF
  END FUNCTION MATMUL_JKI

  FUNCTION MATMUL_KJI(matrix1,matrix2)
    ! This function computes the matrix-matrix multiplication
    ! by switching columns at intermediate speed.
    ! This means that the columns of the matrices are walked
    ! through m times, where m is the common dimension of the
    ! two input matrices.
    ! - matrix1, matrix2 : two REAL*8 matrices; it should be
    !                      [matrix1]=(n,m) and [matrix2]=(m,l),
    !                      otherwise a warning is printed and
    !                      no computation is carried out.
    INTEGER*4 :: ii,jj,kk
    REAL*8, DIMENSION(:,:) :: matrix1,matrix2
    REAL*8, DIMENSION(SIZE(matrix1,1),SIZE(matrix2,2)) :: matmul_kji
    INTEGER*4, DIMENSION(2) :: size1,size2 
    matmul_kji = 0.d0
    size1 = SHAPE(matrix1)
    size2 = SHAPE(matrix2)
    IF (CHECK_DIMS(size1,size2)) THEN
       DO kk=1,size1(2)
          DO jj=1,size2(2)
             DO ii=1,size1(1)
                matmul_kji(ii,jj) = matmul_kji(ii,jj)+&
                     matrix1(ii,kk)*matrix2(kk,jj)
             ENDDO
          ENDDO
       ENDDO
       RETURN
    ELSE
       PRINT*, "Wrong dimensions. (n,m)x(k,l)=>(n,l),&
            & all elements set to 0."
       RETURN
    ENDIF
  END FUNCTION MATMUL_KJI
  
END MODULE MYMATMUL


PROGRAM PROPERTRIAL
  USE MYMATMUL
  USE DEBUGGER
  IMPLICIT NONE
  ! Variables used to record the execution times of multiplications
  REAL*8 :: start, finish
  ! Variable to store the sizes of matrices
  INTEGER*4, DIMENSION(4) :: allsize
  ! Flag for turning manual size input on
  LOGICAL :: manual
  manual = .FALSE.

!------------------ 
  debug_on = .FALSE.
!------------------
  
  ! Getting the dimensions of the matrices and allocating them
  ! In order to make the performance comparison easier, the
  ! automatic reading from file is used.
  ! Uncomment the following if manual input is preferred.
  !----------------------------------------------
  ! manual = .FALSE.
  !----------------------------------------------
  IF (manual) THEN
     CALL INSERT_DIMENSION(rows1,cols1,rows2,cols2)
  ELSE
     CALL READ_DIMENSION("matinfo.txt",allsize)
     rows1=allsize(1)
     cols1=allsize(2)
     rows2=allsize(3)
     cols2=allsize(4)
  END IF
  IF (CHECK_DIMS( (/rows1,cols1/) ,(/rows2,cols2/) )) THEN
     ALLOCATE(mat1(rows1,cols1),mat2(rows2,cols2)) 
     ! Filling the matrices with random numbers  ~U([0,1])
     CALL RANDOM_NUMBER(mat1)
     CALL RANDOM_NUMBER(mat2)
     ! Some debugging if needed: only prints errors!
     CALL DEBUG(debug_on, SHAPE(mat1), &
          (ALL(SHAPE(mat1) .NE. (/rows1,cols1/))),"size mat1")
     CALL DEBUG(debug_on, SHAPE(mat2), &
          (ALL(SHAPE(mat2) .NE. (/rows2,cols2/))),"size mat2")
     
     ! Timing the performances of the three algorithms:
     ! measuring MATMUL_IJK (slowest)
     CALL CPU_TIME(start)
     resu1=MATMUL_IJK(mat1,mat2)
     CALL DEBUG(debug_on, SHAPE(resu1), &
          (ALL(SHAPE(resu1) .NE. (/rows1,cols2/))),"size result!")
     CALL CPU_TIME(finish)
     PRINT*, finish-start
     
     ! measuring MATMUL_JKI (fast)
     CALL CPU_TIME(start)
     resu2=MATMUL_JKI(mat1,mat2)
     CALL DEBUG(debug_on, SHAPE(resu2), &
          (ALL(SHAPE(resu2) .NE. (/rows1,cols2/))),"size result!")
     CALL CPU_TIME(finish)
     PRINT*, finish-start
     
     ! measuring MATMUL_KJI (fast)
     CALL CPU_TIME(start)
     resu3=MATMUL_KJI(mat1,mat2)
     CALL DEBUG(debug_on, SHAPE(resu3), &
          (ALL(SHAPE(resu3) .NE. (/rows1,cols2/))),"size result!")
     CALL CPU_TIME(finish)
     PRINT*, finish-start
     
     ! measuring intrinsic MATMUL (fastest)
     CALL CPU_TIME(start)
     resu4=MATMUL(mat1,mat2)
     CALL DEBUG(debug_on, SHAPE(resu4), &
          (ALL(SHAPE(resu4) .NE. (/rows1,cols2/))),"size result!")
     CALL CPU_TIME(finish)
     PRINT*, finish-start
     
     ! checking if the results match
     CALL DEBUG(debug_on, 0, (ANY(resu1-resu2 .gt. 1d-14) &
          .OR. ANY(resu2-resu3 .gt. 1d-14) &
          .OR. ANY(resu3-resu4 .gt. 1d-14)),"Wrong!")
     Print*, ALL(resu1==resu2)
     print*, ALL(resu1==resu3)
     print*, ALL(resu1==resu4)
     print*, ALL(resu2==resu3)
     print*, ALL(resu2==resu4)
     print*, ALL(resu3==resu4)

  ELSE
     PRINT*, "Dimension error: matrices can not be multiplied."
  ENDIF
  STOP
END PROGRAM PROPERTRIAL
