      MODULE MEX3

      REAL*8, DIMENSION(:,:), ALLOCATABLE :: mat1, mat2, res
      REAL*8, DIMENSION(4) :: result
      INTEGER*2 :: ii, jj, kk
      LOGICAL :: exist
      REAL*8 :: start,finish

      
      CONTAINS

      FUNCTION MM_DOTPROD(matrix1,matrix2,rows1,cr12,cols2) RESULT(resu)
c     this function performs matrix-matrix multiplication by slicing
c     each matrix either row or columnwise, and then using the
c     intrinsic function DOT_PRODUCT(). Does not perform the appropriate
c     controls on the dimensions of the input.
c     matrix1: the first matrix
c     matrix2: the second matrix
c     rows1: the number of rows in the first matrix
c     cr12: the number of columns in the first matrix and of rows in the second
c     cols2: the number of columns in the second matrix  
c     resu: the resulting matrix
      IMPLICIT NONE
      INTEGER*2 :: rows1,cr12,cols2
      REAL*8, DIMENSION(rows1,cr12) :: matrix1
      REAL*8, DIMENSION(cr12,cols2) :: matrix2
      REAL*8, DIMENSION(rows1,cols2) :: resu

      resu = 0.D0
      do ii=1,rows1
         do jj=1,cols2
            resu(ii,jj)=DOT_PRODUCT(matrix1(ii,:),matrix2(:,jj))
         enddo
      enddo
      END FUNCTION MM_DOTPROD

      FUNCTION MM_INDICES(matrix1,matrix2,rows1,cr12,cols2) RESULT(resu)
C     This function performs the matrix-matrix multiplication by direct
C     application of the formula, and so by using indeces. This function does
C     not perform the appropriate check on the dimensions of the input.
C     matrix1: the first matrix
C     matrix2: the second matrix
C     rows1: the number of rows of the first matrix
C     rc12: the number of columns of the first/rows in the second
C     cols2: the number of columns in the second matrix
C     resu: the result of the computation
      IMPLICIT NONE
      INTEGER*2 :: rows1,cr12,cols2
      REAL*8, DIMENSION(rows1,cr12) :: matrix1
      REAL*8, DIMENSION(cr12,cols2) :: matrix2
      REAL*8, DIMENSION(rows1,cols2) :: resu
      REAL*8 :: val

      val = 0.D0
      resu = 0.D0
      DO ii=1,rows1
         DO jj=1,cols2
            DO kk=1,cr12
               resu(ii,jj) = resu(ii,jj) + matrix1(ii,kk)*matrix2(kk,jj)
            ENDDO
         ENDDO
      ENDDO
      END FUNCTION MM_INDICES

      FUNCTION MM_VERT(matrix1,matrix2,rows1,cr12,cols2) RESULT(resu)
C     This function performs the matrix-matrix multiplication by using
C     multiplying the columns of the first matrix by the elements of the
C     second one. No control is performed on the input.
C     matrix1: the first matrix
C     matrix2: the second matrix
C     rows1: the number of rows of the first matrix
C     rc12: the number of columns of the first/rows in the second
C     cols2: the number of columns in the second matrix
C     resu: the result of the computation
      IMPLICIT NONE
      INTEGER*2 :: rows1,cr12,cols2
      REAL*8, DIMENSION(rows1,cr12) :: matrix1
      REAL*8, DIMENSION(cr12,cols2) :: matrix2
      REAL*8, DIMENSION(rows1,cols2) :: resu

      resu = 0.D0
      DO ii=1,cols2
         DO jj=1,cols2
            resu(:,ii) = resu(:,ii) + matrix1(:,jj)*matrix2(jj,ii)
         ENDDO
      ENDDO
      END FUNCTION MM_VERT

      SUBROUTINE READ_INFO(file,sizes)
c     This function opens the file containing the dimensions of the matrices.
c     It then saves those values in an array, so that the program does not need
c     any internal assignment for those variables.      
      IMPLICIT NONE
      CHARACTER*11 file
      INTEGER*2, DIMENSION(4) :: sizes
      open (2, file = file, status = 'old')
      DO ii = 1,4
         read(2,*) sizes(ii)
      ENDDO
      END SUBROUTINE READ_INFO

      END MODULE MEX3


      PROGRAM EX3
      USE MEX3
      IMPLICIT NONE
      INTEGER*2 :: r1,rc,c2
      INTEGER*2, DIMENSION(4) :: vals
      vals=0
      
      call READ_INFO('matinfo.txt',vals)

      IF (vals(2) .EQ. vals(3)) THEN
c     check whether the output file exists; in case it does not, create it
         inquire(file="results.txt", exist=exist)
         if (exist) then
            open(12,file="results.txt",status="old",position="append",
     &action="write")
         else
            open(12, file="results.txt", status="new", action="write")
            write(12,*) 'N',char(9),'DOT_PROD',char(9),'INDICES'
     &,char(9),'VERT',char(9),'MATMUL'
         end if
c     setting the sizes
         r1=vals(1)
         rc=vals(2)
         c2=vals(4)
c     allocating the appropriate memory and initializing the matrices
         ALLOCATE(mat1(r1,rc),mat2(rc,c2),res(r1,c2))
         call RANDOM_NUMBER(mat1)
         call RANDOM_NUMBER(mat2)

c     print*, "Using the intrinsic DOT_PRODUCT():"
         CALL CPU_TIME(start)
         res=MM_DOTPROD(mat1,mat2,r1,rc,c2)
         CALL CPU_TIME(finish)
         result(1)=finish-start

c     print*, "Using the indices formula:"
         CALL CPU_TIME(start)
         res=MM_INDICES(mat1,mat2,r1,rc,c2)
         CALL CPU_TIME(finish)
         result(2)=finish-start
         
c     print*, "Using the 'vertical' formula:"
         CALL CPU_TIME(start)
         res=MM_VERT(mat1,mat2,r1,rc,c2)
         CALL CPU_TIME(finish)
         result(3)=finish-start
         
c     print*, "Using intrinsic functions:"
         CALL CPU_TIME(start)
         res=MATMUL(mat1,mat2)
         CALL CPU_TIME(finish)
         result(4)=finish-start

         write(12,*) r1,result
         DEALLOCATE(mat1,mat2,res)
         
      ELSE
         print*, "Error: wrong dimensions!"
      ENDIF

      STOP
      END PROGRAM EX3
