MODULE MATOP
  IMPLICIT NONE

  TYPE RICHMAT
     ! This dtype is an "enriched matrix":
     ! - dims: a vector of integers of size 2
     ! - elems: a double precision complex matrix; its dimensions should
     !   be those stored in dims
     ! - trace: a double precision complex number, representing the
     !   trace of the matrix elems
     ! - det: a double precision complex number, representing the
     !   determinant of the matrix elems
     ! TO BE ADDED:
     ! - updated : a logical flag to denote if the RICHMAT was updated
     INTEGER*4, DIMENSION(2) :: dims
     COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: elems
     COMPLEX*16 :: trace, det
     !LOGICAL :: updated
  END type RICHMAT

  
  INTERFACE OPERATOR (.ADJ.)
     MODULE PROCEDURE MAT_ADJOINT, RICH_ADJOINT
  END INTERFACE OPERATOR (.ADJ.)

  INTERFACE OPERATOR (.TRACE.)
     MODULE PROCEDURE TRACE_MAT, TRACE_RICH
  END INTERFACE OPERATOR (.TRACE.)

  INTERFACE OPERATOR (.INIT.)
     MODULE PROCEDURE NULL_INIT
  END INTERFACE OPERATOR (.INIT.)
  
CONTAINS

  
  FUNCTION NULL_INIT(mat_size)
    ! given the size, this initialization creates a null RICHMAT. If either
    ! one of the dimensions is null or negative, the matrix is left undefined,
    ! having both dimensions set to 0. An error message is also displayed.
    ! - mat_size1, mat_size2 : integers containing the number of rows and
    !   cols of the matrix, respectively
    INTEGER*4, DIMENSION(2), INTENT(IN) :: mat_size
    TYPE(RICHMAT) :: NULL_INIT
    if ((mat_size(1) .ge. 1) .and. (mat_size(2) .ge. 1)) then
       ALLOCATE(NULL_INIT%elems(mat_size(1),mat_size(2)))
       NULL_INIT%dims = mat_size
       NULL_INIT%elems = 0.d0
       NULL_INIT%trace = 0.d0
       NULL_INIT%det = 0.d0
    else
       print*, "Error: the structure could not be properly created: &
&negative dimension."
       ALLOCATE(NULL_INIT%elems(0,0))
       NULL_INIT%dims = (/ 0, 0 /)
       NULL_INIT%elems = 0.d0
       NULL_INIT%trace = 0.d0
       NULL_INIT%det = 0.d0
    endif
    RETURN
  END FUNCTION NULL_INIT

  FUNCTION TRACE_MAT(mat_elems)
    ! given a matrix (equivalently, the elements of a square RICHMAT),
    ! this function checks whether the matrix is square and then, if it
    ! is, calculates the trace.
    ! - mat_size : an integer 2-vector, representing the number of cols/
    !   rows of mat_elems
    ! - mat_elems : a double precision complex matrix containing the
    !   elements
    ! - trace_sq : a double complex number, containing the trace of the
    !   matrix
    INTEGER*4, DIMENSION(2) :: mat_size
    INTEGER*4 :: ii
    COMPLEX*16, DIMENSION(:,:), INTENT(IN) :: mat_elems
    COMPLEX*16 :: trace_mat
    mat_size=SHAPE(mat_elems)
    if (mat_size(1) .eq. mat_size(2)) then
       trace_mat = 0.d0
       do ii=1, mat_size(1)
          trace_mat = trace_mat + mat_elems(ii,ii)
       enddo
    else
       print*, "Matrix is not square: trace set to 0."
       trace_mat = 0.d0
    endif
    RETURN
  END FUNCTION TRACE_MAT

  FUNCTION TRACE_RICH(rich_mat)
    ! this function is rather useless; it simply returns the trace
    ! of the RICHMAT given in input. It is effectively equivalent to
    ! writing RICHMAT%trace.
    ! - rich_mat : a RICHMAT
    TYPE(RICHMAT), INTENT(IN) :: rich_mat
    COMPLEX*16 :: trace_rich
    INTEGER*4 :: ii
    if (rich_mat%dims(1) .eq. rich_mat%dims(2)) then
       trace_rich = 0.d0
       do ii=1, rich_mat%dims(1)
          trace_rich = trace_rich + rich_mat%elems(ii,ii)
       enddo
    else
       print*, "Matrix is not square: trace set to 0."
       trace_rich = 0.d0
    endif
    RETURN
  END FUNCTION TRACE_RICH

  FUNCTION RICH_ADJOINT(rich_mat)
    ! given a RICHMAT, this function computes the adjoint of the
    ! elements of the input, and bundles it together with the other
    ! components. From a mathematical point of view, the adjoint is only
    ! defined for square matrices. However, no computational reason to
    ! not allow for its calculation. As such, no check on the dimensions
    ! is performed.
    ! - enriched_mat : a RICHMAT, the info of which are used to create
    !   its adjoint
    TYPE(RICHMAT), INTENT(IN) :: rich_mat
    TYPE(RICHMAT) :: rich_adjoint
    if (rich_mat%dims(1) .ne. rich_mat%dims(1)) print*, "WARNING: non-square matrix."
    rich_adjoint%trace = conjg(rich_mat%trace)
    rich_adjoint%det = conjg(rich_mat%det)
    rich_adjoint%dims = rich_mat%dims(2:1:-1)
    rich_adjoint%elems = transpose(conjg(rich_mat%elems))
    RETURN
  END FUNCTION RICH_ADJOINT

  FUNCTION MAT_ADJOINT(mat_elems)
    ! given a matrix, returns the adjoint. 
    ! From a mathematical point of view, the adjoint is
    ! only defined for square matrices. However, no computational
    ! reason to not allow for its calculation. As such, no check on
    ! the dimensions is performed.
    ! - mat_size : an integer 2-vector, containing the number of rows
    !   and columns
    ! - mat_elems : a double precision complex matrix
    COMPLEX*16, DIMENSION(:, :), INTENT(IN) :: mat_elems
    COMPLEX*16, DIMENSION(SIZE(mat_elems,2),SIZE(mat_elems,1)) :: mat_adjoint
    if (SIZE(mat_elems,2) .ne. SIZE(mat_elems,1)) print*, "WARNING: non-square matrix."
    mat_adjoint = transpose(conjg(mat_elems))   
    RETURN
  END FUNCTION MAT_ADJOINT

  SUBROUTINE WRITE_RICH(fname, rich_mat)
    ! this subroutine checks if a file with name fname exists. If it
    ! does, it overwrites it with info about rich_mat (in readable
    ! format). If it does not exist, it creates and fills it with
    ! the aforementioned information. To do so, the REPLACE option
    ! of the OPEN function (?) is employed.
    ! - fname : the name of the file on which to save the info
    ! - rich_mat : the RICHMAT to write on file
    CHARACTER*9 :: fname
    TYPE(RICHMAT) :: rich_mat
    INTEGER*4 :: ii
    open(12, file=fname, status="REPLACE", action="WRITE")
    write(12, *) "Dimensions: ", rich_mat%dims
    write(12, *) ""
    write(12, *) "Trace: ", rich_mat%trace
    write(12, *) ""
    write(12, *) "Determinant: ", rich_mat%det
    write(12, *) ""
    write(12, *) "Matrix elements: "
    do ii=1, rich_mat%dims(1)
       write(12, *) rich_mat%elems(ii,:)
    enddo
    RETURN
  END SUBROUTINE WRITE_RICH

  
END MODULE MATOP



PROGRAM EX2_WEEK2
  USE MATOP
  IMPLICIT NONE

  TYPE(RICHMAT) :: richA, richB
  INTEGER*4 :: ii, nrows, ncols
  INTEGER*4, DIMENSION(2) :: dimens
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: re,im
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: matC,matD

  ! setting the sizes of the matrices, both rich and plain
  nrows=0
  ncols=0
  do while (nrows .le. 0)
     print*, "Insert the number of rows: "
     read*, nrows
     if (nrows .le. 0) print*, "Invalid number of rows."
  enddo
  do while (ncols .le. 0)
     print*, "Insert the number of columns: "
     read*, ncols
     if (ncols .le. 0) print*, "Invalid number of columns."
  enddo
  dimens = (/ nrows, ncols /)
  ALLOCATE(re(nrows,ncols),im(nrows,ncols))
  call RANDOM_NUMBER(re)
  call RANDOM_NUMBER(im)

  
  !-----------------------
  ! USING PLAIN MATRICES
  !-----------------------
  ALLOCATE(matC(nrows,ncols))
  ALLOCATE(matD(ncols,nrows))
  matD=0.d0
  ! setting a random matrix C 
  matC = cmplx(re,im)
  ! setting D as the adjoint of C
  matD = .adj.(matC)
  ! checking if the traces correspond
  print*, "The trace of matrix D is: ",.trace.matD
  print*, "The trace of matrix C is: ",.trace.matC
  DEALLOCATE(matC,matD)
	
  !-----------------
  ! USING RICHMATS
  !-----------------
  ! initializing A, then setting its elements
  
  richA=.INIT.(dimens)
  richA%elems=cmplx(re,im)
  richA%trace= .TRACE.(richA)
  ! setting B as the adjoint of A
  richB=.ADJ.(richA)
  ! writing both to an external file
  call WRITE_RICH("richA.txt", richA)
  call WRITE_RICH("richB.txt", richB)
  DEALLOCATE(richA%elems, richB%elems)
  STOP
END PROGRAM EX2_WEEK2
