MODULE MATOP
  IMPLICIT NONE

  TYPE RICHMAT
     ! This dtype is an "enriched matrix":
     ! - dims: an integer vector of integers of size 2
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

  
  INTERFACE MY_ADJ
     FUNCTION RICH_ADJOINT(rich_mat)
       TYPE(RICHMAT) :: rich_mat
     END FUNCTION RICH_ADJOINT

     FUNCTION MAT_ADJOINT(mat_size, mat_elems)
       COMPLEX*16, DIMENSION(:,:) :: mat_elems
       INTEGER*4, DIMENSION(2) :: mat_size
     END FUNCTION MAT_ADJOINT
  END INTERFACE  MY_ADJ

  INTERFACE MY_TRACE
     FUNCTION TRACE_MAT(mat_size, mat_elems)
       COMPLEX*16, DIMENSION(:,:) :: mat_elems
       INTEGER*4, DIMENSION(2) :: mat_size       
     END FUNCTION TRACE_MAT

     FUNCTION TRACE_RICH(rich_mat)
       TYPE(RICHMAT) :: rich_mat
     END FUNCTION TRACE_RICH
  END INTERFACE MY_TRACE

  
CONTAINS

  
  FUNCTION NULL_INIT(mat_size)
    ! given the size, this initialization creates a null RICHMAT. If either
    ! one of the dimensions is null or negative, the matrix is left undefined,
    ! having both dimensions set to 0. An error message is also displayed.
    ! - mat_size : a 2-elements array containing the number of rows and
    !   cols of the matrix
    INTEGER*4, DIMENSION(2), INTENT(IN) :: mat_size
    TYPE(RICHMAT) :: NULL_INIT
    if ((mat_size(1) .ge. 1) .and. (mat_size(2) .ge. 1)) then
       ALLOCATE(NULL_INIT%elems(mat_size(1),mat_size(2)))
       NULL_INIT%dims = mat_size
       NULL_INIT%elems = 0.d0
       NULL_INIT%trace = 0.d0
       NULL_INIT%det = 0.d0
    else
       print*, "Error: the structure could not be properly created: negative dimension."
       ALLOCATE(NULL_INIT%elems(0,0))
       NULL_INIT%dims = (/ 0, 0 /)
       NULL_INIT%elems = 0.d0
       NULL_INIT%trace = 0.d0
       NULL_INIT%det = 0.d0
    endif
    RETURN
  END FUNCTION NULL_INIT

  FUNCTION TRACE_MAT(mat_size, mat_elems)
    ! given a matrix (equivalently, the elements of a square RICHMAT),
    ! this function checks whether the matrix is square and then, if it
    ! is, calculates the trace.
    ! - mat_size : an integer 2-vector, representing the number of cols/
    !   rows of mat_elems
    ! - mat_elems : a double precision complex matrix containing the
    !   elements
    ! - trace_sq : a double complex number, containing the trace of the
    !   matrix
    INTEGER*4, DIMENSION(2), INTENT(IN) :: mat_size
    INTEGER*4 :: ii
    COMPLEX*16, DIMENSION(mat_size(1),mat_size(2)), INTENT(IN) :: mat_elems
    COMPLEX*16 :: trace_mat
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
       print*, "Non square argument; trace set to 0"
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
    rich_adjoint%trace = rich_mat%trace
    rich_adjoint%det = (rich_mat%det)**((rich_mat%dims(1))-1)
    rich_adjoint%dims = rich_mat%dims
    rich_adjoint%elems = transpose(conjg(rich_mat%elems))
    RETURN
  END FUNCTION RICH_ADJOINT

  FUNCTION MAT_ADJOINT(mat_size, mat_elems)
    ! given a matrix and a vector containing its dimensions, returns
    ! the adjoint. From a mathematical point of view, the adjoint is
    ! only defined for square matrices. However, no computational
    ! reason to not allow for its calculation. As such, no check on
    ! the dimensions is performed.
    ! - mat_size : an integer 2-vector, containing the number of rows
    !   and columns
    ! - mat_elems : a double precision complex matrix
    INTEGER*4, DIMENSION(2), INTENT(IN) :: mat_size
    COMPLEX*16, DIMENSION(mat_size(1), mat_size(2)), INTENT(IN) :: mat_elems
    COMPLEX*16, DIMENSION(mat_size(2), mat_size(1)) :: mat_adjoint
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
    CHARACTER*16 :: fname
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

  TYPE(RICHMAT) :: A, B
  INTEGER*4, DIMENSION(2) :: msize
  INTEGER*4 :: ii
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: re,im
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: C,D
  
  msize(1)=3
  msize(2)=3

  ALLOCATE(re(msize(1),msize(2)),im(msize(1),msize(2)))
  ALLOCATE(C(msize(1),msize(2)))
  ALLOCATE(D(msize(2),msize(1)))
  D=0.d0
  call RANDOM_NUMBER(re)
  call RANDOM_NUMBER(im)
  C = cmplx(re,im)
  D = MY_ADJ(C)
  do ii=1,msize(1)
     print*, D(ii,:)
  enddo
  print*, ""
  do ii=1,msize(1)
     print*, C(ii,:)
  enddo
  print*, MY_TRACE(C)
  print*, MY_TRACE(D)
  !A=NULL_INIT(msize)
  !A%elems=cmplx(re,im)
  !A%trace= .TRACE.(A)
  !B=.ADJ.(A)
  !call WRITE_RICH("test12345678.txt", A)
  !call WRITE_RICH("12345678test.txt", B)
  STOP
END PROGRAM EX2_WEEK2
