MODULE DEBUGGER
  IMPLICIT NONE

  ! A module for debugging. For the listed data types, a debug
  ! subroutine is implemented.
  ! - debug : a flag to activate the debug
  ! - var : the variable on which to perform the debug
  ! - check : a logical value, that represents the test to be performed
  ! - message : an output string, provided by the user, which
  !             should describe the eventual error.
  
  LOGICAL :: debug_on

  !  This is a template for adding debugging options for other data types.
  !  Simply substitute <TYPE> with the desired data type. Do not forget to
  !  add the subroutine to the interface!
  !  
  !  SUBROUTINE <TYPE>_DEBUG(debug,var,check,message)
  !    ! a <TYPE> variable debugging subroutine.
  !    ! Calculates the truth value of check and prints
  !    ! the results.
  !    <TYPE> :: var
  !    LOGICAL :: debug,check
  !    CHARACTER(LEN=*) :: message
  !    IF (debug .and. check) THEN       
  !       PRINT*, "---------------------"
  !       PRINT*, "__DEBUG INFO__: ", message
  !       PRINT*, "Variable content: ", var
  !       PRINT*, "---------------------"
  !    ENDIF
  !  END SUBROUTINE <TYPE>_DEBUG
  !
  
  INTERFACE DEBUG
     MODULE PROCEDURE INT4_DEBUG
     MODULE PROCEDURE INT2_DEBUG
     MODULE PROCEDURE REAL4_DEBUG
     MODULE PROCEDURE REAL8_DEBUG
     MODULE PROCEDURE CMPLX8_DEBUG
     MODULE PROCEDURE CMPLX16_DEBUG
     MODULE PROCEDURE CMPLX8MAT_DEBUG
     MODULE PROCEDURE CMPLX16MAT_DEBUG
     MODULE PROCEDURE REAL8MAT_DEBUG
     MODULE PROCEDURE REAL4MAT_DEBUG
     MODULE PROCEDURE INT2VEC_DEBUG
     MODULE PROCEDURE INT4VEC_DEBUG
  END INTERFACE DEBUG

  
CONTAINS
  
  SUBROUTINE INT4_DEBUG(debug,var,check,message)
    ! a 4-byte integer variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    INTEGER*4 :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE INT4_DEBUG
  
  SUBROUTINE INT2_DEBUG(debug,var,check,message)
    ! a 2-byte integer variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    INTEGER*2 :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE INT2_DEBUG

  SUBROUTINE CMPLX8_DEBUG(debug,var,check,message)
    ! a single precision complex variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    COMPLEX*8 :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE CMPLX8_DEBUG

  SUBROUTINE CMPLX16_DEBUG(debug,var,check,message)
    ! a double precision complex variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    COMPLEX*16 :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE CMPLX16_DEBUG

  SUBROUTINE CMPLX8MAT_DEBUG(debug,var,check,message)
    ! a single precision complex matrix variable debugging
    ! subroutine. Calculates the truth value of check and prints
    ! the results.
    COMPLEX*8, DIMENSION(:,:) :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE CMPLX8MAT_DEBUG

  SUBROUTINE CMPLX16MAT_DEBUG(debug,var,check,message)
    ! a single precision complex matrix variable debugging
    ! subroutine. Calculates the truth value of check and prints
    ! the results.
    COMPLEX*16, DIMENSION(:,:) :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE CMPLX16MAT_DEBUG

  SUBROUTINE REAL8_DEBUG(debug,var,check,message)
    ! a double precision real variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    REAL*8 :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE REAL8_DEBUG

  SUBROUTINE REAL4_DEBUG(debug,var,check,message)
    ! a single precision real variable debugging
    ! subroutine. Calculates the truth value of check and prints
    ! the results.
    REAL*4 :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN      
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE REAL4_DEBUG

  SUBROUTINE REAL8MAT_DEBUG(debug,var,check,message)
    ! a double precision matrix variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    REAL*8, DIMENSION(:,:) :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE REAL8MAT_DEBUG

  SUBROUTINE REAL4MAT_DEBUG(debug,var,check,message)
    ! a single precision matrix variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    REAL*4, DIMENSION(:,:) :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE REAL4MAT_DEBUG

  SUBROUTINE INT4VEC_DEBUG(debug,var,check,message)
    ! a 4-byte integer vector variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    INTEGER*4, DIMENSION(:) :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE INT4VEC_DEBUG

  SUBROUTINE INT2VEC_DEBUG(debug,var,check,message)
    ! a 2-byte integer vector variable debugging subroutine.
    ! Calculates the truth value of check and prints
    ! the results.
    INTEGER*2, DIMENSION(:) :: var
    LOGICAL :: debug,check
    CHARACTER(LEN=*) :: message
    IF (debug .and. check) THEN       
       PRINT*, "---------------------"
       PRINT*, "__DEBUG INFO__: ", message
       PRINT*, "Variable content: ", var
       PRINT*, "---------------------"
    ENDIF
  END SUBROUTINE INT2VEC_DEBUG

END MODULE DEBUGGER


