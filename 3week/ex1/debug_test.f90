PROGRAM DEBUG_TEST
  USE DEBUGGER
  IMPLICIT NONE

  LOGICAL :: DEBUG_ON
  INTEGER*2 :: int2,ii
  INTEGER*4 :: int4
  REAL*4 :: re4
  REAL*8 :: re8
  REAL*16 :: re16
  COMPLEX*8 :: cmplx8
  COMPLEX*16 :: cmplx16
  COMPLEX*8, DIMENSION(3,2) :: matcmplx8
  COMPLEX*16, DIMENSION(4,4) :: matcmplx16
  REAL*4, DIMENSION(3,2) :: realpart4, impart4

  !CALL RANDOM_NUMBER(int2)
  !CALL RANDOM_NUMBER(int4)
  !CALL RANDOM_NUMBER(real4)
  !CALL RANDOM_NUMBER(real8)
  !CALL RANDOM_NUMBER(cmplx8)
  !CALL RANDOM_NUMBER(cmplx16)
  !CALL RANDOM_NUMBER(matcmplx8)
  !CALL RANDOM_NUMBER(matcmplx16)

  DEBUG_ON = .TRUE.
  !CALL DEBUG(DEBUG_ON, int2, (int2 .eq. 0), "int2<0") 
  !CALL DEBUG(DEBUG_ON, int4, (int4 .eq. 0), "int4<0")

  DO ii=1,10
     CALL RANDOM_NUMBER(realpart4)
     CALL RANDOM_NUMBER(impart4)
     matcmplx8=cmplx(realpart4,impart4)
     CALL DEBUG(DEBUG_ON, matcmplx8, (ABS(matcmplx8(ii,ii)) .ge. 1.e0),&
          "diagonal element less than 1")
  ENDDO
  STOP
END PROGRAM DEBUG_TEST
