







PROGRAM R_CALC
  USE RANDOM_MATRIX
  IMPLICIT NONE
  
  CHARACTER(LEN=10) :: tmp_size
  INTEGER*4 :: count_args, ii
  INTEGER*4 :: matsize, num_trials
  REAL*4, ALLOCATABLE, DIMENSION(:) :: egv_herm,egv_diag,r,delta_lambda
  COMPLEX*8, DIMENSION(:,:), ALLOCATABLE :: herm, diag

!  CALL INIT_RANDOM_SEED()
  count_args=COMMAND_ARGUMENT_COUNT()
  IF (count_args.LT.2) THEN
     PRINT*, "#Not enough args!"
  ELSE
     CALL GET_COMMAND_ARGUMENT(1, tmp_size)
     READ (tmp_size,*) matsize
     CALL GET_COMMAND_ARGUMENT(1, tmp_size)
     READ (tmp_size,*) num_trials
     DO ii=1,num_trials
        herm=RAND_INIT(matsize)
        diag=RAND_INIT(matsize,.TRUE.)
        
  
