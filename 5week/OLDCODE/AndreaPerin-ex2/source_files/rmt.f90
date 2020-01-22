MODULE PROB_DIST

CONTAINS

  SUBROUTINE HISTOGRAM(input_data,histo,bin_edges,num_bins_opt,man_rng)
    REAL*4, DIMENSION(:) :: input_data
    REAL*4, DIMENSION(:), ALLOCATABLE :: bin_edges
    INTEGER*4, DIMENSION(:), ALLOCATABLE :: histo
    REAL*4 :: binwidth, min_val, max_val
    INTEGER*4, OPTIONAL :: num_bins_opt
    INTEGER*4 :: num_bins,ii,jj
    REAL*4, DIMENSION(2), OPTIONAL :: man_rng
    
    ! num_bins is set to 25 by default
    IF(PRESENT(num_bins_opt).AND.(num_bins_opt.NE.0)) THEN
       num_bins=num_bins_opt
    ELSE
       num_bins=60
    END IF
    
    ! setting the binwidth
    IF (PRESENT(man_rng).AND.(man_rng(1).LT.man_rng(2))) THEN
       min_val=man_rng(1)
       max_val=man_rng(2)
    ELSE
       min_val = MINVAL(input_data)
       max_val = MAXVAL(input_data)
    END IF
    binwidth=(max_val-min_val)/num_bins

    ! allocating the results
    ALLOCATE(histo(num_bins))
    ALLOCATE(bin_edges(num_bins+1))

    ! filling the histogram
    histo = 0
    DO ii=1,SIZE(input_data)
       DO jj=1,num_bins-1
          IF ((input_data(ii) .ge. min_val+REAL(jj-1)*binwidth) &
               .AND. (input_data(ii) .lt. min_val+REAL(jj)*binwidth)) THEN
             histo(jj)=histo(jj)+1
          END IF
       END DO
       IF ((input_data(ii) .ge. min_val+REAL(num_bins-1)*binwidth) &
            .AND. (input_data(ii) .le. min_val+REAL(num_bins)*binwidth)) THEN
          histo(num_bins)=histo(num_bins)+1
       END IF
    END DO
    ! filling the edges of the histogram
    DO ii=1,num_bins+1
       bin_edges(ii)=min_val+(ii-1)*binwidth
    END DO
    RETURN
  END SUBROUTINE HISTOGRAM

  SUBROUTINE NORM_HISTOGRAM(input_data,histo,bin_edges,num_bins_opt,man_rng)
    REAL*4, DIMENSION(:) :: input_data
    REAL*4, DIMENSION(:), ALLOCATABLE :: bin_edges, histo
    INTEGER*4, DIMENSION(:), ALLOCATABLE :: histo_tmp 
    REAL*4 :: binwidth
    INTEGER*4, OPTIONAL :: num_bins_opt
    REAL*4, DIMENSION(2), OPTIONAL :: man_rng
    ! calling the HISTOGRAM SUBROUTINE
    CALL HISTOGRAM(input_data,histo_tmp,bin_edges,num_bins_opt,man_rng)
    ! normalizing the produced histogram
    histo=REAL(histo_tmp)/SIZE(input_data)
    DEALLOCATE(histo_tmp)
    RETURN
  END SUBROUTINE NORM_HISTOGRAM

  SUBROUTINE PROB_DISTRIBUTION(input_data,dist,x_points,num_bins_opt,man_rng)
    REAL*4, DIMENSION(:) :: input_data
    REAL*4, DIMENSION(:), ALLOCATABLE :: bin_edges, x_points, dist, histo
    INTEGER*4 :: num_bins,ii
    REAL*4 :: binwidth
    INTEGER*4, OPTIONAL :: num_bins_opt
    REAL*4, DIMENSION(2), OPTIONAL :: man_rng
    
    CALL NORM_HISTOGRAM(input_data,histo,bin_edges,num_bins_opt,man_rng)
    num_bins=SIZE(histo)
    binwidth=bin_edges(2)-bin_edges(1)
    ALLOCATE(x_points(num_bins))
    ALLOCATE(dist(num_bins))
    
    DO ii=1, num_bins
       x_points(ii)=(bin_edges(ii-1)+bin_edges(ii))/2
       dist(ii)=REAL(histo(ii))/(binwidth)
    END DO
    DEALLOCATE(bin_edges,histo)
    RETURN
  END SUBROUTINE PROB_DISTRIBUTION
  
  
END MODULE PROB_DIST


PROGRAM PDIST_TRIAL
  USE PROB_DIST
  USE RANDOM_MATRIX
  IMPLICIT NONE

  REAL*4, DIMENSION(:), ALLOCATABLE :: spacings
  REAL*4, DIMENSION(:), ALLOCATABLE :: x_pts, dist
  INTEGER*4, DIMENSION(:), ALLOCATABLE :: hist
  INTEGER*4 :: ii,count_lines,io,count_args,bins
  REAL*4 :: minr,maxr
  CHARACTER(LEN=100) :: filename
  CHARACTER*4 :: arg_tmp

  count_args=COMMAND_ARGUMENT_COUNT()
  IF (count_args.EQ.0)THEN
     WRITE(*,*) "#NO FILENAME PROVIDED"
  ELSE
     ! getting the filename
     CALL GET_COMMAND_ARGUMENT(1, filename)
     ! getting the size of the file, to then allocate the array
     count_lines = 0 
     OPEN(15,FILE=TRIM(filename))
     DO
        READ(15,*,iostat=io)
        IF (io.NE.0) EXIT
        count_lines = count_lines + 1
     END DO
     REWIND(15)
     ! allocating the array
     ALLOCATE(spacings(count_lines))
     ! filling the array
     READ(15,*) spacings
     CLOSE(15)
     ! getting info on the distribution: num_bins and range
     IF (count_args.GT.1) THEN
        CALL GET_COMMAND_ARGUMENT(2,arg_tmp)
        READ(arg_tmp,*) bins
        IF (count_args.GE.4) THEN
           CALL GET_COMMAND_ARGUMENT(3,arg_tmp)
           READ(arg_tmp,*) minr
           CALL GET_COMMAND_ARGUMENT(4,arg_tmp)
           READ(arg_tmp,*) maxr
           CALL PROB_DISTRIBUTION(spacings,dist,x_pts,num_bins_opt=bins,&
                man_rng=(/minr,maxr/))
        ELSE
           CALL PROB_DISTRIBUTION(spacings,dist,x_pts,num_bins_opt=bins)
        END IF
     ELSE
        CALL PROB_DISTRIBUTION(spacings,dist,x_pts)
     END IF
     
     ! printing to console
     DO ii=1,SIZE(x_pts)
        PRINT*, x_pts(ii),dist(ii)
     END DO
  END IF
END PROGRAM PDIST_TRIAL
