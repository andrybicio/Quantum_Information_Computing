MODULE HERMITIAN_MATRICES_UTILS

   !Implementation of a new type: HISTOGRAM
   TYPE HISTOGRAM
       !"Structural" variables
       REAL*8 :: upper, lower
       INTEGER :: n_bins 
       REAL*8,  DIMENSION(:), ALLOCATABLE  :: edges, bins
       !Following values have to be filled
       REAL*8,  DIMENSION(:), ALLOCATABLE  :: densities
       REAL*8                              :: bin_width
       INTEGER, DIMENSION(:), ALLOCATABLE  :: counts
   END TYPE HISTOGRAM 

CONTAINS

FUNCTION INIT_HIST(lower_bound, upper_bound, n_bins) RESULT(hist)
   IMPLICIT NONE
   
   INTEGER, INTENT(IN) :: n_bins
   REAL*8, INTENT(IN)  :: lower_bound, upper_bound

   TYPE(HISTOGRAM) :: hist
   INTEGER :: ii
   REAL*8  :: ii_double
   
   ALLOCATE(hist%edges(n_bins+1))
   ALLOCATE(hist%bins(n_bins))
   ALLOCATE(hist%densities(n_bins))
   ALLOCATE(hist%counts(n_bins))
   
   hist%n_bins = n_bins
   hist%upper  = upper_bound
   hist%lower  = lower_bound
   hist%bin_width = (upper_bound-lower_bound)/n_bins
   
   !Trust me, it's needed....
   ii_double = 0d0
   !Compute the edges and the center of each bin
   DO ii = 0, n_bins - 1
      hist%edges(ii+1) = (lower_bound + ii_double*hist%bin_width)
      hist%bins(ii+1)  = (lower_bound + hist%bin_width*( 5d-1 + ii_double ))
      ii_double = ii_double + 1d0
   END DO
      hist%edges(n_bins + 1 ) = upper_bound
   RETURN
   
   DO ii = 1, n_bins
      hist%counts(ii)    = 0
      hist%densities(ii) = 0d0
   END DO
   
END FUNCTION INIT_HIST

FUNCTION COPY_EMPTY_HIST(hist_to_be_copied) RESULT (new_hist)
   IMPLICIT NONE
   
   TYPE(HISTOGRAM), INTENT(IN) :: hist_to_be_copied
   TYPE(HISTOGRAM) :: new_hist
   
   INTEGER :: ii
   
   new_hist = INIT_HIST(hist_to_be_copied%lower, hist_to_be_copied%upper, &
                      & hist_to_be_copied%n_bins)
                      
   DO ii = 1, new_hist%n_bins
      new_hist%counts(ii)    = 0
      new_hist%densities(ii) = 0d0
   END DO
   
END FUNCTION

SUBROUTINE FILL_HIST(hist, values)
   IMPLICIT NONE
   
   !Input values 
   TYPE(HISTOGRAM), INTENT(INOUT) :: hist
   REAL*8, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: values
   
   !Values to be copied
   INTEGER, DIMENSION(:), ALLOCATABLE :: counts
   REAL*8,  DIMENSION(:), ALLOCATABLE :: densities, edges
   
   !Temp variables
   REAL*8 :: bin_width, norm_factor
   LOGICAL :: DEBUG = .FALSE.
   INTEGER :: nbins, bin_selector, ii_temp, ii
   
   !Copy temporarily all variables
   nbins     = hist%n_bins
   bin_width = hist%bin_width
   ALLOCATE(edges(nbins+1))
   ALLOCATE(counts(nbins))
   ALLOCATE(densities(nbins))
   edges = hist%edges
   
   !Initialize the count array to zero!
   DO ii = 1, SIZE(counts)
      counts(ii) = 0
   END DO
   
   !it is the index for the values!
   ii_temp = 1
   
   !Count how many values fall inside each bin, array of values is already sorted
   DO bin_selector = 2, SIZE(edges)
   IF (DEBUG) THEN
      PRINT*, bin_selector, SIZE(edges)
   END IF
      DO WHILE ((values(ii_temp).LE.edges(bin_selector)).AND.(ii_temp.LE.SIZE(values)))
         IF (DEBUG) THEN
            PRINT*, "FIRST CONDITION", values(ii_temp), edges(bin_selector)
            PRINT*, "SECOND CONDITION", ii_temp, SIZE(values)
         END IF
         counts(bin_selector - 1) = counts(bin_selector - 1) + 1
         ii_temp = ii_temp + 1
      END DO
   END DO
      
   !Compute the normalization factor
   norm_factor = SUM(counts)*bin_width
   
!DO NOT CHECK, SINCE IF SOME VALUES FALL OUTSIDE THE RANGE OF HIST IT WILL GIVE ERROR
!   IF (SUM(counts).NE.SIZE(values)) THEN
!     IF (.TRUE.) THEN
!        PRINT*, "Tot Counts", SUM(counts)
!     END IF
!       PRINT*, "Error in Histogram function"
!       PRINT*, values
!       CALL EXIT(3)
!   END IF
      
   !Rescale the densities array
   DO ii = 1, nbins
      densities(ii) = counts(ii)/norm_factor
       IF (DEBUG) THEN
         PRINT*, ii, "   ", nbins
       END IF
   END DO
   
   hist%densities = densities
   hist%counts    = counts
   
   DEALLOCATE(edges)
   DEALLOCATE(counts)
   DEALLOCATE(densities)
       
END SUBROUTINE

FUNCTION ADD_HISTOGRAMS(hist1, hist2) RESULT(sum_hist)
   IMPLICIT NONE

   !Input values 
   TYPE(HISTOGRAM), INTENT(IN) :: hist1, hist2
   
   !Resulting hist
   TYPE(HISTOGRAM) :: sum_hist
   
   !Other variables
   REAL*8  :: norm_factor
     
   !Should be a check whether bins/edges are equal, skipping due to lack of time
   INTEGER :: n_bins, ii
   !LOGICAL :: DEBUG
   
   n_bins = hist1%n_bins
   sum_hist = COPY_EMPTY_HIST(hist1)
   
   !Sum the counts
   DO ii = 1, n_bins
      sum_hist%counts(ii) = hist1%counts(ii) + hist2%counts(ii)
   END DO
   
   !Here we do not need to multiply by the bin width! We have already rescaled...
   norm_factor = SUM(sum_hist%counts)

   DO ii = 1, n_bins
      sum_hist%densities(ii) = sum_hist%counts(ii)/norm_factor
   END DO
   
!   PRINT*, "AREA IS", SUM(sum_hist%densities)
   
END FUNCTION

FUNCTION ADD_HISTOGRAMS_ARRAY(hist_array) RESULT(sum_hist)
   IMPLICIT NONE

   !Input values 
   TYPE(HISTOGRAM), DIMENSION(:), INTENT(IN) :: hist_array
   
   !Resulting hist
   TYPE(HISTOGRAM) :: sum_hist
   
   !Other variables
   REAL*8  :: norm_factor
     
   !Should be a check whether bins/edges are equal, skipping due to lack of time
   INTEGER :: n_bins, ii, jj, number_histograms, temp_sum
   !LOGICAL :: DEBUG
   
   
   n_bins = hist_array(1)%n_bins
   sum_hist = COPY_EMPTY_HIST(hist_array(1))
   number_histograms = SIZE(hist_array)
   
   !Sum the counts
   DO ii = 1, n_bins
      temp_sum = 0
      DO jj = 1, number_histograms
        temp_sum = temp_sum + hist_array(jj)%counts(ii)
      END DO
      sum_hist%counts(ii) = temp_sum
   END DO
   
   !Here we do not need to multiply by the bin width! We have already rescaled...
   norm_factor = SUM(sum_hist%counts)

   DO ii = 1, n_bins
      sum_hist%densities(ii) = sum_hist%counts(ii)/norm_factor
   END DO
   
   !PRINT*, "Sum of columns is:", SUM(sum_hist%densities)
   
END FUNCTION

SUBROUTINE OBTAIN_PDF(hist)
IMPLICIT NONE

   TYPE(HISTOGRAM), INTENT(INOUT) :: hist

   !Other variables
   REAL*8  :: norm_factor
   INTEGER :: ii

   norm_factor = SUM(hist%counts*hist%bin_width)

   DO ii = 1, hist%n_bins
      hist%densities(ii) = hist%counts(ii)/norm_factor
   END DO
   
   !PRINT*, "Area is", SUM(hist%densities*hist%bin_width)
END SUBROUTINE

FUNCTION GET_EDGES(lower, upper, n_bins) RESULT (edges)
   IMPLICIT NONE
   
   REAL*8, INTENT(IN)  :: lower, upper
   INTEGER, INTENT(IN) :: n_bins
   REAL*8, DIMENSION(:), ALLOCATABLE :: edges
   
   INTEGER :: ii
   
   IF(.not.ALLOCATED(edges)) THEN
      ALLOCATE(edges(n_bins + 1))
   END IF
   
   DO ii = 0, n_bins
      edges(ii+1) = lower + ii*upper
   END DO
   RETURN
   
END FUNCTION

SUBROUTINE PRINT_HIST_TO_FILE( hist ,filename)
   IMPLICIT NONE

   TYPE(HISTOGRAM), INTENT(IN) :: hist
   CHARACTER(*), INTENT(IN) :: filename
   
   INTEGER :: number_lines, ii
   
   number_lines = hist%n_bins
   
   OPEN(25, file = filename , status = 'unknown')
   DO ii = 1, number_lines
      WRITE(25,*) hist%bins(ii), hist%densities(ii)
   END DO
   CLOSE(25) 

END SUBROUTINE

!Subroutine that takes is input the matrix to be diagonalized
!and returns the diagonalized matrix and the sorted array of eigenvalues
SUBROUTINE HERMITIAN_DIAGONALIZE(matrix, matrix_size, array_eigenvalues, info)

   IMPLICIT NONE
   
   !Declaration of variables INPUT/OUTPUT for subroutines
   DOUBLE COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: matrix
   INTEGER, INTENT(IN)  :: matrix_size
   REAL*8, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: array_eigenvalues
   INTEGER, INTENT(OUT) :: info
   
   !Members of this subroutine 
   INTEGER :: lwork
   DOUBLE COMPLEX, DIMENSION(:), ALLOCATABLE :: work
   REAL*8, DIMENSION(:), ALLOCATABLE :: rwork
      
   !if -1, then we call subroutine in order to obtain the optimal lwork value
   lwork = -1
   ALLOCATE(work(MAX(1, lwork)))
   ALLOCATE(rwork(MAX(1, 3*matrix_size-2)))
   IF(.not.ALLOCATED(array_eigenvalues)) THEN
      ALLOCATE(array_eigenvalues(matrix_size))
   END IF
   
   CALL ZHEEV('N', 'U', matrix_size, matrix, matrix_size, array_eigenvalues, &
              & work, lwork, rwork, info)     
   lwork = int(real(work(1)))
   DEALLOCATE(work)


   !reallocate in order to use the values just found
   !this call does the dirty job
   ALLOCATE(work(MAX(1,lwork)))
   CALL ZHEEV('N', 'U', matrix_size, matrix, matrix_size, array_eigenvalues, &
              & work, lwork, rwork, info)

   DEALLOCATE(work)
   DEALLOCATE(rwork)
   
   CALL SHELL_SORT_ARRAY( array_eigenvalues )

END SUBROUTINE

!Implementation for shell sort algorithm
SUBROUTINE SHELL_SORT_ARRAY(a)
   IMPLICIT NONE
   
   INTEGER :: ii, jj, increment
   REAL*8 :: temp
   REAL*8, INTENT(INOUT) :: a(:)
 
   increment = SIZE(a) / 2
   DO WHILE (increment > 0)
       DO ii = increment+1, SIZE(a)
          jj = ii
          temp = a(ii)
          DO WHILE (jj >= increment+1 .AND. a(jj-increment) > temp)
             a(jj) = a(jj-increment)
             jj = jj - increment
          END DO
          a(jj) = temp
       END DO
       IF (increment == 2) THEN
          increment = 1
       ELSE
          increment = increment * 5 / 11
       END IF      
   END DO
END SUBROUTINE SHELL_SORT_ARRAY

SUBROUTINE DELTA_ELEMENTS_ARRAY(array, delta_array) 
    IMPLICIT NONE
    
    REAL*8, INTENT(IN) :: array(:)
    REAL*8, INTENT(OUT) :: delta_array(:)
    
    INTEGER :: total_elements, ii
    
    total_elements = SIZE(array)
    
    DO ii = 1, total_elements-1
       delta_array(ii) = array(ii+1) - array(ii)
    END DO
    
END SUBROUTINE 

SUBROUTINE NORMALIZED_SPACINGS_TOTAL(array, norm_array) 
    IMPLICIT NONE
    
    REAL*8, INTENT(IN) :: array(:)
    REAL*8, INTENT(OUT) :: norm_array(:)
    
    INTEGER :: total_elements, ii
    REAL*8 :: mean
    
    total_elements = SIZE(array)
    mean = SUM(array)/total_elements

    DO ii = 1, total_elements
       norm_array(ii) = array(ii)/mean
    END DO
    
    !Return the array already sorted
    CALL SHELL_SORT_ARRAY( norm_array )
    
END SUBROUTINE 

SUBROUTINE NORMALIZED_SPACINGS_LOCAL(array, norm_array, window_size_side)
    IMPLICIT NONE
     
    REAL*8, INTENT(IN) :: array(:)
    INTEGER, INTENT(IN) :: window_size_side
    REAL*8, DIMENSION(:), INTENT(OUT) :: norm_array(:)
    
    INTEGER :: total_elements, ii, lb, ub
    REAL*8 :: temp_mean
    
    total_elements = SIZE(array)
    
    
    DO ii = 1, total_elements
       lb = MAX(1 , ii-window_size_side)
       ub = MIN(ii + window_size_side , total_elements)
!       PRINT*, lb, ub, ii, ub-lb+1
       
       temp_mean = SUM(array(lb:ub))/(ub-lb + 1)
!       PRINT*, temp_mean

       norm_array(ii) = array(ii)/temp_mean
!       PRINT*, norm_array(ii)
    END DO
    
    !Return the array already sorted
    CALL SHELL_SORT_ARRAY( norm_array )
!    PRINT*, norm_array
END SUBROUTINE 

FUNCTION COMPUTE_R_MEAN(delta_array) RESULT(r_mean)
   IMPLICIT NONE

   REAL*8, INTENT(IN) :: delta_array(:)
   
   REAL*8, DIMENSION(:), ALLOCATABLE :: r_array(:)
   REAL*8 :: r_mean
   
   INTEGER :: ii, n_values
   
   n_values = SIZE(delta_array) - 1
   
   ALLOCATE(r_array(n_values))
   
   DO ii = 1, n_values 
      r_array = MIN(delta_array(ii), delta_array(ii + 1) )/MAX( delta_array(ii), delta_array(ii + 1))
   END DO
   
   r_mean = SUM(r_array)/n_values
   
   DEALLOCATE(r_array)
   
END FUNCTION


!Function to convert integer to string
CHARACTER(len=20) FUNCTION str(k) 
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: k
    WRITE (str, *) k
    str = ADJUSTL(str)
END FUNCTION str

END MODULE



PROGRAM MAIN

   USE CHECKPOINT_DEBUG_MODULE
   USE MY_MATRIX_MODULE
   USE HERMITIAN_MATRICES_UTILS
   
   IMPLICIT NONE
   
   !Debug flags
   LOGICAL :: DEBUG = .FALSE.
   LOGICAL :: SINGLE_RUN_DEBUG = .FALSE.

   LOGICAL :: PRINT_OUTPUT = .FALSE.
   LOGICAL :: EXIT_ON_ERROR = .FALSE.
   
   !Matrix size variable
   INTEGER :: matrix_size
   !integer that returns info from the diagonalization subroutine
   INTEGER :: diagon_info
   
   
   !ARGUMENTS VARIABLES
   INTEGER :: moving_param 
   INTEGER :: numb_matrices
   CHARACTER(1) :: matrix_type
   CHARACTER(124) :: output_filename
   CHARACTER(8) :: matrix_size_str, numb_matrices_str, moving_param_str
   
   !indices variable
   INTEGER :: ii, jj
   !Arrays for storing the values and matrix 
   REAL*8, DIMENSION(:), ALLOCATABLE :: arrays_eigen, arrays_deltas
   REAL*8, DIMENSION(:), ALLOCATABLE :: arrays_deltas_total, arrays_deltas_local
   TYPE(CMATRIX) :: matrix_A
   
   !Arrays and variables useful for histograms
   TYPE(HISTOGRAM) :: my_single_hist, my_sum_hist
   TYPE(HISTOGRAM), DIMENSION(:), ALLOCATABLE :: hist_arr
   
   !Following flag is TRUE if diagonal, false if Hermitian
   LOGICAL :: diagonal_matrix_flag, hermitian_matrix_flag
   
   REAL*8  :: lower_bound, upper_bound
   INTEGER :: bins_num
   REAL*8, DIMENSION(:), ALLOCATABLE  :: rational_max_min
   
   lower_bound = 0d0
   upper_bound = 3.5d0
   bins_num    = 350
   diagonal_matrix_flag  = .TRUE.
   hermitian_matrix_flag = .FALSE.
   
IF(COMMAND_ARGUMENT_COUNT() < 4) THEN
   PRINT*, "Please provide the following arguments when calling"
   PRINT*, "Matrix size (int)"
   PRINT*, "Number of samples (int)"
   PRINT*, "Hermitian (H) or Diagonal (D)"
   PRINT*, "Global average (int = 0) or Moving average (int = parameter)"
  CALL EXIT(5)
  
ELSE
  
  CALL GET_COMMAND_ARGUMENT(1, matrix_size_str)
  READ(matrix_size_str,*)matrix_size
  CALL GET_COMMAND_ARGUMENT(2, numb_matrices_str)
  READ(numb_matrices_str,*)numb_matrices
  CALL GET_COMMAND_ARGUMENT(3, matrix_type)
  
  
  !Initialize the proper variables
  IF (matrix_type == "H" ) THEN
     diagonal_matrix_flag = .FALSE.
     hermitian_matrix_flag = .TRUE.
  ELSE IF (matrix_type == "D") THEN
     diagonal_matrix_flag = .TRUE.
     hermitian_matrix_flag = .FALSE.
  ELSE IF((matrix_type.NE."H" ).OR.(matrix_type.NE."D")) THEN
     PRINT*, "Please provide the following arguments when calling"
     PRINT*, "Matrix size (int)", "Number of samples (int)", "Hermitian (H) or Diagonal (D)", &
          & "Global average (int = 0) or Moving average (int = parameter)"
     CALL EXIT(5)
  END IF
  
  CALL GET_COMMAND_ARGUMENT(4, moving_param_str)
  READ(moving_param_str,*)moving_param
END IF
   
   !BEGIN OF THE PROGRAM
   ALLOCATE(hist_arr(numb_matrices))
   ALLOCATE(rational_max_min(numb_matrices))
   ALLOCATE(arrays_eigen(matrix_size))
   ALLOCATE(arrays_deltas(matrix_size - 1))
   ALLOCATE(arrays_deltas_total(matrix_size - 1))
   ALLOCATE(arrays_deltas_local(matrix_size - 1))
   
   IF (SINGLE_RUN_DEBUG) THEN
   
      !If the diagonal_matrix_flag is True then initialize a diagonal matrix
      IF (diagonal_matrix_flag) THEN
         matrix_A = INIT_DIAG_MATRIX_RANDOM(matrix_size, matrix_size)
      ELSE IF (hermitian_matrix_flag) THEN
      !Else initialize an Hermitian matrix
         matrix_A = INIT_HERMIT_MATRIX_RANDOM(matrix_size, matrix_size)
      END IF

      IF (DEBUG) THEN
      !check if the matrix is Hermitian 
      !(each element off the diag is conj of its transposed)
         DO ii = 1, matrix_size 
            DO jj = 1, ii
            PRINT*, assert_is_conjugate(matrix_A%element(ii, jj), matrix_A%element(jj, ii),&
                                & PRINT_OUTPUT, EXIT_ON_ERROR)
            END DO
         END DO
         IF (PRINT_OUTPUT) THEN
            CALL PRINT_MATRIX_OUTPUT(matrix_A)
         END IF
      END IF
   
      !Call the subroutine to diagonalize the matrix and obtain the sorted eigenvalues
      CALL HERMITIAN_DIAGONALIZE(matrix_A%element, matrix_size , arrays_eigen, diagon_info)
      IF (DEBUG) THEN
         PRINT*, "Sorted eigenvalues"
         PRINT*, arrays_eigen
         PRINT*, ""
      END IF

      !Call the subroutine to obtain the arrays of Delta's for different eigenvalues
      CALL DELTA_ELEMENTS_ARRAY(arrays_eigen, arrays_deltas)
         IF (DEBUG) THEN
         PRINT*, "Intervals between eigenvalues"
         PRINT*, arrays_deltas
         PRINT*, ""
      END IF

      !Call the subroutine to obtain the normalized spacings wrt to total mean
      CALL NORMALIZED_SPACINGS_TOTAL(arrays_deltas, arrays_deltas_total) 
      IF (DEBUG) THEN
         PRINT*, "Eigenvalues averaged using total average"
         PRINT*, arrays_deltas_total
         PRINT*, ""
      END IF

      !Call the subroutine to obtain the normalized spacings wrt to local means
      CALL NORMALIZED_SPACINGS_LOCAL(arrays_deltas, arrays_deltas_local, moving_param) 
      IF (DEBUG) THEN
         PRINT*, "Eigenvalues averaged using moving average"
         PRINT*, arrays_deltas_local
         PRINT*, ""
      END IF
      
      my_single_hist = INIT_HIST(lower_bound , upper_bound , bins_num)
      CALL FILL_HIST(hist_arr(ii), arrays_deltas_total)
      CALL PRINT_HIST_TO_FILE(my_single_hist, "SINGLE_HIST.DAT")
   
   !If we are not doing the single debug, then
   ELSE
   
   DO ii = 1, numb_matrices
   PRINT*, "Iterating ...", ii, " out of ", numb_matrices, " times..."
      hist_arr(ii) = INIT_HIST(lower_bound , upper_bound , bins_num)
      
      !If the diagonal_matrix_flag is True then initialize a diagonal matrix
      IF (diagonal_matrix_flag) THEN
         matrix_A = INIT_DIAG_MATRIX_RANDOM(matrix_size, matrix_size)
      ELSE IF (hermitian_matrix_flag) THEN
      !Else initialize an Hermitian matrix
         matrix_A = INIT_HERMIT_MATRIX_RANDOM(matrix_size, matrix_size)
      END IF
      
      CALL HERMITIAN_DIAGONALIZE(matrix_A%element, matrix_size , arrays_eigen, diagon_info)
      CALL DELTA_ELEMENTS_ARRAY(arrays_eigen, arrays_deltas)
      
      rational_max_min(ii) = COMPUTE_R_MEAN(arrays_deltas)
      
      !Decide on a base of argument moving_param whether to compute global or local average 
      IF (moving_param.EQ.0) THEN
          CALL NORMALIZED_SPACINGS_TOTAL(arrays_deltas, arrays_deltas_total)
          CALL FILL_HIST(hist_arr(ii), arrays_deltas_total)
      ELSE IF  (moving_param.NE.0)  THEN
!         PRINT*, moving_param
          CALL NORMALIZED_SPACINGS_LOCAL(arrays_deltas, arrays_deltas_local, moving_param)
          CALL FILL_HIST(hist_arr(ii), arrays_deltas_local) 
      END IF

      
   END DO
   
   !Call the function for the hist over a single array of spacings
   my_sum_hist = ADD_HISTOGRAMS_ARRAY(hist_arr)
   
   !In order to obtain a PDF out of the densities entries
   CALL OBTAIN_PDF(my_sum_hist)
   
   output_filename = TRIM("size"//TRIM(str(matrix_size))//"_samples"//&
                     & TRIM(str(numb_matrices))//"_"//matrix_type//"_aver"//TRIM(str(moving_param))//".dat")
   CALL PRINT_HIST_TO_FILE(my_sum_hist, output_filename)
      
   IF (DEBUG) THEN
      PRINT*, char(10), "Counts"
      PRINT*, my_sum_hist%counts
      PRINT*, char(10),"Densities"
      PRINT*, my_sum_hist%densities
      PRINT*, char(10), "Bins"
      PRINT*, my_sum_hist%bins
      PRINT*, char(10), char(10)
   END IF
   
   !End if statement wrt the SINGLE_RUN_DEBUG
   END IF
   
   !Append to a text file the average ratio between eigenvalues"
   OPEN(1, file = "results.dat" , status='unknown', position='append')
   WRITE(1,*) matrix_size, numb_matrices, matrix_type, moving_param, SUM(rational_max_min)/numb_matrices
   CLOSE(1)
   
DEALLOCATE(matrix_A%element)
DEALLOCATE(arrays_eigen)
DEALLOCATE(arrays_deltas)
DEALLOCATE(arrays_deltas_total)
DEALLOCATE(arrays_deltas_local)

END PROGRAM



   
