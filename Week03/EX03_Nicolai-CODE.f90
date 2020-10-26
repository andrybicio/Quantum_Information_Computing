!%%%%%%%%%%%%%%%%%%%%%%%%DOCUMENTATION%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!CHECKPOINT DEBUG MODULE
!Executes tests and checks the output of some operators applied to different 
!types of variables. These types are in particular:
!INTEGER*4, INTEGER*8, LOGICAL, CHARACTERS(*)
!As for now, it only checks whether: variables are equal, are different, 
!are less (or equal), are greater (or equal), are positive, are negative
!have been implemented. New types might be to follow.
!
!For each function called we must give the following
!############ARGUMENTS###############
!args1, (args2): variables, or eventually the variable to be tested
!output_flag: if TRUE, then prints the variables being tested and the operator used
!exit_error: if TRUE, stop and exit the program with error(2)
!
!and the function:
!############RETURNS####################
!test_passed: if TRUE, test has been passed
!
!



MODULE CHECKPOINT_DEBUG_MODULE

IMPLICIT NONE

    INTERFACE assert_eq
        PROCEDURE integer4_assert_eq, integer8_assert_eq, &
        & logical_assert_eq, string_assert_eq
    END INTERFACE
    
    INTERFACE assert_neq
        PROCEDURE integer4_assert_neq, integer8_assert_neq, logical_assert_neq, &
        & string_assert_neq
    END INTERFACE
    
    INTERFACE assert_greater
        PROCEDURE  integer4_assert_greater, integer8_assert_greater 
    END INTERFACE
    
    INTERFACE assert_greater_equal
        PROCEDURE integer4_assert_greater_equal, integer8_assert_greater_equal
    END INTERFACE
    
    INTERFACE assert_less
        PROCEDURE integer4_assert_less, integer8_assert_less
    END INTERFACE
    
    INTERFACE assert_less_equal
        PROCEDURE integer4_assert_less_equal, integer8_assert_less_equal
    END INTERFACE
    
    INTERFACE assert_positive
        PROCEDURE  integer4_assert_positive, integer8_assert_positive 
    END INTERFACE
    
    INTERFACE assert_negative
        PROCEDURE  integer4_assert_positive, integer8_assert_positive 
    END INTERFACE

CONTAINS

   SUBROUTINE OUTPUT_SUBROUTINE(var1_str, var2_str, operat, test_passed)
   
   CHARACTER(*), INTENT(IN) :: var1_str, var2_str
   CHARACTER(*), INTENT(IN) :: operat
   LOGICAL, INTENT(IN) :: test_passed
   CHARACTER(1) :: NEW_LINE_CHAR = char(10)
   
   PRINT*, NEW_LINE_CHAR, " --------------------", &
   & NEW_LINE_CHAR, TRIM(ADJUSTL(var1_str)),"  " , &
   &  operat, TRIM(ADJUSTL(var2_str)), "     -> ", test_passed 
   IF(test_passed.neqv..TRUE.) THEN
   PRINT*, "CHECK FAILED"
   END IF
   PRINT*, "--------------------"
   
   END SUBROUTINE

   !Function to check whether two integers are equal
   !Returns True if are equal
   FUNCTION integer4_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
     IMPLICIT NONE

      INTEGER*4, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 
      
      passed = (arg1.EQ.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed) 
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   FUNCTION integer8_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE

      INTEGER*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = (arg1.EQ.arg2)

      IF (output_flag) THEN
          WRITE(arg1_str, '(I8)') arg1
          WRITE(arg2_str, '(I8)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   !Function to check whether two booleans are equal
   !Returns True if are equal
   FUNCTION logical_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      LOGICAL, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN) :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = (arg1.EQV.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(L2)') arg1
          WRITE(arg2_str, '(L2)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed) 
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   !Function to check whether two strings are equal
   !Returns True if are equal  
   FUNCTION string_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      CHARACTER(*), INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)      :: output_flag
      LOGICAL :: passed, exit_error
      
      passed = (arg1.EQ.arg2)
      
      IF (output_flag) THEN
          CALL OUTPUT_SUBROUTINE(TRIM(arg1), TRIM(arg2), "== ", passed)
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   !Function to check whether two integers are equal
   !Returns True if are not equal   
   FUNCTION integer4_assert_neq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      
      INTEGER*4, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 
      
      passed = (arg1.NE.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "!= ", passed) 
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   FUNCTION integer8_assert_neq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      
      INTEGER*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 
      
      passed = (arg1.NE.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "!= ", passed) 
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   !Function to check whether two booleans are not equal
   !Returns True if are not equal
   FUNCTION logical_assert_neq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      
      LOGICAL, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN) :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = (arg1.NEQV.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(L2)') arg1
          WRITE(arg2_str, '(L2)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "!= ", passed)  
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   !Function to check whether two strings are not equal
   !Returns True if are equal  
   FUNCTION string_assert_neq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      CHARACTER(*), INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)      :: output_flag
      LOGICAL :: passed, exit_error
      
      passed = (arg1.NE.arg2)
      
      IF (output_flag) THEN
          CALL OUTPUT_SUBROUTINE(TRIM(arg1), TRIM(arg2), "!= ", passed)
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION

   !Function to check whether an integer is greater than another one
   !Returns True if arg1 is greater than arg2
   FUNCTION integer4_assert_greater(arg1,arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      INTEGER*4, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
            
      CHARACTER(32) :: arg1_str, arg2_str 
      
      passed = (arg1.GT.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "> ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   FUNCTION integer8_assert_greater(arg1,arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      INTEGER*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = (arg1.GT.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "> ", passed)  
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION

   !Function to check whether an integer is less than another one
   !Returns True if arg1 is less than arg2   
   FUNCTION integer4_assert_less(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*4, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "< ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION

   FUNCTION integer8_assert_less(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "< ", passed)    
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   FUNCTION integer4_assert_positive(arg1, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*4, INTENT(IN) :: arg1
      LOGICAL, INTENT(IN)   :: output_flag
      
      INTEGER*4 :: arg2 = 0
      LOGICAL   :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.GT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "> ", passed) 
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   FUNCTION integer8_assert_positive(arg1, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*8, INTENT(IN) :: arg1
      LOGICAL, INTENT(IN)   :: output_flag
      
      INTEGER*8 :: arg2 = 0
      LOGICAL   :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "> ", passed)  
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   FUNCTION integer4_assert_negative(arg1, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*4, INTENT(IN) :: arg1
      LOGICAL, INTENT(IN)   :: output_flag
      
      LOGICAL :: passed, exit_error
      INTEGER*4 :: arg2 = 0
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "< ", passed)    
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   FUNCTION integer8_assert_negative(arg1, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*8, INTENT(IN) :: arg1
      LOGICAL, INTENT(IN)   :: output_flag
      
      INTEGER*8 :: arg2 = 0
      LOGICAL :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), " < ", passed)  
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION   
   
      FUNCTION integer4_assert_greater_equal(arg1,arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      INTEGER*4, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
            
      CHARACTER(32) :: arg1_str, arg2_str 
      
      passed = (arg1.GE.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), ">= ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
   FUNCTION integer8_assert_greater_equal(arg1,arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      INTEGER*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = (arg1.GE.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), ">= ", passed)  
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION

   !Function to check whether an integer is less than another one
   !Returns True if arg1 is less than arg2   
   FUNCTION integer4_assert_less_equal(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*4, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LE.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I11)') arg1
          WRITE(arg2_str, '(I11)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "<= ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION

   FUNCTION integer8_assert_less_equal(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      INTEGER*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.LE.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "<= ", passed)    
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
   END FUNCTION
   
    
END MODULE 

MODULE MATRIX_UTIL

USE CHECKPOINT_DEBUG_MODULE

IMPLICIT NONE

!Implementation of a new type complex matrix
   TYPE MATRIX
   !Matrix dimension (rows, columns)
       INTEGER, DIMENSION (2) :: dims 
       !2-dim matrix itself
       REAL*4, DIMENSION(:, :), ALLOCATABLE  :: element
   END TYPE MATRIX 

   !Declare indeces
   INTEGER, PRIVATE :: ii,jj,kk

CONTAINS

!Initialize whole elements of the matrix to a predefined value
   FUNCTION INIT_MATRIX_VALUE(rows, columns, myvalue) RESULT(out_matrix)
       IMPLICIT NONE
       
       !Input and temp variables declaration
       INTEGER, INTENT(IN) :: rows, columns
       REAL*4, INTENT(IN)  :: myvalue
       INTEGER :: ii, jj
       
       !Output matrix and memory allocation
       TYPE(MATRIX) :: out_matrix
       
       !Check if dimensions are negative, if so raise an error
       IF (rows .LE. 0 .OR. columns .LE. 0)  THEN
           PRINT*, "Cannot initialize a matrix with null or negative dimensions!"
           CALL EXIT(1)
       END IF
       
       out_matrix%dims(1) = rows
       out_matrix%dims(2) = columns
       ALLOCATE(out_matrix%element(out_matrix%dims(1),out_matrix%dims(2)))

       !Set all elements to that value
       DO jj = 1, out_matrix%dims(2)
          DO ii = 1, out_matrix%dims(1)
             out_matrix%element(ii, jj) = myvalue
          ENDDO
       ENDDO
       RETURN  
   END FUNCTION INIT_MATRIX_VALUE

!Initialize whole elements of the matrix to a random value
   FUNCTION INIT_MATRIX_RANDOM(rows, columns) RESULT(out_matrix)
       IMPLICIT NONE
       
       !Input and temp variables declaration
       INTEGER, INTENT(IN) :: rows, columns
       INTEGER :: ii, jj
       
       !Output matrix and memory allocation
       TYPE(MATRIX) :: out_matrix

       !Check if dimensions are negative, if so raise an error
       IF (rows .LE. 0 .OR. columns .LE. 0)  THEN
           PRINT*, "Cannot initialize a matrix with null or negative dimensions!"
           CALL EXIT(1)
       END IF
       
       out_matrix%dims(1) = rows
       out_matrix%dims(2) = columns
       ALLOCATE(out_matrix%element(out_matrix%dims(1),out_matrix%dims(2)))
       
       !Each element is drawn from a [-1,1] distribution 
       DO jj = 1, out_matrix%dims(2)
          DO ii = 1, out_matrix%dims(1)
             out_matrix%element(ii, jj) = 2*(RAND(0) - 0.5)
          ENDDO
       ENDDO
   END FUNCTION INIT_MATRIX_RANDOM

!A is lhs matrix, B rhs second, C is the resulting one
!first index "i" runs slower in c_ij 
FUNCTION matrix_product_def(matrixA, matrixB) result(matrixC)
   INTEGER :: NN
   TYPE(MATRIX), INTENT(IN) :: matrixA, matrixB
   TYPE(MATRIX)             :: matrixC
   
   NN = matrixA%dims(1)
   matrixC = INIT_MATRIX_VALUE(NN,NN, 0.)
      
   DO ii = 1,NN
      DO jj = 1,NN
        DO kk = 1, NN
           matrixC%element(ii,jj) = matrixC%element(ii,jj) &
           & + matrixA%element(ii,kk)*matrixB%element(kk,jj)
        ENDDO
      ENDDO
   ENDDO
END FUNCTION

!Here it is the "second way", i.e. the one with transposed matrix
!first index "i" runs faster in c_ij 
FUNCTION matrix_product_transposed(matrixA, matrixB) result(matrixC)
   INTEGER :: NN
   TYPE(MATRIX), INTENT(IN) :: matrixA, matrixB
   TYPE(MATRIX)             :: matrixC
   
   NN = matrixA%dims(1)
   matrixC = INIT_MATRIX_VALUE(NN,NN, 0.)
   
   DO kk = 1,NN 
      DO jj = 1,NN
        DO ii = 1, NN
         matrixC%element(ii,jj) = matrixC%element(ii,jj) &
           & + matrixA%element(ii,kk)*matrixB%element(kk,jj)
        ENDDO
      ENDDO
   ENDDO
END FUNCTION

END MODULE

!This program computes the times spent in making matrical product in different 
!ways: 
!-Definition of matricial product
!-Matricial product using transposed matrices
!-Intrinsic function of Fortran90
!
!When run the executable two arguments might be provided:
!-size of the matrix
!-filename for output where to store the size of matrices and time spent 
!
!If arguments are not provided, then the size is set by default to 100 
!and output file will not be created.

PROGRAM TEST 

USE MATRIX_UTIL
USE CHECKPOINT_DEBUG_MODULE

IMPLICIT NONE 

CHARACTER(10) :: filename
CHARACTER(5) :: nnstring
CHARACTER(4) :: file_extension = ".dat"
CHARACTER(5) :: name_string
LOGICAL :: FILE_OUTPUT 

INTEGER :: size_matrix
REAL :: time_delta1
REAL :: time_delta2
REAL :: time_delta3
REAL :: start_time, finish_time

!Debug flags
LOGICAL :: DEBUG = .TRUE.
LOGICAL :: DEBUG_PRINT_OUTPUT = .TRUE.
LOGICAL :: EXIT_ON_ERROR = .TRUE.


!Variables for storing size of the matrix and matrices themselves 
TYPE(MATRIX) :: matrixA, matrixB, matrixC

!If present when calling the functions,
!first argument is the size of the matrix and second the output filename
IF(COMMAND_ARGUMENT_COUNT() < 1) THEN
!set a default size
  size_matrix = 100
  FILE_OUTPUT = .FALSE.
ELSE
  CALL GET_COMMAND_ARGUMENT(1, nnstring)
  READ(nnstring,*)size_matrix
  CALL GET_COMMAND_ARGUMENT(2, name_string)
  filename = TRIM(name_string) // file_extension
ENDIF

!PRE-CONDITIONS step1
!Check whether values for size matrix is positive
IF (DEBUG) THEN
   PRINT*, "Check if matrix size variable is positive"
   PRINT*, assert_positive(size_matrix, DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
END IF

matrixA = INIT_MATRIX_RANDOM(size_matrix, size_matrix)
matrixB = INIT_MATRIX_RANDOM(size_matrix, size_matrix)

!PRE-CONDITIONS step2
!Check whether initialized matrices are square
IF (DEBUG) THEN
   PRINT*, "Check whether matrices are square"
   PRINT*, "Matrix A ->", assert_eq(matrixB%dims(1), matrixA%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Matrix B ->", assert_eq(matrixB%dims(1), matrixB%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
END IF

CALL CPU_TIME(start_time)
matrixC = matrix_product_def(matrixA, matrixB)

!POST-CONDITIONS step
IF (DEBUG) THEN
   PRINT*, "Check whether resulting matrix is square"
   PRINT*, assert_eq(matrixC%dims(1), matrixC%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Check whether resulting matrix has compatible dims wrt input matrices"
   PRINT*, assert_eq(matrixC%dims(1), matrixA%dims(1), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR), &
   & assert_eq(matrixC%dims(2), matrixB%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
END IF

CALL CPU_TIME(finish_time)

time_delta1 = finish_time - start_time
PRINT*, "Time passed using definition", time_delta1, "sec"

CALL CPU_TIME(start_time)
matrixC = matrix_product_transposed(matrixA, matrixB)
!POST-CONDITIONS step
IF (DEBUG) THEN
   PRINT*, "Check whether resulting matrix is square"
   PRINT*,  assert_eq(matrixC%dims(1), matrixC%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Check whether resulting matrix has same dims as input matrices"
   PRINT*,  assert_eq(matrixC%dims(1), matrixA%dims(1), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR), &
   & assert_eq(matrixC%dims(2), matrixB%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
END IF
CALL CPU_TIME(finish_time)

time_delta2 = finish_time - start_time
PRINT*, "Time passed using transposed matrices", time_delta2, "sec"

CALL CPU_TIME(start_time)
matrixC = INIT_MATRIX_VALUE(size_matrix, size_matrix, 0.)
matrixC%element = matmul(matrixA%element, matrixB%element)
!POST-CONDITIONS step
IF (DEBUG) THEN
   PRINT*, "Check whether resulting matrix is square"
   PRINT*, assert_eq(matrixC%dims(1), matrixC%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Check whether resulting matrix has same dims as input matrices"
   PRINT*, assert_eq(matrixC%dims(1), matrixA%dims(1), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR), &
   & assert_eq(matrixC%dims(2), matrixB%dims(2), DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
END IF
CALL CPU_TIME(finish_time)

time_delta3 = finish_time - start_time
PRINT*, "Time passed using intrinsic function", time_delta3, "sec"

!create an output data file whose entries are:
!size of the matrix, 
!time spent with the definition of matricial product
!time spent using transposed matrices product
!time spent using the intrinsic function
IF (FILE_OUTPUT .eqv. .TRUE.) THEN
    !output data into a file
    OPEN(1, file = filename , status='unknown', position='append')      
    WRITE(1,*) size_matrix, time_delta1, time_delta2, time_delta3 
    CLOSE(1) 
END IF

!Deallocate space
DEALLOCATE(matrixA%element)
DEALLOCATE(matrixB%element)
DEALLOCATE(matrixC%element)

END PROGRAM
