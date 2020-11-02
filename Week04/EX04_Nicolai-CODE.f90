MODULE CHECKPOINT_DEBUG_MODULE

IMPLICIT NONE
    INTERFACE assert_is_type
        PROCEDURE dummy_assert_is_type, integer4_assert_is_type, &
        & integer8_assert_is_type, logical_assert_is_type
    END INTERFACE

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
   &  operat,"  " ,TRIM(ADJUSTL(var2_str)), "     -> ", test_passed 
   IF(test_passed.neqv..TRUE.) THEN
   PRINT*, "CHECK FAILED"
   END IF
   PRINT*, "--------------------"
   
   END SUBROUTINE
   
   FUNCTION dummy_assert_is_type(arg1, type_string, output_flag, exit_error) RESULT (passed)
     IMPLICIT NONE

      CLASS(*), INTENT(IN)      :: arg1
      CHARACTER(*), INTENT(IN)  :: type_string

      LOGICAL, INTENT(IN)   :: output_flag, exit_error
      LOGICAL :: passed
      LOGICAL :: is_int4, is_int8, is_logical
      CHARACTER(32) :: arg1_str 
      
      
      is_int4 = .FALSE.
      is_int8 = .FALSE.
      is_logical = .FALSE.
      
      SELECT TYPE (arg1)
         TYPE IS (INTEGER(4))
             is_int4 = integer4_assert_is_type(arg1)
         TYPE IS (INTEGER(8))
             is_int8 = integer8_assert_is_type(arg1)
         TYPE IS (LOGICAL)
             is_logical = logical_assert_is_type(arg1)
      END SELECT
      
      passed = .FALSE.
      
      SELECT CASE (type_string)
         CASE ("int4")
             passed = is_int4
         CASE ("int8")
             passed = is_int8
         CASE ("logical")
             passed = is_logical
      END SELECT
      
      IF (output_flag) THEN
         SELECT TYPE (arg1)
            TYPE IS (INTEGER(4))
               WRITE(arg1_str, '(I11)') arg1 
            TYPE IS (INTEGER(8))
                WRITE(arg1_str, '(I19)') arg1
            TYPE IS (LOGICAL)
                WRITE(arg1_str, '(L2)') arg1
      END SELECT
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), " is of type  ", TRIM(type_string) ,  passed) 
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
         
   END FUNCTION
   
   
   !Return True if the variable is an integer*4 type
   FUNCTION integer4_assert_is_type(arg1) RESULT(passed)
      IMPLICIT NONE
      INTEGER*4, INTENT(IN) :: arg1
      
      LOGICAL :: passed
 
      passed = .TRUE.

   END FUNCTION   
   
   !Return True if the variable is an integer*8 type
   FUNCTION integer8_assert_is_type(arg1) RESULT(passed)
      IMPLICIT NONE
      INTEGER*8, INTENT(IN) :: arg1
      LOGICAL :: passed
      
      passed = .TRUE.

   END FUNCTION   
   
   
   !Return True if the variable is logical type
   FUNCTION logical_assert_is_type(arg1) RESULT(passed)
      IMPLICIT NONE
      LOGICAL, INTENT(IN)   :: arg1
      
      LOGICAL :: passed

      passed = .TRUE.
      
   END FUNCTION   
   

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
          WRITE(arg1_str, '(I18)') arg1
          WRITE(arg2_str, '(I18)') arg2
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

PROGRAM MATRIX_MULTIPLICATION_SCRIPTS

USE MATRIX_UTIL
USE CHECKPOINT_DEBUG_MODULE

IMPLICIT NONE 

CHARACTER(32) :: input_name

INTEGER :: size_matrix
REAL :: time_delta1
REAL :: time_delta2
REAL :: time_delta3
REAL :: start_time, finish_time

!Debug flags
LOGICAL :: DEBUG = .FALSE.
LOGICAL :: DEBUG_PRINT_OUTPUT = .FALSE.
LOGICAL :: EXIT_ON_ERROR = .FALSE.

!Variables for storing size of the matrix and matrices themselves 
TYPE(MATRIX) :: matrixA, matrixB, matrixC

!If present when calling the functions,
!argument is the name of the file for matrix size in input,
!Otherwise it is set to 100 by default
IF(COMMAND_ARGUMENT_COUNT() < 1) THEN
  !set a default size
  size_matrix = 100
  PRINT*, "No arguments passed, setting default size (100)"
ELSE
  CALL GET_COMMAND_ARGUMENT(1, input_name)
  OPEN (UNIT = 1, FILE = TRIM(input_name))
  READ (UNIT = 1, FMT=*) size_matrix
  PRINT*,"Getting input from ", TRIM(input_name), "   matrix size is ", size_matrix
  !PRE-CONDITIONS step0
  !Check whether the size_matrix is either int4 or int8
   IF (DEBUG) THEN
      PRINT*, "Check whether the size_matrix is of type int4"
      PRINT*, assert_is_type(size_matrix, "int4" , DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
   END IF
END IF

!PRE-CONDITIONS step1
!Check whether values for size matrix is positive
IF (DEBUG) THEN
   PRINT*, "Check if matrix size variable is positive"
   PRINT*, assert_positive(size_matrix, DEBUG_PRINT_OUTPUT, EXIT_ON_ERROR)
END IF

!Open files for storing the results
OPEN(2, file = "definition.dat" , status='unknown', position='append')      
OPEN(3, file = "transposed.dat" , status='unknown', position='append')      
OPEN(4, file = "intrinsic.dat"  , status='unknown', position='append')      

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

!Write times spent used different methodsfor matrix multiplications
WRITE(2,*) size_matrix, time_delta1
WRITE(3,*) size_matrix, time_delta2 
WRITE(4,*) size_matrix, time_delta3 
CLOSE(2)
CLOSE(3)
CLOSE(4) 


!Deallocate space
DEALLOCATE(matrixA%element)
DEALLOCATE(matrixB%element)
DEALLOCATE(matrixC%element)

END PROGRAM
