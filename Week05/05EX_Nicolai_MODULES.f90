MODULE CHECKPOINT_DEBUG_MODULE

IMPLICIT NONE
    INTERFACE assert_is_type
        PROCEDURE dummy_assert_is_type, integer4_assert_is_type, &
        & integer8_assert_is_type, logical_assert_is_type
    END INTERFACE

    INTERFACE assert_eq
        PROCEDURE integer4_assert_eq, integer8_assert_eq, &
        & logical_assert_eq, string_assert_eq, &
        & complex_assert_eq, double_complex_assert_eq, &
        & real_assert_eq, double_real_assert_eq 
    END INTERFACE
    
    INTERFACE assert_is_conjugate
        PROCEDURE complex_assert_is_conj, double_complex_assert_is_conj
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
   
   FUNCTION real_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)

      IMPLICIT NONE

      REAL*4, INTENT(IN) :: arg1, arg2
      REAL*4 :: eps = 1e-6
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = .FALSE.
      
      IF ((ABS(arg1 - arg2) .LE. eps).OR.(ABS(arg1 - arg2).LE.(eps*MAX(arg1,arg2)))) THEN
         passed = .TRUE.
      END IF

      IF (output_flag) THEN
          WRITE(arg1_str, '(f16.6)') arg1
          WRITE(arg2_str, '(f16.6)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION

   FUNCTION double_real_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE

      REAL*8, INTENT(IN) :: arg1, arg2
      REAL*8 :: eps = 1e-15

      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = .FALSE.
      
      IF ((ABS(arg1 - arg2) .LE. eps).OR.(ABS(arg1 - arg2).LE.(eps*MAX(arg1,arg2)))) THEN
         passed = .TRUE.
      END IF

      IF (output_flag) THEN
          WRITE(arg1_str, '(f20.15)') arg1
          WRITE(arg2_str, '(f20.15)') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
      

   FUNCTION complex_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE

      COMPLEX, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(64) :: arg1_str, arg2_str 
      REAL*4 :: eps = 1e-6


      passed = .FALSE.
      
      IF ((ABS(REAL(arg1) - REAL(arg2)).LE.eps).OR.&
      &((ABS(REAL(arg1) - REAL(arg2)).LE.MAX(REAL(arg1),REAL(arg2))*eps)).AND.&
      &(ABS(AIMAG(arg1)-AIMAG(arg1)).LE.eps).OR.&
      &(ABS(AIMAG(arg1) - AIMAG(arg2)).LE.MAX(AIMAG(arg1),AIMAG(arg2))*eps)) THEN
         passed = .TRUE.
      END IF

      IF (output_flag) THEN
          WRITE(arg1_str, '(f16.6,",",f16.6,"i")') arg1
          WRITE(arg2_str, '(f16.6,",",f16.6,"i")') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   FUNCTION double_complex_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE

      DOUBLE COMPLEX, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      CHARACTER(64) :: arg1_str, arg2_str 
      REAL*8 :: eps = 1e-15

      passed = .FALSE.
            
      IF ((ABS(REAL(arg1) - REAL(arg2)).LE.eps).OR.&
      &((ABS(REAL(arg1) - REAL(arg2)).LE.MAX(REAL(arg1),REAL(arg2))*eps)).AND.&
      &(ABS(AIMAG(arg1)-AIMAG(arg1)).LE.eps).OR.&
      &(ABS(AIMAG(arg1) - AIMAG(arg2)).LE.MAX(AIMAG(arg1),AIMAG(arg2))*eps)) THEN
         passed = .TRUE.
      END IF

      IF (output_flag) THEN
          WRITE(arg1_str, '(F20.15,",",F20.15,"i")') arg1
          WRITE(arg2_str, '(F20.15,",",F20.15,"i")') arg2
          CALL OUTPUT_SUBROUTINE(TRIM(arg1_str), TRIM(arg2_str), "== ", passed)   
      END IF
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
   END FUNCTION
   
   FUNCTION complex_assert_is_conj(arg1, arg2, output_flag, exit_error) RESULT(passed)
   IMPLICIT NONE

      COMPLEX, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
   passed = assert_eq(arg1, CONJG(arg2), output_flag, exit_error)

   END FUNCTION
   
   
      FUNCTION double_complex_assert_is_conj(arg1, arg2, output_flag, exit_error) RESULT(passed)
   IMPLICIT NONE

      DOUBLE COMPLEX, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
      
   passed = assert_eq(arg1, CONJG(arg2), output_flag, exit_error)
   
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

MODULE HISTOGRAM_MODULE

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


END MODULE 








MODULE MY_MATRIX_MODULE

IMPLICIT NONE

!Implementation of a new type complex matrix
   TYPE CMATRIX
   !Matrix dimension (rows, columns)
       INTEGER, DIMENSION (2) :: dims 
       !2-dim matrix itself
       DOUBLE COMPLEX, DIMENSION(:, :), ALLOCATABLE  :: element
       !variable to store trace and determinant
       DOUBLE COMPLEX :: trace, determinant
   END TYPE CMATRIX 
   
!Defining the operators for this module, external programs will see this
   INTERFACE OPERATOR(.Adj.)
       MODULE PROCEDURE Mat_Adj
   END INTERFACE
   
   INTERFACE OPERATOR(.Trace.)
       MODULE PROCEDURE Mat_Trace
   END INTERFACE
   
   !Declare indeces
   INTEGER, PRIVATE :: ii,jj

CONTAINS

!Initialize whole elements of the matrix to a random value
   FUNCTION INIT_HERMIT_MATRIX_RANDOM(rows, columns) RESULT(out_matrix)
       IMPLICIT NONE
       
       !Input and temp variables declaration
       INTEGER, INTENT(IN) :: rows, columns
       
       !Output matrix and memory allocation
       TYPE(CMATRIX) :: out_matrix
       REAL*8 :: a, b
       
       !Check if dimensions are negative, if so raise an error
       IF (rows .LE. 0 .OR. columns .LE. 0)  THEN
           PRINT*, "Cannot initialize a matrix with null or negative dimensions!"
           CALL EXIT(1)
       END IF
       
       out_matrix%dims(1) = rows
       out_matrix%dims(2) = columns
       
       ALLOCATE(out_matrix%element(out_matrix%dims(1),out_matrix%dims(2)))
       
       !Each element is drawn from a [-1,1] distribution 
       DO ii = 1, rows
             
             DO jj = ii, columns
                a = 2*(RAND(0) - 0.5)
                b = 2*(RAND(0) - 0.5)
                
                IF (jj == ii) THEN
                   out_matrix%element(ii, jj) = DCMPLX(a, 0)
                ELSE
                   out_matrix%element(ii, jj) = DCMPLX(a, b)
                   out_matrix%element(jj, ii) = DCMPLX(a, -b)
                END IF
             END DO
       END DO 
   END FUNCTION INIT_HERMIT_MATRIX_RANDOM
   
   
!Initialize whole elements of the matrix to a random value
   FUNCTION INIT_DIAG_MATRIX_RANDOM(rows, columns) RESULT(out_matrix)
       IMPLICIT NONE
       
       !Input and temp variables declaration
       INTEGER, INTENT(IN) :: rows, columns
       
       !Output matrix and memory allocation
       TYPE(CMATRIX) :: out_matrix
       REAL*8 :: a
       
       !Check if dimensions are negative, if so raise an error
       IF (rows .LE. 0 .OR. columns .LE. 0)  THEN
           PRINT*, "Cannot initialize a matrix with null or negative dimensions!"
           CALL EXIT(1)
       END IF
       
       out_matrix%dims(1) = rows
       out_matrix%dims(2) = columns
       
       ALLOCATE(out_matrix%element(out_matrix%dims(1),out_matrix%dims(2)))
       
       !Each element is drawn from a [-1,1] distribution 
       DO ii = 1, rows
             DO jj = ii, columns
                IF (jj == ii) THEN
                   a = 2*(RAND(0) - 0.5)
                   out_matrix%element(ii, jj) = DCMPLX(a,  0.)
                ELSE
                   out_matrix%element(ii, jj) = DCMPLX(0., 0.)
                   out_matrix%element(jj, ii) = DCMPLX(0., 0.)
                END IF
             END DO
       END DO 
   END FUNCTION INIT_DIAG_MATRIX_RANDOM
   
   

!Subroutine to print the matrix to a file txt
   SUBROUTINE PRINT_TO_FILE(in_matrix, name_string)
      IMPLICIT NONE
  
      TYPE(CMATRIX), INTENT(IN) :: in_matrix
      CHARACTER(10), INTENT(IN) :: name_string
      INTEGER :: ii, jj
  
      CHARACTER*1 NEWLINE
      NEWLINE = char(10)
  
      OPEN(25, file = name_string , status = 'unknown')      
      DO ii = 1, in_matrix%dims(1)
         DO jj = 1, in_matrix%dims(2)
                   WRITE(25, "(a, ES9.2)", ADVANCE = "NO") "(" , REAL(in_matrix%element(ii,jj))
                   WRITE(25, "(a, ES9.2)", ADVANCE = "NO") "," , AIMAG(in_matrix%element(ii,jj))
                   WRITE(25, "(a,a)", ADVANCE = "NO") ")" , "	"
         ENDDO
         !begin a new line, that is a new row for the matrix itself
         WRITE (25, "(a)", ADVANCE="NO") NEWLINE
      ENDDO
  
      !Only if the matrix is square, then print Trace and Determinant
      IF (in_matrix%dims(1) .EQ. in_matrix%dims(2)) THEN
         WRITE(25, "(a, a,  ES10.2, a, ES10.2, a)") NEWLINE, "Trace:  (", &
    &     REAL(Mat_Trace(in_matrix)), "," , AIMAG(Mat_Trace(in_matrix)), ")"
         WRITE(25, "(a, ES10.2, a, ES10.2, a)") "Determinant:  (", &
    &     REAL(in_matrix%determinant),"," ,AIMAG(in_matrix%determinant), ")"
      END IF
  
          CLOSE(25) 
      RETURN
    
   END SUBROUTINE PRINT_TO_FILE
   
!Subroutine to print the matrix to a file txt
   SUBROUTINE PRINT_MATRIX_OUTPUT(in_matrix)
      IMPLICIT NONE
  
      TYPE(CMATRIX), INTENT(IN) :: in_matrix
      INTEGER :: ii, jj
  
      CHARACTER*1 NEWLINE
      NEWLINE = char(10)
  
      DO ii = 1, in_matrix%dims(1)
         DO jj = 1, in_matrix%dims(2)
            WRITE(*, "(a, ES9.2)", ADVANCE = "NO") "(" , REAL(in_matrix%element(ii,jj))
            WRITE(*, "(a, ES9.2)", ADVANCE = "NO") "," , AIMAG(in_matrix%element(ii,jj))
            WRITE(*, "(a,a)", ADVANCE = "NO") ")" , "	"
         ENDDO
         WRITE(*, "(a)") NEWLINE
      ENDDO
      RETURN
    
   END SUBROUTINE PRINT_MATRIX_OUTPUT
   
   
!  Function for computing the adjoint matrix
   FUNCTION Mat_Adj(in_matrix) RESULT(adj_matrix)            
       IMPLICIT NONE
       
       TYPE(CMATRIX), INTENT(IN) :: in_matrix
       TYPE(CMATRIX) :: adj_matrix
       
       !Transpose the matrix
       adj_matrix%dims(2) = in_matrix%dims(1)
       adj_matrix%dims(1) = in_matrix%dims(2)
       ALLOCATE(adj_matrix%element(adj_matrix%dims(1),adj_matrix%dims(2)))
       
      !Adjoint matrix is defined only when matrix is square
      IF (in_matrix%dims(1) .NE. in_matrix%dims(2)) THEN
          PRINT*,"The matrix is not square! Adjoint matrix is not defined."
          DEALLOCATE (adj_matrix%element)
          RETURN
      ELSE
       !Use the intrinsic function
       adj_matrix%element = TRANSPOSE(CONJG(in_matrix%element))
       
       !Note as if elements change, the value does not update
       adj_matrix%trace = .Trace.adj_matrix
       !Let assign it this value for now
       adj_matrix%determinant = (0.0, 0.0)
      END IF 
               
   END FUNCTION MAT_ADJ
   
  !Function for computing the trace of the matrix
   FUNCTION  Mat_Trace(in_matrix) RESULT(trace)
      IMPLICIT NONE
      
      TYPE(CMATRIX), INTENT(IN) :: in_matrix
      DOUBLE COMPLEX :: trace
      INTEGER :: ii
      
      trace = (0.0, 0.0)
      
      !Trace is defined only when the matrix is square,
      !If not, then return a null value (NaN in Fortran is not available)
      IF (in_matrix%dims(1) .NE. in_matrix%dims(2)) THEN
          PRINT*,"The matrix is not square! Trace is therefore set to 0"
          RETURN
      ELSE
          DO ii = 1, in_matrix%dims(1)
              trace = trace + in_matrix%element(ii,ii)
          END DO
          RETURN
      END IF 
          
      RETURN
  END FUNCTION


END MODULE
