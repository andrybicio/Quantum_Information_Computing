MODULE CHECKPOINT_DEBUG_MODULE
   USE MY_MATRIX_MODULE
   
IMPLICIT NONE
    INTERFACE assert_is_type
        PROCEDURE dummy_assert_is_type, integer4_assert_is_type, &
        & integer8_assert_is_type, logical_assert_is_type
    END INTERFACE

    INTERFACE assert_eq
        PROCEDURE integer4_assert_eq, integer8_assert_eq, &
        & logical_assert_eq, string_assert_eq, &
        & complex_assert_eq, double_complex_assert_eq, &
        & real_assert_eq, double_real_assert_eq , cmatrix_assert_eq
    END INTERFACE
    
    INTERFACE assert_is_conjugate
        PROCEDURE complex_assert_is_conj, double_complex_assert_is_conj
    END INTERFACE
    
    INTERFACE assert_is_adjoint
        PROCEDURE double_complex_assert_is_adj
    END INTERFACE

    INTERFACE assert_neq
        PROCEDURE integer4_assert_neq, integer8_assert_neq, logical_assert_neq, &
        & string_assert_neq
    END INTERFACE
    
    INTERFACE assert_greater
        PROCEDURE  integer4_assert_greater, integer8_assert_greater, &
        & real8_assert_greater
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
        PROCEDURE  integer4_assert_positive, integer8_assert_positive, &
        & real8_assert_positive
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
   
   
FUNCTION cmatrix_assert_eq(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE

      TYPE(CMATRIX), INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      INTEGER :: ii,jj 
      LOGICAL :: passed, exit_error
      CHARACTER(64) :: arg1_str, arg2_str       

      passed = .FALSE.
      
      !Checking if dimensions are the same
      IF (output_flag) THEN
         PRINT*, "Checking whether dimensions are equal"
      END IF
      passed = assert_eq(arg1%dims(1), arg2%dims(1), output_flag, exit_error).AND.&
               & assert_eq(arg1%dims(2), arg2%dims(2), output_flag, exit_error)
      
      IF (exit_error.AND.(passed.EQV..FALSE.)) THEN
          CALL EXIT(2)
      END IF
      
      !Checking over elements
      IF (output_flag) THEN
         PRINT*, "Checking whether elements are equal"
      END IF        
      DO ii = 1, arg1%dims(2)
         DO jj = 1, arg1%dims(1)
            passed = assert_eq(arg1%element(ii,jj),arg2%element(ii,jj), output_flag, exit_error)
         END DO
      END DO
      
      IF (output_flag) THEN
          arg1_str = "Double complex Matrix1"
          arg2_str = "Double complex Matrix2"
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
   
   FUNCTION double_complex_assert_is_adj(arg1, arg2, output_flag, exit_error) RESULT(passed)
   IMPLICIT NONE
   
      DOUBLE COMPLEX, DIMENSION(:,:), INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error, precondition_check
      
      !index
      INTEGER :: ii, jj, matrix_size
      INTEGER :: dim1A, dim2A, dim1B, dim2B
   
   
   passed = .FALSE.
   dim1A = SIZE(arg1,1)
   dim2A = SIZE(arg2,1)
   dim1B = SIZE(arg1,2)
   dim2B = SIZE(arg2,2)
   
   precondition_check = assert_eq(dim1A,dim2A,output_flag,exit_error).AND.&
    & assert_eq(dim1B,dim2B, output_flag, exit_error)
   IF (precondition_check) THEN
   
   matrix_size = SIZE(arg1,1)

   DO ii = 1, matrix_size 
       DO jj = 1, ii
       passed = assert_is_conjugate(arg1(ii, jj), arg2(jj, ii), &
                                & output_flag, exit_error)
       END DO
   END DO
   ELSE IF (.NOT.precondition_check) THEN
      PRINT*, "Matrices do not have the same dimension!"
      IF (exit_error) THEN
         CALL EXIT(3)
      END IF
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

   FUNCTION real8_assert_greater(arg1, arg2, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
   
      REAL*8, INTENT(IN) :: arg1, arg2
      LOGICAL, INTENT(IN)   :: output_flag
      LOGICAL :: passed, exit_error
            
      CHARACTER(32) :: arg1_str, arg2_str 
      
      passed = (arg1.GE.arg2)
      
      IF (output_flag) THEN
          WRITE(arg1_str, '(G0)') arg1
          WRITE(arg2_str, '(G0)') arg2
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
   
   FUNCTION real8_assert_positive(arg1, output_flag, exit_error) RESULT(passed)
      IMPLICIT NONE
      REAL*8, INTENT(IN) :: arg1
      LOGICAL, INTENT(IN)   :: output_flag
      
      REAL*8    :: arg2 = 0d0
      LOGICAL   :: passed, exit_error
      CHARACTER(32) :: arg1_str, arg2_str 

      passed = arg1.GT.arg2

      IF (output_flag) THEN
          WRITE(arg1_str, '(G0)') arg1
          WRITE(arg2_str, '(G0)') arg2
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
   SUBROUTINE PRINT_CMATRIX_OUTPUT(in_matrix)
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
    
   END SUBROUTINE PRINT_CMATRIX_OUTPUT
   
   !Subroutine to print the matrix to a file txt
   SUBROUTINE PRINT_REAL_MATRIX_OUTPUT(in_matrix)
      IMPLICIT NONE
  
      REAL*8, DIMENSION(:,:), INTENT(IN) :: in_matrix
      INTEGER :: ii, jj
  
      CHARACTER*1 NEWLINE
      NEWLINE = char(10)
      
      
      
      DO ii = 1, SIZE(in_matrix, 1)
         DO jj = 1, SIZE(in_matrix, 1)
            WRITE(*, "(a, ES9.2)", ADVANCE = "NO") "(" , in_matrix(ii,jj)
            WRITE(*, "(a,a)", ADVANCE = "NO") ")" , "	"
         ENDDO
         WRITE(*, "(a)") NEWLINE
      ENDDO
      RETURN
    
   END SUBROUTINE PRINT_REAL_MATRIX_OUTPUT
   
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

MODULE HERMITIAN_MATRICES_UTILS
   
   USE MY_MATRIX_MODULE
   !Declaration for indexes
   INTEGER, PRIVATE :: ii, jj

CONTAINS

!Subroutine that takes is input the matrix to be diagonalized
!and returns array of sorted eigenvalues
SUBROUTINE HERMITIAN_EIGENVALUES(matrix, matrix_size, array_eigenvalues, info)

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
   
   !CALL SHELL_SORT_ARRAY( array_eigenvalues )

END SUBROUTINE

!Subroutine that takes is input the matrix to be diagonalized
!and returns eigenvectors of matrix stored columnwise
SUBROUTINE HERMITIAN_EIGENVECTORS(matrix, matrix_size, array_eigenvalues, info)

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
   
   CALL ZHEEV('V', 'U', matrix_size, matrix, matrix_size, array_eigenvalues, &
              & work, lwork, rwork, info)     
   lwork = int(real(work(1)))
   DEALLOCATE(work)
    
   !reallocate in order to use the values just found
   !this call does the dirty job
   ALLOCATE(work(MAX(1,lwork)))
   CALL ZHEEV('V', 'U', matrix_size, matrix, matrix_size, array_eigenvalues, &
              & work, lwork, rwork, info)

   DEALLOCATE(work)
   DEALLOCATE(rwork)
      
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

!Initialization for a laplacian discrete operator matrix with Dirichlet boundary conditions
FUNCTION discrete_laplacian_oper(matrix_size) RESULT(laplacian)
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: matrix_size
    TYPE(CMATRIX) :: laplacian
    
    IF(.not.ALLOCATED(laplacian%element)) THEN
       ALLOCATE(laplacian%element(matrix_size,matrix_size))
    END IF
    
    laplacian%dims(1) = matrix_size
    laplacian%dims(2) = matrix_size
    
    laplacian%element = 0d0
    DO ii = 2, matrix_size
       laplacian%element(ii    , ii - 1) = 1d0
       laplacian%element(ii    ,     ii) = -2d0
       laplacian%element(ii - 1,     ii) = 1d0
    END DO
    
    laplacian%element(1,1) = -2d0

END FUNCTION

FUNCTION discrete_potential_oper(n_points, dx, omega, mass, debug_flag) RESULT(potential)
   IMPLICIT NONE
   
   TYPE(CMATRIX) :: potential
   !Lattice quantities
   !dx is the spacing,
   !length_number is how many dx's such that our system has length 2*length_number*dx
   REAL*8, INTENT(IN)  :: dx
   INTEGER, INTENT(IN) :: n_points
   LOGICAL, INTENT(IN) :: debug_flag
   
   !Physical quantities
   REAL*8, INTENT(IN) :: omega, mass
   
   !x_ii is the actual position wrt the origin
   REAL*8 :: x_ii, ii_real, x_ii_min 
   
   IF(.not.ALLOCATED(potential%element)) THEN
      ALLOCATE(potential%element(n_points,n_points))
   END IF
   
   potential%dims(1) = n_points
   potential%dims(2) = n_points
    
   potential%element = 0d0
   ii_real = 1d0
   
   !consider the first point and cast to real*8
   x_ii_min = -0.5d0*REAL(n_points-1, 8)*dx
   
   DO ii = 1, n_points
      !iterate over the points
      x_ii = x_ii_min + (ii_real - 1d0)*dx
      !update the index of real type
      ii_real = ii_real + 1d0
      !compute the potential
      potential%element(ii,ii) = 0.5*mass*(omega*x_ii)**2
   ENDDO
   
   ii_real = 1d0
   OPEN(UNIT = 25, FILE = "potential.dat", ACTION = "write", STATUS = "replace")
   IF (debug_flag) THEN
      DO ii = 1, n_points
      !iterate over the points
      x_ii = x_ii_min +  + (ii_real - 1d0)*dx
      !update the index of real type
      ii_real = ii_real + 1d0
      WRITE(25,*) x_ii, ",", 0.5*mass*(omega*x_ii)*(omega*x_ii)
      END DO
   END IF
   CLOSE(25)
   
END FUNCTION

SUBROUTINE print_eigenvalues_to_file(eigenvalues_array, filename, num_eig_input)
   
   CHARACTER(*), INTENT (IN)       :: filename
   REAL*8, DIMENSION(:), INTENT(IN):: eigenvalues_array
   INTEGER, INTENT(IN) :: num_eig_input
   
   INTEGER :: num_eig_variable
   
   
   !If it is not specified (k==0), print all eigenvalues
   IF (num_eig_input.EQ.0) THEN
      num_eig_variable = SIZE(eigenvalues_array)
   ELSE 
      num_eig_variable = num_eig_input
   END IF
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write", STATUS = "replace")
   DO ii = 1, num_eig_variable
      WRITE(25, *) eigenvalues_array(ii)
   END DO
   
   CLOSE(25)
   
END SUBROUTINE

!Here eigenvectors are printed one after the other one
SUBROUTINE print_num_complex_eigenvectors_to_file(matrix, filename, num, dh)
   IMPLICIT NONE
   
   CHARACTER(*),  INTENT (IN)  :: filename
   INTEGER, INTENT(IN)         :: num
   TYPE(CMATRIX), INTENT(IN)   :: matrix
   REAL*8, INTENT(IN)          :: dh
   
   REAL*8, DIMENSION(:), ALLOCATABLE :: x_ii_arr
   
   ALLOCATE(x_ii_arr(matrix%dims(1)))
      
   OPEN(UNIT = 10, FILE = filename, ACTION = "write", STATUS = "replace")
   
   !this is the x_min of the interval we consider! every time we update this value by dh
   x_ii_arr(1) = -0.5d0*REAL(matrix%dims(1) - 1, 8)*dh
   DO ii = 2, matrix%dims(1)
      x_ii_arr(ii) = x_ii_arr(ii - 1) + dh
   END DO
   
   !Iterate over columns and get "num" elements
   
   DO jj = 1, num
   !Iterate over rows (it is a single eigenvector) and column fixed
      DO ii = 1, matrix%dims(1) 
         WRITE(10, "(G0,G0,A,G0)") x_ii_arr(ii), REAL(matrix%element(ii, jj)), "," , AIMAG(matrix%element(ii, jj))
      END DO
   END DO
   
   CLOSE(25)
   DEALLOCATE(x_ii_arr)
   
END SUBROUTINE

!Here real parts of eigenvectors are printed one at side of the other one
SUBROUTINE print_num_real_eigenvectors_to_file(matrix, filename, num, norm_flag, dh)
   IMPLICIT NONE
   
   CHARACTER(*),  INTENT (IN) :: filename
   INTEGER, INTENT(IN)        :: num
   TYPE(CMATRIX), INTENT(IN)  :: matrix
   LOGICAL, INTENT(IN)        :: norm_flag
   REAL*8, INTENT(IN)         :: dh
   
   REAL*8                     :: xii_float
      
   OPEN(UNIT = 10, FILE = filename, ACTION = "write", STATUS = "replace")
   
   !this is the x_min of the interval we consider! every time we update this value by dh
   xii_float = -0.5d0*REAL(matrix%dims(1) - 1, 8)*dh
   IF (norm_flag) THEN
      !Iterate over rows (it is a single eigenvector) and column fixed
      DO ii = 1, matrix%dims(1)
      
         !Write the interval coordinates first
         WRITE(10, '(G0,A)', ADVANCE = 'NO') xii_float, ","
         
         DO jj = 1, num - 1
            WRITE(10, '(G0,A)', ADVANCE = 'NO') REAL(matrix%element(ii,jj))/SQRT(dh), ","
         END DO
         
         WRITE(10, '(G0,A)', ADVANCE = 'NO') REAL(matrix%element(ii,num - 1))/SQRT(dh), char(10)
         !Update the interval coordinates
         xii_float = xii_float + dh
         
      END DO
   ELSE IF (.NOT.norm_flag) THEN
      !Iterate over rows (it is a single eigenvector) and column fixed
      DO ii = 1, matrix%dims(1)
      
         !Write the interval coordinates first
         WRITE(10, '(G0,A)', ADVANCE = 'NO') xii_float, ","
         
         DO jj = 1, num - 1
            WRITE(10, '(G0,A)', ADVANCE = 'NO') REAL(matrix%element(ii,jj)), ","
         END DO
         
         WRITE(10, '(G0,A)', ADVANCE = 'NO') REAL(matrix%element(ii,num - 1)), char(10)
         !Update the interval coordinates
         xii_float = xii_float + dh
         
      END DO
   END IF
   
   CLOSE(25)
   
END SUBROUTINE

!Function to convert integer to string
CHARACTER(len=20) FUNCTION str(k) 
    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: k
    WRITE (str, *) k
    str = ADJUSTL(str)
END FUNCTION str

END MODULE
