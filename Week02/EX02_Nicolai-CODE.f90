MODULE MY_MODULE_MATRICES
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
   
!List of functions to be implemented!
CONTAINS

!Initialize whole elements of the matrix to a predefined value
   FUNCTION INIT_MATRIX_VALUE(rows, columns, myvalue) RESULT(matrix)
       IMPLICIT NONE
       
       !Input and temp variables declaration
       INTEGER, INTENT(IN) :: rows, columns
       DOUBLE COMPLEX, INTENT(IN) :: myvalue
       INTEGER :: ii, jj
       
       !Output matrix and memory allocation
       TYPE(CMATRIX) :: matrix
       matrix%dims(1) = rows
       matrix%dims(2) = columns
       ALLOCATE(matrix%element(matrix%dims(1),matrix%dims(2)))
       
       !Check if dimensions are negative, if so raise an error
       IF (rows .LE. 0 .OR. columns .LE. 0)  THEN
           PRINT*, "Cannot initialize a matrix with null or negative dimensions!"
           DEALLOCATE(matrix%element)
           CALL EXIT(1)
       END IF
       
       !Set all elements to that value
       DO jj = 1, matrix%dims(2)
          DO ii = 1, matrix%dims(1)
             matrix%element(ii, jj) = myvalue
          ENDDO
       ENDDO
       
       !Note as if elements change, this value does not update
       matrix%trace = .Trace.matrix
       !Let assign it this value for now
       matrix%determinant = (0d0, 0d0)
       
       RETURN
   
   END FUNCTION INIT_MATRIX_VALUE

!Initialize whole elements of the matrix to a random value
   FUNCTION INIT_MATRIX_RANDOM(rows, columns) RESULT(matrix)
       IMPLICIT NONE
       
       !Input and temp variables declaration
       INTEGER, INTENT(IN) :: rows, columns
       INTEGER :: ii, jj
       REAL :: a, b
       
       !Output matrix and memory allocation
       TYPE(CMATRIX) :: matrix
       matrix%dims(1) = rows
       matrix%dims(2) = columns
       ALLOCATE(matrix%element(matrix%dims(1),matrix%dims(2)))
       
       !Check if dimensions are either null or negative, if so, raise an error
       IF (rows .LE. 0 .OR. columns .LE. 0)  THEN
           PRINT*, "Cannot initialize a matrix with null or negative dimensions!"
           DEALLOCATE(matrix%element)
           CALL EXIT(1)
       END IF
       
       !Each element is drawn from a [-1,1] distribution 
       DO jj = 1, matrix%dims(2)
          DO ii = 1, matrix%dims(1)
             a = 2*(RAND(0) - 0.5)
             b = 2*(RAND(0) - 0.5)
             matrix%element(ii, jj) = CMPLX(a,b)
          ENDDO
       ENDDO
       
       !Note as if elements change, this value does not update
       matrix%trace = .Trace.matrix
       !Let assign it this value for now
       matrix%determinant = (0d0, 0d0)
       RETURN
   
   END FUNCTION INIT_MATRIX_RANDOM
   

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
       adj_matrix%determinant = (0d0, 0d0)
      END IF 
               
   END FUNCTION MAT_ADJ
   
  !Function for computing the trace of the matrix
   FUNCTION  Mat_Trace(in_matrix) RESULT(trace)
      IMPLICIT NONE
      
      TYPE(CMATRIX), INTENT(IN) :: in_matrix
      DOUBLE COMPLEX :: trace
      INTEGER :: ii
      
      trace = (0d0, 0d0)
      
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

END MODULE


PROGRAM MATRIX_MODULE_TEST
    USE MY_MODULE_MATRICES

    IMPLICIT NONE
    
    DOUBLE COMPLEX :: my_trace
    
    TYPE(CMATRIX) :: my_matrix
    TYPE(CMATRIX) :: my_adjoint_matrix 

    LOGICAL :: DEBUG = .FALSE.
    CHARACTER(10) :: filename
    
    !Either set to a predefined value, or a random one
!    my_matrix = INIT_MATRIX_VALUE(4,4, (3d0,-2d0))
    my_matrix = INIT_MATRIX_RANDOM(4,4)

    !Note as the operator and function below return the same output
    my_adjoint_matrix = .Adj.my_matrix
!    my_adjoint_matrix = Mat_Adj(my_matrix)
    
    IF (DEBUG .EQV. .TRUE.) THEN
        PRINT*, my_matrix%element
        PRINT*, my_adjoint_matrix%element
    END IF
    
    my_trace = .Trace.my_matrix
    
    IF (DEBUG .EQV. .TRUE.) THEN
        PRINT*, "Trace of the matrix is", my_trace 
    END IF
    
    !Print to a file the matrix and to another its adjoint and trace/determ.
    filename = "output.txt"
    CALL PRINT_TO_FILE(my_matrix, filename )
    filename = "AdjMat.txt"
    CALL PRINT_TO_FILE(my_adjoint_matrix, filename )
    DEALLOCATE(my_matrix%element)
    DEALLOCATE(my_adjoint_matrix%element)

END PROGRAM
    


