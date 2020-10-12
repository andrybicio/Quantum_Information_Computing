!Declare a module
MODULE matrix_functions

IMPLICIT NONE

!Declare indeces
INTEGER :: ii,jj,kk

CONTAINS

!return a matrix with random values
FUNCTION fill_matrix_rval(NN) result(mat_filled)
   INTEGER :: NN
!must be specified otherwise it does not know what type is mat_filled
   REAL*4, DIMENSION(NN,NN) :: mat_filled
   DO ii = 1,NN
      DO jj = 1,NN
         mat_filled(ii,jj) = RAND(0)
      ENDDO
   ENDDO
END FUNCTION

!A is lhs matrix, B rhs second, C is the resulting one
!first index "i" runs slower in c_ij 
FUNCTION matrix_product_def(matrixA, matrixB, NN) result(matrixC)
   INTEGER :: NN
   REAL*4, DIMENSION(:,:) :: matrixA,matrixB
   REAL*4, DIMENSION(NN,NN) :: matrixC
   
   DO ii = 1,NN
      DO jj = 1,NN
        DO kk = 1, NN
           matrixC(ii,jj) = matrixC(ii,jj) + matrixA(ii,kk)*matrixB(kk,jj)
        ENDDO
      ENDDO
   ENDDO
END FUNCTION

!Here it is the "second way", i.e. the one with transposed matrix
!first index "i" runs faster in c_ij 
FUNCTION matrix_product_transposed(matrixA, matrixB, NN) result(matrixC)
   INTEGER :: NN
   REAL*4, DIMENSION(:,:) :: matrixA,matrixB
   REAL*4, DIMENSION(NN,NN) :: matrixC
   
   DO kk = 1,NN 
      DO jj = 1,NN
        DO ii = 1, NN
         matrixC(ii,jj) = matrixC(ii,jj) + matrixA(ii,kk)*matrixB(kk,jj)
        ENDDO
      ENDDO
   ENDDO
END FUNCTION

END MODULE

!This is the "program" i.e. the "main() {...}" in c++
PROGRAM matrix_product_test_performance

USE matrix_functions
IMPLICIT NONE


CHARACTER(5) :: nnstring
CHARACTER(5) :: name_string
CHARACTER(10) :: filename
CHARACTER(4) :: file_extension = ".dat"

REAL :: time_delta1
REAL :: time_delta2
REAL :: time_delta3
REAL :: start_time, finish_time

LOGICAL :: DEBUG = .FALSE.
LOGICAL :: OUTPUT = .TRUE.


INTEGER :: size_matrix
REAL*4, DIMENSION (:,:), ALLOCATABLE :: matrixA, matrixB 
REAL*4, DIMENSION (:,:), ALLOCATABLE :: matrixC

!If present,
!first argument is the size of the matrix and second the output filename
IF(COMMAND_ARGUMENT_COUNT() < 1)then
!set a default size
  size_matrix = 100
  OUTPUT = .FALSE.
ELSE
  CALL GET_COMMAND_ARGUMENT(1, nnstring)
  READ(nnstring,*)size_matrix

  CALL GET_COMMAND_ARGUMENT(2, name_string)
  filename = TRIM(name_string) // file_extension
ENDIF

IF (DEBUG .eqv. .TRUE.)PRINT*, filename


ALLOCATE ( matrixA (size_matrix,size_matrix))
ALLOCATE ( matrixB (size_matrix,size_matrix))
ALLOCATE ( matrixC (size_matrix,size_matrix))

!Fill matrices with random values
matrixA = fill_matrix_rval(size_matrix)
matrixB = fill_matrix_rval(size_matrix)
IF (DEBUG .eqv. .TRUE.)print*, matrixA

CALL CPU_TIME(start_time)
matrixC = matrix_product_def(matrixA, matrixB, size_matrix)
CALL CPU_TIME(finish_time)

time_delta1 = finish_time - start_time
PRINT*, "Time passed using definition", time_delta1, "sec"

CALL CPU_TIME(start_time)
matrixC = matrix_product_transposed(matrixA, matrixB, size_matrix)
CALL CPU_TIME(finish_time)

time_delta2 = finish_time - start_time
PRINT*, "Time passed using transposed matrices", time_delta2, "sec"


CALL CPU_TIME(start_time)
matrixC = matmul(matrixA,matrixB)
CALL CPU_TIME(finish_time)

time_delta3 = finish_time - start_time
PRINT*, "Time passed using intrinsic function", time_delta3, "sec"

IF (DEBUG .eqv. .TRUE.)print*, matrixC



!create an output data file whose entries are:
!size of the matrix, 
!time spent with the definition of matricial product
!time spent using transposed matrices product
!time spent using the intrinsic function
IF (OUTPUT .eqv. .TRUE.) THEN
    !output data into a file
    OPEN(1, file = filename , status='unknown', position='append')      
    WRITE(1,*) size_matrix, time_delta1, time_delta2, time_delta3 
    CLOSE(1) 
END IF

!Deallocate space
DEALLOCATE(matrixA)
DEALLOCATE(matrixB)
DEALLOCATE(matrixC)



END PROGRAM
