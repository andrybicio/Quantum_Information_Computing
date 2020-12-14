MODULE MEAN_FIELD_HAMILTONIAN

   USE CHECKPOINT_DEBUG_MODULE
   USE MY_MATRIX_MODULE
   USE HERMITIAN_MATRICES_UTILS
   USE DENSITY_MAT_UTILITIES
   
   IMPLICIT NONE
   
   !Indexes
   INTEGER, PRIVATE :: ii, jj
   
CONTAINS

!This function returns a Pauli Matrix given which one we select
!1 = sigma_x
!2 = sigma_y
!3 = sigma_z
FUNCTION PAULI_MATRIX(INTEGER_TYPE) RESULT(p_matrix)
   INTEGER, INTENT(IN) :: INTEGER_TYPE
   
   TYPE(CMATRIX) :: p_matrix
   
   IF(.not.ALLOCATED(p_matrix%element)) THEN
       ALLOCATE(p_matrix%element(2,2))
       p_matrix%dims(1) = 2
       p_matrix%dims(2) = 2
   END IF
   
   SELECT CASE(INTEGER_TYPE)
      CASE(1)
      p_matrix%element(1,:) = (/DCMPLX(0d0,0d0),DCMPLX(1d0,0d0)/) 
      p_matrix%element(2,:) = (/DCMPLX(1d0,0d0),DCMPLX(0d0,0d0)/) 
      
      CASE(2)
      p_matrix%element(1,:) = (/DCMPLX(0d0,0d0),DCMPLX(0d0,-1d0)/) 
      p_matrix%element(2,:) = (/DCMPLX(0d0,1d0),DCMPLX(0d0,0d0)/) 
      
      CASE(3) 
      p_matrix%element(1,:) = (/DCMPLX(1d0,0d0),DCMPLX(0d0,0d0)/)
      p_matrix%element(2,:) = (/DCMPLX(0d0,0d0),DCMPLX(-1d0, 0d0)/) 

      CASE DEFAULT
         PRINT*, "Invalid selection for Pauli matrix, exiting..."
         STOP
   END SELECT

END FUNCTION

!This function returns the tensor product between tensor_a and tensor_b
!If both a and b have rank 2, then indeed we are doing Kronecker product
!since we know we will be dealing with them, we expect as tensor_a and tensor_b
!to have two indeces and at the end of the day it is a K. product
FUNCTION TENSOR_PRODUCT(tensor_a, tensor_b) RESULT(result_matrix)
   
   TYPE(CMATRIX), INTENT(IN) :: tensor_a, tensor_b
   TYPE(CMATRIX) :: result_matrix
   
   IF(.not.ALLOCATED(result_matrix%element)) THEN
       ALLOCATE(result_matrix%element(tensor_a%dims(1)*tensor_b%dims(1), tensor_a%dims(2)*tensor_b%dims(2)))
       result_matrix%dims(1) = tensor_a%dims(1)*tensor_b%dims(1)
       result_matrix%dims(2) = tensor_a%dims(2)*tensor_b%dims(2)
   END IF
   
   !Use "array slicing", for sure it is better optimized
   DO ii = 1, tensor_a%dims(1)
      DO jj = 1, tensor_a%dims(2)
          result_matrix%element((ii-1)*tensor_b%dims(1)+1:ii*tensor_b%dims(1), &    !rows
          &                     (jj-1)*tensor_b%dims(2)+1:jj*tensor_b%dims(2)) = &  !columns
          &                     tensor_a%element(ii,jj)*tensor_b%element  
      END DO
   END DO

END FUNCTION

!This function returns the identity matrix of size nn
FUNCTION INITIALIZE_IDENTITY_N(nn) RESULT(IDENTITY_mat)
   INTEGER, INTENT(IN) :: nn
   
   TYPE(CMATRIX) :: IDENTITY_mat
   
   IF(.not.ALLOCATED(IDENTITY_mat%element)) THEN
       ALLOCATE(IDENTITY_mat%element(nn,nn))
       IDENTITY_mat%dims(1) = nn
       IDENTITY_mat%dims(2) = nn
   END IF
   
   DO ii = 1, nn
      IDENTITY_mat%element(ii,ii) = DCMPLX(1d0, 0d0)
   END DO
END FUNCTION

SUBROUTINE PRINT_LAMBDA_KK_EIGENVALUES_TO_FILE(lambda, arr_eigenv, filename, kk)
   
   CHARACTER(*), INTENT (IN)       :: filename
   REAL*8, DIMENSION(:), INTENT(IN):: arr_eigenv
   INTEGER, INTENT(IN) :: kk
   REAL*8,  INTENT(IN) :: lambda
   INTEGER :: num_eig_variable
   
   
   !If it is not specified (k==0), print all eigenvalues
   IF (kk.EQ.0) THEN
      num_eig_variable = SIZE(arr_eigenv)
   ELSE 
      num_eig_variable = kk
   END IF
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write", POSITION = "append")
   WRITE(25, '(G0, A)', ADVANCE = "NO") lambda, ","
   DO ii = 1 , num_eig_variable - 1
      WRITE(25, '(G0, A)', ADVANCE = "NO")  arr_eigenv(ii), ","
   END DO
   WRITE(25, '(G0)') arr_eigenv(num_eig_variable)
   CLOSE(25)

END SUBROUTINE

SUBROUTINE PRINT_MATRIX_TO_FILE(matrix,filename) 
   
   CHARACTER(*), INTENT (IN)       :: filename
   REAL*8, DIMENSION(:,:), INTENT(IN):: matrix
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write")
   DO ii = 1 , SIZE(matrix, 1)
      DO jj = 1, SIZE(matrix, 2) - 1
         WRITE(25, '(G0, A)', ADVANCE = "NO")  matrix(ii,jj), ","
      END DO
      WRITE(25, '(G0)') matrix(ii, SIZE(matrix, 2))
   END DO
   
   CLOSE(25)
END SUBROUTINE 

END MODULE

PROGRAM DENSITY_MATRICES

   USE CHECKPOINT_DEBUG_MODULE
   USE MY_MATRIX_MODULE
   USE HERMITIAN_MATRICES_UTILS
   USE DENSITY_MAT_UTILITIES
   USE MEAN_FIELD_HAMILTONIAN
   
   IMPLICIT NONE

   !Debug flags
   LOGICAL :: DEBUG = .FALSE.
   LOGICAL :: PRINT_OUTPUT = .FALSE.
   LOGICAL :: EXIT_ON_ERROR = .TRUE.
   
   !Indeces and debug integers
   INTEGER :: ii, jj, kk, nn, info
   
   !Variables useful for the problem
   INTEGER :: d_dim, n_particles, n_lambdas
   !Dummy matrices
   TYPE(CMATRIX) :: identity_mat, pauli_mat_z, pauli_mat_x, temp
   !Hamiltonian quantities, interaction strength and array of eigenvalues
   TYPE(CMATRIX) :: Hamiltonian
   REAL*8, DIMENSION(:), ALLOCATABLE :: array_eigenvalues
   REAL*8, DIMENSION(:,:), ALLOCATABLE :: matrix_times
   REAL*8 :: lambda, start_time, end_time
   CHARACTER(20) :: filename, nn_str, npart_str

   n_particles = 10
   d_dim       = 2
   lambda      = 3d0
   n_lambdas   = 60
      
   identity_mat   = INITIALIZE_IDENTITY_N(d_dim)   
   pauli_mat_z    = PAULI_MATRIX(3)
   pauli_mat_x    = PAULI_MATRIX(1)
 
!Take as input the number of particles to simulate
IF(COMMAND_ARGUMENT_COUNT() == 1) THEN

  CALL GET_COMMAND_ARGUMENT(1, npart_str)
  READ(npart_str,*) n_particles
  ALLOCATE(matrix_times(n_lambdas + 1, n_particles - 1))
  
  IF (DEBUG) THEN
      PRINT*, "Check whether the number of particles is at least 2"
      PRINT*, assert_greater_equal(n_particles, 2 , PRINT_OUTPUT, EXIT_ON_ERROR)
  END IF
  
ELSE
  PRINT*, "Please provide how many particles to simulate. Must be at least 2."
  PRINT*, "Exiting..."
  STOP
  
END IF
   
   DO nn = 2, n_particles
      !Print the number of particles
      PRINT*, char(10), char(10), "Number of particles ->", nn, char(10)
      matrix_times(1,(nn-1)) = nn
      
      WRITE (nn_str, '(I2)') nn
      !Filename for the output file
      filename = TRIM("n_particles"//TRIM(nn_str)//".dat")
      
      !Iteratefor all the lambdas
      DO kk = 0, n_lambdas
      
      CALL CPU_TIME(start_time)
            
      lambda = 3d0/(REAL(n_lambdas))*REAL(kk)
      PRINT*, "Iteration  -> ",kk,"/",INT(n_lambdas), "  lambda = ", lambda
   
      !Initialize an empty Hamiltonian and array of eigenvalues
      Hamiltonian = INIT_CMAT_VALUE(d_dim**nn, d_dim**nn, DCMPLX(0d0,0d0))
      ALLOCATE(array_eigenvalues(d_dim**nn))
      
      !External field term
      DO ii = 1, nn
      !temp for the first iteration is the identity matrix unless we are considering the 1st particle
         IF (ii == 1) THEN
            temp = pauli_mat_z
         ELSE 
            temp = INITIALIZE_IDENTITY_N(d_dim)
         END IF
         DO jj = 2, nn
            IF (jj == ii) THEN
               temp = TENSOR_PRODUCT(temp, pauli_mat_z) 
            ELSE
               temp = TENSOR_PRODUCT(temp, identity_mat) 
            END IF
         END DO
      Hamiltonian%element = Hamiltonian%element + lambda*temp%element
      DEALLOCATE(temp%element)
      END DO
   
      !Interaction term of the Hamiltonian for two neighbour particles
      DO ii = 1, nn - 1
         temp = INITIALIZE_IDENTITY_N(d_dim)
         DO jj = 2, nn
            IF ((jj == ii).OR.(jj == ii + 1)) THEN
               temp = TENSOR_PRODUCT(temp, pauli_mat_x) 
            ELSE
               temp = TENSOR_PRODUCT(temp, identity_mat) 
            END IF
         END DO
      Hamiltonian%element = Hamiltonian%element + temp%element
      DEALLOCATE(temp%element)
      END DO   
   
      !Debug checkpoint!
      IF (DEBUG) THEN
         IF (PRINT_OUTPUT) THEN
            CALL PRINT_CMATRIX_OUTPUT(Hamiltonian)
         END IF
         PRINT*, "Check for Hamiltonian Hermitianity"
         PRINT*, "Check whether Hamiltonian is self-adjoint ->", &
         & assert_is_adjoint(Hamiltonian%element, Hamiltonian%element, PRINT_OUTPUT , EXIT_ON_ERROR )
         WRITE(*, '(G0)', ADVANCE="NO") "Check whether Hamiltonian diagonal entries are real -> "
         DO ii = 1, Hamiltonian%dims(1)
            WRITE(*, '(G0)', ADVANCE="NO") assert_eq(DIMAG(Hamiltonian%element(ii,ii)), 0d0, PRINT_OUTPUT, EXIT_ON_ERROR)
         END DO
      END IF
  
     !Diagonalize and obtain eigenvalues
     CALL HERMITIAN_EIGENVALUES(Hamiltonian%element, Hamiltonian%dims(1), array_eigenvalues, info)

     IF (info.NE.0) then
         PRINT*,'DIAGONALIZATION FAILED'
         STOP
     END IF
  
     !"Normalize" to obtain the energy density
     array_eigenvalues(:) = array_eigenvalues(:)/(nn)
  
     !Print lambda and the set of eigenvalues to a file
     CALL PRINT_LAMBDA_KK_EIGENVALUES_TO_FILE(lambda, array_eigenvalues, filename, 4)
     DEALLOCATE(Hamiltonian%element)
     DEALLOCATE(array_eigenvalues)
     
     CALL CPU_TIME(end_time)
     matrix_times(kk + 1,(nn-1)) = end_time - start_time
     
  END DO
  

END DO

CALL PRINT_MATRIX_TO_FILE(matrix_times,"times.dat")

DEALLOCATE(matrix_times)
END PROGRAM
