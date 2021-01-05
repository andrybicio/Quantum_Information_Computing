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
   
   IDENTITY_mat%element = DCMPLX(0d0,0d0)
   DO ii = 1, nn
      IDENTITY_mat%element(ii,ii) = DCMPLX(1d0, 0d0)
   END DO
END FUNCTION


FUNCTION XOR_FUN(n, m, NN) RESULT(XOR)

    INTEGER, INTENT(IN) :: m, n, NN
    INTEGER :: kk, XOR
    
    XOR = 0
    DO kk=1,NN-1
       IF(IEOR(m,n).EQ.(2**(kk-1)+2**kk)) THEN
          XOR = XOR + 1
       END IF
    END DO
END FUNCTION XOR_FUN


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

FUNCTION GET_HAMILTONIAN_MF(nn_part, d_dim, lambda, debug, print_output, exit_on_error) RESULT(Hamiltonian)

   IMPLICIT NONE

   !Input variables for the function
   LOGICAL, INTENT(IN) :: DEBUG, PRINT_OUTPUT, EXIT_ON_ERROR  
   INTEGER, INTENT(IN) :: d_dim, nn_part
   REAL*8,  INTENT(IN) :: lambda
   
   !Indeces
   INTEGER :: ii, jj
   
   !Dummy matrices
   TYPE(CMATRIX) :: identity_mat, pauli_mat_z, pauli_mat_x, temp
   
   !Hamiltonian quantities, interaction strength and array of eigenvalues
   TYPE(CMATRIX) :: Hamiltonian
   REAL*8, DIMENSION(:), ALLOCATABLE :: array_eigenvalues
   
   !Start program      
   identity_mat   = INITIALIZE_IDENTITY_N(d_dim)   
   pauli_mat_z    = PAULI_MATRIX(3)
   pauli_mat_x    = PAULI_MATRIX(1)
   
   !Initialize an empty Hamiltonian and array of eigenvalues
   Hamiltonian = INIT_CMAT_VALUE(d_dim**nn_part, d_dim**nn_part, DCMPLX(0d0,0d0))
   ALLOCATE(array_eigenvalues(d_dim**nn_part))
            
   !External field term
   DO ii = 1, nn_part
   !temp for the first iteration is the identity matrix unless we are considering the 1st particle
      IF (ii == 1) THEN
         temp = pauli_mat_z
      ELSE 
         temp = INITIALIZE_IDENTITY_N(d_dim)
      END IF
      DO jj = 2, nn_part
          IF (jj == ii) THEN
             temp = TENSOR_PRODUCT(temp, pauli_mat_z)
          ELSE
             temp = TENSOR_PRODUCT(temp, identity_mat) 
          END IF
      END DO
      Hamiltonian%element = Hamiltonian%element + lambda*temp%element
      DEALLOCATE(temp%element)
   END DO
      
   !Compute the interaction term of the Hamiltonian using the "element matrix"
   !computation and not the tensor product which is slower
   temp = INIT_CMAT_VALUE(d_dim**nn_part, d_dim**nn_part, DCMPLX(0d0,0d0))
   DO ii=0, 2**nn_part - 1
     DO jj=0, 2**nn_part - 1
        temp%element(jj + 1,ii + 1) = XOR_FUN(jj, ii, nn_part)
     END DO
  END DO
   
  Hamiltonian%element = Hamiltonian%element + temp%element
  DEALLOCATE(temp%element) 
   
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

   DEALLOCATE(pauli_mat_x%element)
   DEALLOCATE(pauli_mat_z%element)
   DEALLOCATE(identity_mat%element)

END FUNCTION

END MODULE


MODULE RSRG_MODULE

   USE MEAN_FIELD_HAMILTONIAN
   USE MY_MATRIX_MODULE
   USE HERMITIAN_MATRICES_UTILS
   USE DENSITY_MAT_UTILITIES
   USE CHECKPOINT_DEBUG_MODULE
   
   IMPLICIT NONE
   
   !indeces
   INTEGER, PRIVATE :: ii
   
CONTAINS

!Computes the change of basis given matrix A and matrix of change of basis P
!such that A' = P\dagger A P
!A = matrA
!P = matrB
FUNCTION CHANGE_OF_BASIS(matrA, matrB) RESULT(A_transformed)

   IMPLICIT NONE
   
   TYPE(CMATRIX), INTENT(IN) :: matrA, matrB
   TYPE(CMATRIX) :: A_transformed
   
   !Initialize the resulting matrix A
   A_transformed = INIT_CMAT_VALUE(matrB%dims(2), matrB%dims(2), DCMPLX(0d0,0d0))
   A_transformed%element = MATMUL(CONJG(TRANSPOSE(matrB%element)), MATMUL(matrA%element, matrB%element))
      
END FUNCTION

!This function returns the identity matrix left-applied to a matrix A a certain number of times
FUNCTION LEFT_APPLY_IDENTITY(matrixA, d_dim, times) RESULT(res_mat)
   
   IMPLICIT NONE
   
   TYPE(CMATRIX), INTENT(IN) :: matrixA
   INTEGER, INTENT(IN)       :: times, d_dim
   
   TYPE(CMATRIX) :: id_mat, res_mat
   
   res_mat = INITIALIZE_IDENTITY_N(d_dim)
   id_mat  = INITIALIZE_IDENTITY_N(d_dim)

   DO ii = 1, times - 1
      res_mat = TENSOR_PRODUCT(res_mat, id_mat) 
   END DO
   
   res_mat = TENSOR_PRODUCT(res_mat, matrixA) 
   
   DEALLOCATE(id_mat%element)
   
END FUNCTION

!This function returns the identity matrix right-applied to a matrix A a certain number of times
FUNCTION RIGHT_APPLY_IDENTITY(matrixA, d_dim, times) RESULT(res_mat)
   
   IMPLICIT NONE
   
   TYPE(CMATRIX), INTENT(IN) :: matrixA
   INTEGER, INTENT(IN)       :: times, d_dim
   
   TYPE(CMATRIX) :: id_mat, res_mat
   
   res_mat = matrixA
   id_mat  = INITIALIZE_IDENTITY_N(d_dim)

   DO ii = 1, times
      res_mat = TENSOR_PRODUCT(res_mat, id_mat) 
   END DO
   
   DEALLOCATE(id_mat%element)
   
END FUNCTION

END MODULE



PROGRAM RSRG_PROGRAM

   USE CHECKPOINT_DEBUG_MODULE
   USE MY_MATRIX_MODULE
   USE HERMITIAN_MATRICES_UTILS
   USE DENSITY_MAT_UTILITIES
   USE MEAN_FIELD_HAMILTONIAN
   USE RSRG_MODULE
   
   IMPLICIT NONE

   !Debug flags
   LOGICAL :: DEBUG = .FALSE.
   LOGICAL :: PRINT_OUTPUT = .FALSE.
   LOGICAL :: EXIT_ON_ERROR = .TRUE.
   
   !Indeces and debug integers
   INTEGER :: ii, jj, kk, info
   
   !Variables useful for the problem
   INTEGER   :: d_dim, n_lambdas, n_iter, n_particles, n_particles_max
   TYPE(CMATRIX) :: Hamiltonian, Hamiltonian_2N, eigen_mat, projector
   TYPE(CMATRIX) :: H_left, H_right, A_left, B_right, pauli_mat_x, AB_mat
   REAL*8 :: lambda, reduction_factor
   CHARACTER(20) :: filename, nn_str, n_lambdas_str, n_iter_str, n_part_str
   CHARACTER(40) :: filename2
   
   !Dummy variables
   REAL*8, DIMENSION(:), ALLOCATABLE :: array_eigenvalues
   
   d_dim           = 2
   n_lambdas       = 100
   WRITE (n_lambdas_str, '(I3)') n_lambdas
   
!Take as input the max number of particles to simulate and number of iterations
IF(COMMAND_ARGUMENT_COUNT() == 2) THEN

  CALL GET_COMMAND_ARGUMENT(1, nn_str)
  READ(nn_str,*) n_particles_max
  CALL GET_COMMAND_ARGUMENT(2, n_iter_str)
  READ(n_iter_str,*) n_iter
  
  IF (DEBUG) THEN
      PRINT*, "Check whether the number of particles is at least 2"
      PRINT*, assert_greater_equal(n_particles_max, 2 , PRINT_OUTPUT, EXIT_ON_ERROR)
      PRINT*, "Check whether the number of particles is at most 5"
      PRINT*, assert_less_equal(n_particles_max, 5, PRINT_OUTPUT, EXIT_ON_ERROR)
      PRINT*, "Check whether the number of iteration is positive"
      PRINT*, assert_positive(n_iter, PRINT_OUTPUT, EXIT_ON_ERROR)
  END IF
  
ELSE
  PRINT*, "Please provide the max number of particles to simulate and overall number of iterations for the RSRG algorithm."
  PRINT*, "Max number of particles must be at most 5 and more than 2"
  PRINT*, "Exiting..."
  STOP
  
END IF
   
   
!Iterate for any number of particles up to max number of particles
   DO ii = 2, n_particles_max
   
   n_particles = ii
   WRITE (n_part_str, '(I3)') n_particles
   filename = TRIM("n_particles"//TRIM(n_part_str)//".dat")

   PRINT*, "##########################################################"
   PRINT*, "##########################################################"
   PRINT*, "############ Number of particles: ", TRIM(nn_str), " #####################"
   PRINT*, "##########################################################"
   PRINT*, "##########################################################"
   
   !Iterate for all the lambdas
   DO kk = 0, n_lambdas
   
   lambda = 3d0/(DBLE(n_lambdas))*DBLE(kk)

   
   !Initialize Pauli Matrix
   pauli_mat_x    = PAULI_MATRIX(1)
   !Obtain the starting Hamiltonian with N particles
   Hamiltonian = GET_HAMILTONIAN_MF(n_particles, d_dim, lambda, debug, print_output, exit_on_error)
   IF (DEBUG) THEN
      PRINT*, char(10), "Hamiltonian term" 
      CALL PRINT_CMATRIX_OUTPUT(Hamiltonian)
   END IF

   !Initialize the interaction terms of the Hamiltonian
   A_left  = LEFT_APPLY_IDENTITY(pauli_mat_x, d_dim, n_particles - 1)
   B_right = RIGHT_APPLY_IDENTITY(pauli_mat_x, d_dim, n_particles - 1)
   IF (DEBUG) THEN
      PRINT*, "Check if A_left is Hermitian ->", cmatrix_assert_hermit(A_left, PRINT_OUTPUT, EXIT_ON_ERROR)
      PRINT*, "Check if B_right is Hermitian ->", cmatrix_assert_hermit(B_right, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Left interaction term"
         CALL PRINT_CMATRIX_OUTPUT(A_left)
         PRINT*, char(10), "Right interaction term" 
         CALL PRINT_CMATRIX_OUTPUT(B_right)
      END IF
   END IF
   
   DO jj = 1, n_iter
   
   IF (MOD(jj,50)==0) THEN
      PRINT*, "Iteration RSRG number ->", jj,"/", TRIM(n_iter_str)
   END IF
   
   !Compute the first term of the hamiltonian (1st half of the bipartite system)
   !After 1st iteration, Hamiltonian will be the projected one
   H_left  = RIGHT_APPLY_IDENTITY(Hamiltonian, d_dim, n_particles)
   IF (DEBUG) THEN
      PRINT*, "HL shape ->", SHAPE(H_left%element)
      PRINT*, "Check if HL is Hermitian ->", cmatrix_assert_hermit(H_left, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Left subsystem term of 2N hamiltonian" 
         CALL PRINT_CMATRIX_OUTPUT(H_left)
      END IF
   END IF
   
   !Compute the second term of the hamiltonian (2nd half of the bipartite system)
   !After 1st iteration, Hamiltonian will be the projected one
   H_right = LEFT_APPLY_IDENTITY(Hamiltonian, d_dim, n_particles)
   IF (DEBUG) THEN
      PRINT*, "HR shape ->", SHAPE(H_right%element)
      PRINT*, "Check if HR is Hermitian ->", cmatrix_assert_hermit(H_right, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Right subsystem term of 2N hamiltonian"
         CALL PRINT_CMATRIX_OUTPUT(H_right)
      END IF 
   END IF

   !Compute the last term of the Hamiltonian, which is the interaction term
   !Afterthe 1st iteration A_left and B_right will be projected
   AB_mat = TENSOR_PRODUCT(A_left, B_right)
   IF (DEBUG) THEN
      PRINT*, "AB shape ->", SHAPE(AB_mat%element)
      PRINT*, "Check if AB is Hermitian ->", cmatrix_assert_hermit(AB_mat, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Interaction term of 2N hamiltonian"
         CALL PRINT_CMATRIX_OUTPUT(AB_mat)
      END IF
   END IF

   !Create the new Hamiltonian and update it
   Hamiltonian_2N = INIT_CMAT_VALUE(d_dim**(2*n_particles), d_dim**(2*n_particles), DCMPLX(0d0,0d0))
   Hamiltonian_2N%element = H_left%element + H_right%element + AB_mat%element
   IF (DEBUG) THEN
      PRINT*, "New Ham shape", SHAPE(Hamiltonian_2N%element)
      PRINT*, "Check if 2N Hamiltonian is Hermitian ->", cmatrix_assert_hermit(Hamiltonian_2N, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "2N hamiltonian"
         CALL PRINT_CMATRIX_OUTPUT(Hamiltonian_2N)
      END IF
   END IF

   !Create a dummy matrix to be diagonalized and from which to store the first eigenvectors
   eigen_mat         = INIT_CMAT_VALUE(Hamiltonian_2N%dims(1), Hamiltonian_2N%dims(2), DCMPLX(0d0,0d0))
   eigen_mat%element = Hamiltonian_2N%element
   CALL HERMITIAN_EIGENVECTORS(eigen_mat%element, eigen_mat%dims(1), array_eigenvalues, info)
   
   !Build the projector, whose columns are the eigenvectors of the 2N Hamiltonian
   projector = INIT_CMAT_VALUE(Hamiltonian_2N%dims(1), Hamiltonian%dims(2), DCMPLX(0d0,0d0))
   projector%element = eigen_mat%element(:,1:d_dim**n_particles)
   IF (DEBUG) THEN
      PRINT*, "Projector shape", SHAPE(projector%element)
   END IF
   
   !Project the Hamiltonian into the new basis
   IF (DEBUG) THEN
      PRINT*, "Before projection, shape Ham_2N", SHAPE(Hamiltonian_2N%element)
   END IF
   Hamiltonian = CHANGE_OF_BASIS(Hamiltonian_2N, projector)
   IF (DEBUG) THEN
      PRINT*, "After projection, shape Ham", SHAPE(Hamiltonian%element)
      PRINT*, "Check if projected Hamiltonian is Hermitian ->", cmatrix_assert_hermit(Hamiltonian, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Projected hamiltonian"
         CALL PRINT_CMATRIX_OUTPUT(Hamiltonian)
      END IF
   END IF
   
   !Left interaction term into the new basis
   A_left  = RIGHT_APPLY_IDENTITY(A_left, d_dim, n_particles)
   A_left  = CHANGE_OF_BASIS(A_left, projector)
   IF (DEBUG) THEN
      PRINT*, "Projected A_left shape", SHAPE(A_left%element)
      PRINT*, "Check if projected A_left is Hermitian ->", cmatrix_assert_hermit(A_left, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Projected A_left"
         CALL PRINT_CMATRIX_OUTPUT(A_left)
      END IF
   END IF
   
   !Right interaction term into the new basis
   B_right = LEFT_APPLY_IDENTITY(B_right, d_dim, n_particles)
   B_right = CHANGE_OF_BASIS(B_right, projector)
   IF (DEBUG) THEN
      PRINT*, "Projected B_right shape", SHAPE(B_right%element)
      PRINT*, "Check if projected B_right is Hermitian ->", cmatrix_assert_hermit(B_right, PRINT_OUTPUT, EXIT_ON_ERROR)
      IF (PRINT_OUTPUT) THEN
         PRINT*, char(10), "Projected B_right"
         CALL PRINT_CMATRIX_OUTPUT(B_right)
      END IF
   END IF
   
   !Print to a file the ground state value for different number of iterations
   IF (lambda.EQ.0d0) THEN
      filename2 = TRIM("gs_vs_iter_npart"//TRIM(n_part_str)//".dat")
      reduction_factor = DBLE(n_particles)*(DBLE(2)**DBLE(jj))
      CALL PRINT_LAMBDA_KK_EIGENVALUES_TO_FILE(DBLE(jj), array_eigenvalues/reduction_factor, filename2, 1)
   END IF   
   
   DEALLOCATE(H_left%element)
   DEALLOCATE(H_right%element)
   DEALLOCATE(AB_mat%element)
   DEALLOCATE(eigen_mat%element) 
   
   END DO
   
   reduction_factor = DBLE(n_particles)*(DBLE(2)**DBLE(n_iter))
   !Print only some of the iterations results
   IF (MOD(kk,25)==0) THEN
      PRINT*, "Iteration on lambda -> ",kk,"/", TRIM(n_lambdas_str), "  lambda = ", lambda
      PRINT*, "Energy density ground state: -> ", array_eigenvalues(1)/reduction_factor, char(10)
   END IF   
   !Print only the first eigenvalue but before rescale the array
   CALL PRINT_LAMBDA_KK_EIGENVALUES_TO_FILE(lambda, array_eigenvalues(:)/reduction_factor, filename, 4)
   DEALLOCATE(array_eigenvalues)
   
   END DO
   
   END DO
   
END PROGRAM
