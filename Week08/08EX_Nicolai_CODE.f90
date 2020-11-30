! ARGUMENT TO PASS WHEN RUNNING THE PROGRAM
! 0 ==> Coefficients are chosen at random
! 1 ==> Coefficients are [(0,0),(-1,0),(1,0),(0,0)]
! 2 ==> Coefficients are [(1/sqrt(2),0),(1/sqrt(2),0), (1,0), (0,0)]
! 3 ==> Coefficients are [(1/sqrt(2),0),(0,0), (1,0), (-1/sqrt(2),0)]
! 
!This code computes the density matrix of a Quantum system composed by 2 subsystems in C^2
!Different arrays of coefficients are actually already given and must be changed before the compilation
!as well as whether the wavefunction PSI for the full system is either separable
!There are actually 3 (given sets)+1(random set) modes to run the program, each with a specific choice of coefficients
!This was done in order not to create so much confusion with input files and can be easily changed
!
!
!
MODULE DENSITY_MAT_UTILITIES

   USE MY_MATRIX_MODULE
   USE CHECKPOINT_DEBUG_MODULE
   
   IMPLICIT NONE
   
   !index
   INTEGER, PRIVATE :: ii, jj, kk
   
   !New type: psi_state that stores a number n_vectors of psi in d-dim Hilbert space
   TYPE PSI_STATE
      !d_dim parameter, dimension of Hilbert space
      INTEGER :: d_dim
      !number of objects of the complex system
      INTEGER :: n_vectors
      !separable flag: true if separable
      LOGICAL :: separable
      !double complex coefficients of wave function
      !if separable, then dim is n_vectors*d_dim
      !if not separable, then dim is n_vectors**d_dim
      DOUBLE COMPLEX, DIMENSION(:), ALLOCATABLE :: coeffs
   END TYPE 

CONTAINS
!Function to initialize a certain psi state given its coefficients
FUNCTION INIT_PSI_STATE_COEFFS(coeffs, sep_flag, d_dim, n_subsystems ) RESULT(psi_initialized)
   DOUBLE COMPLEX, INTENT(IN), DIMENSION(:) :: coeffs
   LOGICAL, INTENT(IN) :: sep_flag
   INTEGER, INTENT(IN) :: d_dim, n_subsystems
    
   REAL*8          :: norm 
   TYPE(PSI_STATE) :: psi_initialized
   
   psi_initialized%n_vectors = n_subsystems
   psi_initialized%d_dim     = d_dim
   psi_initialized%separable = sep_flag
   
   !SEPARABLE CASE
   IF (sep_flag) THEN
      !Check whether it is a multiple of n_part
      PRINT*, "Check whether coefficients are good in number to initialize the psi", &
      & assert_eq(SIZE(coeffs), d_dim*n_subsystems, .FALSE., .TRUE.)
      IF(.not.ALLOCATED(psi_initialized%coeffs)) THEN
          ALLOCATE(psi_initialized%coeffs(psi_initialized%d_dim*psi_initialized%n_vectors))
      END IF
      psi_initialized%coeffs(:) = coeffs(:) 
   !NOT SEPARABLE CASE
   ELSE
      PRINT*, "Check whether coefficients are good in number to initialize the psi", &
      &  assert_eq(SIZE(coeffs), d_dim*n_subsystems, .FALSE., .TRUE.)
      IF(.not.ALLOCATED(psi_initialized%coeffs)) THEN
          ALLOCATE(psi_initialized%coeffs(psi_initialized%d_dim**psi_initialized%n_vectors))
      END IF
      psi_initialized%coeffs(:) = coeffs(:) 
   END IF
   
   norm = SQRT(SUM(ZABS(psi_initialized%coeffs(:))**2))
   psi_initialized%coeffs(:) = psi_initialized%coeffs(:)/norm
   
END FUNCTION

!Function to initialize a psi state with n_systems particles in C^(Hilbert_dim) space
!coefficients are normalized to 1
!Different initializations whether states are either separable
FUNCTION INIT_PSI_STATE_RANDOM(Hilbert_dim, n_systems, separable_flag, DEBUG) RESULT(psi)
   IMPLICIT NONE
   
   INTEGER :: Hilbert_dim, n_systems
   LOGICAL :: separable_flag, DEBUG
   TYPE(PSI_STATE) :: psi
   
   !dummy variables
   REAL*8  :: random_real, random_complex, norm
   INTEGER :: min_particle, max_particle

   
   psi%n_vectors = n_systems
   psi%d_dim     = Hilbert_dim
   psi%separable = separable_flag
      
   !If the wavefunctions are separable
   IF (separable_flag) THEN
   ALLOCATE(psi%coeffs(n_systems*Hilbert_dim))
      !ii iterates over the number of systems
      DO ii = 1, n_systems
         !running total for the normalization
         norm = 0d0
         !jj iterates over the dimension of the Hilbert space
         DO jj = 1, Hilbert_dim
            !Draw a couple of numbers uniformly from [-1,1]
            random_real    = 2d0*(RAND(0)-0.5d0)
            random_complex = 2d0*(RAND(0)-0.5d0)
            norm = norm + random_real**2 + random_complex**2
            psi%coeffs(jj + (ii-1)*Hilbert_dim) = DCMPLX(random_real,random_complex)
         END DO
         !indeces for a more compact notation
         min_particle = 1 + (ii-1)*Hilbert_dim
         max_particle = ii*Hilbert_dim
         !Normalize the only last coefficients, i.e. a single wavefunction 
         psi%coeffs(min_particle:max_particle) = psi%coeffs(min_particle:max_particle)/SQRT(norm)
      END DO
   ELSE
   !If the wavefunctions are not separable then allocate the related space for coeffs
      ALLOCATE(psi%coeffs(Hilbert_dim**n_systems))
      norm = 0d0
      !jj is the index of the particle/subsystem
      ii    = 0
      DO jj = 1, Hilbert_dim**n_systems
      !Draw a couple of numbers uniformly from [-1,1]
         random_real    = 2d0*(RAND(0)-0.5d0)
         random_complex = 2d0*(RAND(0)-0.5d0)
         psi%coeffs(jj) = DCMPLX(random_real,random_complex)
       END DO
       norm = SQRT(SUM(ZABS(psi%coeffs(:))**2))
       psi%coeffs(:) = psi%coeffs(:)/norm
   END IF
   
   IF (DEBUG) THEN
      OPEN(UNIT = 25, FILE = "PSI_DEBUG.txt", ACTION = "write", STATUS = "replace")
      WRITE(25,*) "Number of systems: ", psi%n_vectors
      WRITE(25,*) "Dimension of Hilbert space: ", psi%d_dim
      WRITE(25,*) "Pure states are separable ->", psi%separable
      
      !Debug if separable
      IF (psi%separable) THEN
      
      !Iterate over particles
      DO ii = 0, psi%n_vectors-1
         !indeces for a more compact notation
         min_particle = 1 + (ii)*Hilbert_dim
         max_particle = (ii+1)*Hilbert_dim
         WRITE(25,*) "Subsystem n°: ", ii
         norm = SQRT(SUM(ZABS(psi%coeffs(min_particle:max_particle))**2))
         WRITE(25,*) "Norm of vector", norm
         PRINT*, ii + 1 , "-th vector is normalized", assert_eq(norm, 1d0, .FALSE., .FALSE.)
         DO jj = 1, psi%d_dim
            WRITE(25, '(G0)') psi%coeffs(jj + (ii-1)*Hilbert_dim )
         END DO
         WRITE(25,*) char(10), char(10)
      END DO
      
      !General case, not separable systems
      ELSE
         norm = SQRT(SUM(ZABS(psi%coeffs(:))**2))
         WRITE(25,*) "Norm of vector", norm
         PRINT*, "Vector is normalized", assert_eq(norm, 1d0, .TRUE., .FALSE.)
         DO jj = 1, Hilbert_dim**n_systems
            WRITE(25, '(G0)') psi%coeffs(jj)
         END DO
      END IF
   CLOSE(25)
   
   !Close debug if statement
   END IF 
   
END FUNCTION

!Function to compute the density matrix
FUNCTION DENSITY_MATRIX(psi, DEBUG) RESULT(DEN_MAT)

   TYPE(PSI_STATE) :: psi, new_psi_not_sep
   TYPE(CMATRIX)   :: DEN_MAT, DEN_MAT_SQUARE
   LOGICAL :: DEBUG
   
   IF(.not.ALLOCATED(DEN_MAT%element)) THEN
       ALLOCATE(DEN_MAT%element(psi%d_dim**psi%n_vectors,psi%d_dim**psi%n_vectors))
   END IF
   
   !Check whether the state is separable
   IF (.NOT.psi%separable) THEN
   !If not then proceed to compute the density matrix
      PRINT*, "State is not separable, proceeding to compute Density matrix" 
      DEN_MAT = OUTER_PROD_vec(psi%coeffs, psi%coeffs)
   ELSE
   !Else write the non-separable multistate basis state and compute the matrix only then
      PRINT*, "State is separable, proceeding to write it in the multistate basis" 
      new_psi_not_sep = PSI_SEPARABLE_TO_MULTISTATE(psi, DEBUG)
      PRINT*, "Proceeding to compute the multistate matrix" 
      DEN_MAT = OUTER_PROD_vec(new_psi_not_sep%coeffs, new_psi_not_sep%coeffs)
   END IF
   
   IF (DEBUG) THEN
      PRINT*, "Check whether density matrix is self-adjoint ->", &
      & assert_is_adjoint(DEN_MAT%element, DEN_MAT%element, .FALSE. , .TRUE. )
      WRITE(*, '(G0)', ADVANCE="NO") "Check whether density matrix diagonal entries are real -> "
      DO ii = 1, DEN_MAT%dims(1)
         WRITE(*, '(G0)', ADVANCE="NO") assert_eq(DIMAG(DEN_MAT%element(ii,ii)), 0d0, .FALSE., .FALSE.)
      END DO
      WRITE(*,*) char(10)
      PRINT*, "Trace of Matrix is:", Mat_Trace(DEN_MAT) 
      PRINT*, "Trace is equal to 1 ->", assert_eq(DREAL(Mat_Trace(DEN_MAT)), 1d0, .FALSE., .FALSE.)
      
      !Check whether rho_sq = rho, it should hold for construction
      IF(.not.ALLOCATED(DEN_MAT_SQUARE%element)) THEN
       ALLOCATE(DEN_MAT_SQUARE%element(psi%d_dim**psi%n_vectors,psi%d_dim**psi%n_vectors))
      END IF
      DEN_MAT_SQUARE%dims(1) = DEN_MAT%dims(1)
      DEN_MAT_SQUARE%dims(2) = DEN_MAT%dims(2)
      DEN_MAT_SQUARE%element = MATMUL(DEN_MAT%element,DEN_MAT%element)
      PRINT*, "Rho^2 equal to Rho ->", assert_eq(DEN_MAT_SQUARE, DEN_MAT, .FALSE., .FALSE.)
      DEALLOCATE(DEN_MAT_SQUARE%element)
   END IF
      
END FUNCTION

!General function to compute outer product between x and y
!y must be conjugated for definition
FUNCTION OUTER_PROD_vec(x,y) RESULT(res)

   TYPE(CMATRIX) :: res
   DOUBLE COMPLEX, DIMENSION(:) :: x,y

   !initialize
   res%dims(1) = SIZE(x)
   res%dims(2) = SIZE(y)
   IF(.not.ALLOCATED(res%element)) THEN
       ALLOCATE(res%element(res%dims(1),res%dims(2)))
   END IF

   res%element = MATMUL(RESHAPE(x, (/res%dims(1),1/)), RESHAPE(CONJG(y), (/1,res%dims(2)/)))
   
END FUNCTION

!Function to compute coefficients in the multistate basis, starting from
!a one-state basis and a number n_vectors of separable subsystems
FUNCTION PSI_SEPARABLE_TO_MULTISTATE(psi_separable, DEBUG) RESULT(psi_multi_state)
   TYPE(PSI_STATE), INTENT(IN) :: psi_separable
   LOGICAL                     :: DEBUG 
   TYPE(PSI_STATE)             :: psi_multi_state
   
   REAL*8  :: norm
   INTEGER :: rest, temp, index_separable, max_index
   
   psi_multi_state%d_dim     = psi_separable%d_dim
   psi_multi_state%n_vectors = psi_separable%n_vectors
   psi_multi_state%separable = .FALSE.
   
   IF(.not.ALLOCATED(psi_multi_state%coeffs)) THEN
       ALLOCATE(psi_multi_state%coeffs(psi_multi_state%d_dim**psi_multi_state%n_vectors))
   END IF
   max_index = psi_multi_state%d_dim**psi_multi_state%n_vectors
   psi_multi_state%coeffs = DCMPLX(1d0, 0d0)
   
   !ONLY IF separable case
   IF(psi_separable%separable) THEN
   !compute the coefficients of the multistate basis: reasoning is really 
   !similar to write a number (which will be the index ii i.e. ii-th elem of vector)
   !in base that is "d_dim" and using a number "n_vectors" of digits
   DO ii = 0 , max_index - 1
   
      !Copy the index to a temporary variable
      temp  = ii
      
      !Compute the running index that selects the proper coefficient in the
      !separable wavefunction
      !here we iterate over particles
      DO jj = 1, psi_multi_state%n_vectors
         rest = MOD(temp, psi_multi_state%d_dim)         
         index_separable = 1 + rest + (psi_multi_state%n_vectors - jj)*psi_multi_state%d_dim
         IF (DEBUG) THEN
            WRITE(*,'(G0, G0, G0)', ADVANCE = "NO") "jj ->", jj, "  "
            WRITE(*,'(G0, G0, G0)', ADVANCE = "NO") "index -> ", index_separable, " ##### "
         END IF
         psi_multi_state%coeffs( ii + 1 ) = &
         & psi_multi_state%coeffs( ii + 1 )*psi_separable%coeffs(index_separable)
         temp = (temp - rest)/psi_multi_state%d_dim
      END DO
      IF (DEBUG) THEN
         PRINT*, char(10) 
      END IF
   END DO
   END IF
   
   norm =  SQRT(SUM(ZABS(psi_multi_state%coeffs(:))**2))
   psi_multi_state%coeffs(:) = psi_multi_state%coeffs(:)/norm
   
END FUNCTION

!Function to compute the trace over an indexed particle, it returns a matrix that is the reduced
!density matrix
FUNCTION TRACE_OVER_PARTICLE(index_particle, density_matrix, n_particles, d_dim, DEBUG) RESULT(red_den_mat)

TYPE(CMATRIX), INTENT(IN)  :: density_matrix
INTEGER, INTENT(IN)        :: n_particles, d_dim, index_particle
LOGICAL, INTENT(IN)        :: DEBUG

!internal variables
TYPE(CMATRIX) :: red_den_mat, red_den_mat_sq 
INTEGER       :: address_row, address_column, counter, period
INTEGER, DIMENSION(:), ALLOCATABLE :: starting_index_arr

!New reduced matrix has dimensions d_dim^(npart-1)
red_den_mat%dims(1) = d_dim**(n_particles-1)
red_den_mat%dims(2) = d_dim**(n_particles-1)

!Initialize the reduced density matrix
IF(.not.ALLOCATED(red_den_mat%element)) THEN
   ALLOCATE(red_den_mat%element(red_den_mat%dims(1),red_den_mat%dims(2) ))
END IF
red_den_mat%element = DCMPLX(0d0, 0d0)

ALLOCATE(starting_index_arr(d_dim**(n_particles-1)))

!Reduced density matrix elements
!Find the starting index for each row/column of the reduced density matrix
counter = 0
DO ii = 0, d_dim**n_particles-1
    IF (MOD(ii/(d_dim**(n_particles-index_particle)),d_dim).EQ.0) THEN
       counter = counter + 1
       starting_index_arr(counter) = ii
    END IF
END DO
!Shift the indeces by 1 to be Fortran friendly
starting_index_arr = starting_index_arr + 1
!Compute the "periodicity" of the index
period = d_dim**(n_particles - index_particle)
DO ii = 1, red_den_mat%dims(1)
   DO jj = 1, red_den_mat%dims(2)
       DO kk = 0, d_dim - 1
         address_row    = starting_index_arr(ii) + kk*period
         address_column = starting_index_arr(jj) + kk*period
         IF (DEBUG) THEN
            PRINT*, "rho_red(", ii+1 ,";", jj+1, ") <-", address_row, address_column
         END IF
         red_den_mat%element(ii,jj) = red_den_mat%element(ii,jj) + &
            & density_matrix%element( address_row, address_column)
       END DO
    END DO
END DO

DEALLOCATE(starting_index_arr)

IF (DEBUG) THEN
      PRINT*, "Check whether density matrix is self-adjoint ->", &
      & assert_is_adjoint(red_den_mat%element, red_den_mat%element, .FALSE. , .TRUE. )
      WRITE(*, '(G0)', ADVANCE="NO") "Check whether density matrix diagonal entries are real -> "
      DO ii = 1, red_den_mat%dims(1)
         WRITE(*, '(G0)', ADVANCE="NO") assert_eq(DIMAG(red_den_mat%element(ii,ii)), 0d0, .FALSE., .FALSE.)
      END DO
      WRITE(*,*) char(10)
      PRINT*, "Trace of Matrix is:", Mat_Trace(red_den_mat) 
      PRINT*, "Trace is equal to 1 ->", assert_eq(DREAL(Mat_Trace(red_den_mat)), 1d0, .FALSE., .FALSE.)
      
      !Check whether rho_sq = rho, it should hold for construction
      IF(.not.ALLOCATED(red_den_mat_sq%element)) THEN
       ALLOCATE(red_den_mat_sq%element(d_dim**(n_particles-1),d_dim**(n_particles-1)))
      END IF
      red_den_mat_sq%element = MATMUL(red_den_mat_sq%element,red_den_mat_sq%element)
      PRINT*, "Rho^2 equal to Rho ->", assert_eq(red_den_mat_sq, red_den_mat, .FALSE., .FALSE.)
      DEALLOCATE(red_den_mat_sq%element)
END IF

END FUNCTION

END MODULE



PROGRAM DENSITY_MATRICES

   USE CHECKPOINT_DEBUG_MODULE
   USE MY_MATRIX_MODULE
   USE HERMITIAN_MATRICES_UTILS
   USE DENSITY_MAT_UTILITIES
   
   IMPLICIT NONE

   !Debug flags
   LOGICAL :: DEBUG = .FALSE.
   LOGICAL :: PRINT_OUTPUT = .FALSE.
   LOGICAL :: EXIT_ON_ERROR = .TRUE.
   
   !Number of subsystems and dimension of Hilbert space
   INTEGER :: n_sub, n_dim
   
   !Whether states are separable
   LOGICAL :: sep_flag = .FALSE.
   
   TYPE(PSI_STATE) :: psi_wv
   TYPE(CMATRIX)   :: density_matr, density_matrix_reduced
   DOUBLE COMPLEX, DIMENSION(:), ALLOCATABLE :: coefficients
   INTEGER :: ii
   CHARACTER(1) :: coefficients_set
   
   n_dim = 2
   n_sub = 2
   ALLOCATE(coefficients(4))
   
IF(COMMAND_ARGUMENT_COUNT() == 1) THEN

  CALL GET_COMMAND_ARGUMENT(1, coefficients_set)
  SELECT CASE (coefficients_set)
  CASE ("0")
     !Initialize using random coefficients
     PRINT*, "Initialize using random coefficients."
     PRINT*, char(10)
     psi_wv = INIT_PSI_STATE_RANDOM(n_dim, n_sub, sep_flag, DEBUG)
     
  CASE ("1")
     !Initialize Bell State!
     coefficients = (/DCMPLX(0d0, 0d0),DCMPLX(-1d0, 0d0),DCMPLX(1d0, 0d0),DCMPLX(0d0, 0d0)/)
     PRINT*, "Coefficients are:"
     DO ii = 1,4
        PRINT*, coefficients(ii)
     END DO
     PRINT*, char(10)
     psi_wv = INIT_PSI_STATE_COEFFS(coefficients, sep_flag, 2, 2 )
     
  CASE ("2")
  
     coefficients = (/DCMPLX(1/sqrt(2d0), 0d0),DCMPLX(1/sqrt(2d0), 0d0),DCMPLX(1d0, 0d0),DCMPLX(0d0, 0d0)/)
     psi_wv = INIT_PSI_STATE_COEFFS(coefficients, sep_flag, 2, 2 )
     PRINT*, "Coefficients are:"
     DO ii = 1,4
         PRINT*, coefficients(ii)
     END DO
     PRINT*, char(10)
     
  CASE ("3")
     coefficients = (/DCMPLX(1/sqrt(2d0), 0d0),DCMPLX(0d0, 0d0),DCMPLX(0d0, 0d0),DCMPLX(-1/sqrt(2d0), 0d0)/)
     psi_wv = INIT_PSI_STATE_COEFFS(coefficients, sep_flag, 2, 2 )
     
     PRINT*, "Coefficients are:"
     DO ii = 1,4
        PRINT*, coefficients(ii)
     END DO
     PRINT*, char(10)
     
  CASE DEFAULT 
     PRINT*, "##########  Argument not valid  ###########"
     PRINT*, "Please provide any of the following argument in order to properly initialize coefficients"
     PRINT*, "0 ==> Coefficients are drawn randomly"
     PRINT*, "1 ==> Coefficients are [(0,0),(-1,0),(1,0),(0,0)]"
     PRINT*, "2 ==> Coefficients are [(1/sqrt(2),0),(1/sqrt(2),0), (1,0), (0,0)]"
     PRINT*, "3 ==> Coefficients are [(1/sqrt(2),0),(0,0), (1,0), (-1/sqrt(2),0)]"
     DEALLOCATE(coefficients)
     CALL EXIT(5)
  END SELECT
   
ELSE 
   PRINT*, "Please provide any of the following argument in order to properly initialize coefficients"
   PRINT*, "0 ==> Coefficients are chosen at random"
   PRINT*, "1 ==> Coefficients are [(0,0),(-1,0),(1,0),(0,0)]"
   PRINT*, "2 ==> Coefficients are [(1/sqrt(2),0), (1/sqrt(2),0), (1,0), (0,0)]"
   PRINT*, "3 ==> Coefficients are [(1/sqrt(2),0), (0,0), (1,0), (-1/sqrt(2),0)]"
   DEALLOCATE(coefficients)
   CALL EXIT(5)
END IF


   !Check whether nn, n_dim are greater than zero
   IF (DEBUG) THEN
      PRINT*, "Number of subsystems is greater than zero", &
      assert_positive(n_sub, PRINT_OUTPUT, EXIT_ON_ERROR)
      PRINT*, "Hilbert space dimension is greater than zero", &
      assert_positive(n_dim, PRINT_OUTPUT, EXIT_ON_ERROR)
   END IF
   
   
   !prova = INIT_PSI_STATE_RANDOM(n_dim, n_sub, sep_flag, DEBUG)
   density_matr = DENSITY_MATRIX(psi_wv, DEBUG)
   
   IF (.TRUE.) THEN
      PRINT*, "Density matrix:", char(10)
      CALL PRINT_CMATRIX_OUTPUT(density_matr)
   END IF
   
   !TRACE_OVER_PARTICLE(index_particle, density_matrix, n_particles, d_dim, DEBUG)
   density_matrix_reduced = TRACE_OVER_PARTICLE(1, density_matr, n_sub, n_dim, DEBUG)
   IF (.TRUE.) THEN
      PRINT*, "Reduced density matrix tracing over subsystem 1:", char(10)
      CALL PRINT_CMATRIX_OUTPUT(density_matrix_reduced)
   END IF
   
   density_matrix_reduced = TRACE_OVER_PARTICLE(2, density_matr, n_sub, n_dim, DEBUG)
   IF (.TRUE.) THEN
      PRINT*, "Reduced density matrix tracing over subsystem 2:", char(10)
      CALL PRINT_CMATRIX_OUTPUT(density_matrix_reduced)
   END IF
   
   DEALLOCATE(psi_wv%coeffs)
   DEALLOCATE(density_matr%element)   
   DEALLOCATE(density_matrix_reduced%element)

   
END PROGRAM
