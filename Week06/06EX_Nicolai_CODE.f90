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



PROGRAM ONE_DIM_QA

USE CHECKPOINT_DEBUG_MODULE
USE MY_MATRIX_MODULE
USE HERMITIAN_MATRICES_UTILS

IMPLICIT NONE

!Debug flags
LOGICAL :: DEBUG = .FALSE.
LOGICAL :: PRINT_OUTPUT = .FALSE.
LOGICAL :: EXIT_ON_ERROR = .TRUE.

!Problem variables
REAL*8, DIMENSION(:), ALLOCATABLE  :: arrays_eigen
INTEGER :: ii
REAL*8  :: k_const, h_bar
INTEGER :: kk_number

!Lattice quantities
!LL is the total length of the interval
INTEGER :: nn, nn_half
REAL*8  :: dh, LL
REAL*8  :: mass, omega

!String variables
CHARACTER(12)  :: LL_str, res_str, mass_str, omega_str, kk_number_str
CHARACTER(120) :: filename_eigenvalues, filename_eigenvectors 

!Integer that returns info from the diagonalization subroutine
INTEGER :: diagon_info

!Complex matrices and initialization of the problem
TYPE(CMATRIX) :: laplacian_matr, potential_mat, Hamiltonian, kinetic_mat

IF(COMMAND_ARGUMENT_COUNT() == 5) THEN

  CALL GET_COMMAND_ARGUMENT(1, LL_str)
  READ(LL_str,*) LL
  CALL GET_COMMAND_ARGUMENT(2, res_str)
  READ(res_str,*) dh
  CALL GET_COMMAND_ARGUMENT(3, mass_str)
  READ(mass_str,*) mass
  CALL GET_COMMAND_ARGUMENT(4, omega_str)
  READ(omega_str,*) omega
  CALL GET_COMMAND_ARGUMENT(5, kk_number_str)
  READ(kk_number_str,*) kk_number
  
  PRINT*, "Total number of points is", INT(LL/dh) + 1
  
ELSE 

   PRINT*, "Please provide the following arguments when calling:"
   PRINT*, "Total length of the interval centered in the origin (float)"
   PRINT*, "Resolution in space dh (float), number of points will be (LL/dh + 1)"
   PRINT*, "Mass (float) "
   PRINT*, "Omega (float)"
   PRINT*, "Number of eigenfunctions and energy levels to be printed (int)"
  CALL EXIT(5)
END IF


filename_eigenvalues  = TRIM(ADJUSTL("energies_L"//TRIM(LL_str)//"_dx"//TRIM(res_str)//".dat"))  
filename_eigenvectors = TRIM(ADJUSTL("eigenvectors_L"//TRIM(LL_str)//"_dx"//TRIM(res_str)//".dat")) 

!Fix the parameters for the model
nn = NINT(LL/dh) + 1
nn_half = NINT(0.5*LL/dh)
h_bar = 1d0
k_const = h_bar*h_bar/(2*mass*dh*dh)
ALLOCATE(arrays_eigen(nn))

!Some preconditions to be checked
IF (DEBUG) THEN
   PRINT*, "Check signs of the input values"
   PRINT*, "Interval length is positive: ", assert_positive(LL, PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Resolution is positive: ", assert_positive(dh, PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Mass is positive: ", assert_positive(mass, PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Omega is positive: ", assert_positive(omega, PRINT_OUTPUT, EXIT_ON_ERROR)
   PRINT*, "Number of eigenv/eigenf wanted is positive: ", assert_positive(kk_number, PRINT_OUTPUT, EXIT_ON_ERROR)
END IF

!discrete_laplacian_oper(matrix_size)
laplacian_matr = discrete_laplacian_oper(nn)

!discrete_potential_oper(n_points, dx, omega, m)
potential_mat  = discrete_potential_oper( nn , dh, omega, mass, DEBUG )
!Check if the O.A. potential is symmetric wrt the central element of the diagonal
IF (DEBUG) THEN
   PRINT*, "Check symmetry of the potential for O.A."
   DO ii = 1, nn_half
      WRITE(*, '(L1)', ADVANCE = "NO") assert_eq(potential_mat%element(nn_half + 1 + ii, nn_half + 1 + ii), &
      & potential_mat%element(nn_half + 1 - ii, nn_half + 1 - ii), PRINT_OUTPUT, EXIT_ON_ERROR  )
   END DO
   !Print newline
   PRINT*, char(10)
END IF

!Initialize the Hamiltonian matrix
ALLOCATE(Hamiltonian%element(nn,nn)) 
Hamiltonian%dims(1)  = nn
Hamiltonian%dims(2)  = nn
Hamiltonian%element  = -k_const*laplacian_matr%element + potential_mat%element

!Print only if the matrix is not so large, otherwise it is a mess... 
IF ((DEBUG).AND.(NN.LE.21)) THEN

   ALLOCATE(kinetic_mat%element(nn,nn))
   kinetic_mat%dims(1)  = nn 
   kinetic_mat%dims(2)  = nn 
   kinetic_mat%element = -k_const*laplacian_matr%element
   
   PRINT*, "#############################################################"
   PRINT*, "################## LAPLACIAN MATRIX #########################"
   PRINT*, "#############################################################"
   CALL PRINT_CMATRIX_OUTPUT(laplacian_matr)
   
   PRINT*, "#############################################################"
   PRINT*, "################### KINETIC MATRIX ##########################"
   PRINT*, "#############################################################"
   CALL PRINT_CMATRIX_OUTPUT(kinetic_mat)
   
   PRINT*, "#############################################################"
   PRINT*, "################## POTENTIAL MATRIX #########################"
   PRINT*, "#############################################################"
   
   CALL PRINT_CMATRIX_OUTPUT(potential_mat)
   PRINT*, "#############################################################"
   PRINT*, "################# HAMILTONIAN MATRIX ########################"
   PRINT*, "#############################################################"
   CALL PRINT_CMATRIX_OUTPUT(Hamiltonian)
END IF

!Check if the Hamiltonian is adjoint
IF (DEBUG) THEN
    PRINT*, "Check for Hamiltonian Matrix to be selfadjont" 
    WRITE(*, '(L1)', ADVANCE = "NO") assert_is_adjoint(Hamiltonian%element, Hamiltonian%element, PRINT_OUTPUT, EXIT_ON_ERROR)
    PRINT*, char(10)
END IF

!Diagonalize the Hamiltonian!
CALL HERMITIAN_EIGENVECTORS(Hamiltonian%element, nn, arrays_eigen, diagon_info)
IF (diagon_info.NE.0) then
    PRINT*,'DIAGONALIZATION FAILED'
    STOP
END IF

PRINT*, "Diagonalization has been successful!"

CALL print_eigenvalues_to_file(arrays_eigen, filename_eigenvalues, kk_number)

IF (DEBUG) THEN
   PRINT*, "Check if Hamiltonian matrix is square"
   PRINT*, assert_eq(Hamiltonian%dims(1), Hamiltonian%dims(2), PRINT_OUTPUT, EXIT_ON_ERROR)
END IF

CALL print_num_real_eigenvectors_to_file(Hamiltonian, filename_eigenvectors, kk_number, .TRUE., dh)

DEALLOCATE(laplacian_matr%element)
DEALLOCATE(potential_mat%element)
DEALLOCATE(Hamiltonian%element)
DEALLOCATE(arrays_eigen)

END PROGRAM
