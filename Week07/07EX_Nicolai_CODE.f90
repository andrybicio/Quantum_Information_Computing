!http://www.fftw.org/fftw3_doc/Complex-One_002dDimensional-DFTs.html


MODULE DTT_UTILITIES

   !Libraries for the DFFT
   USE, INTRINSIC :: iso_c_binding
   IMPLICIT NONE
   INCLUDE 'fftw3.f03'

!Define a new type "grid" that can be used either for time or space (1D) 
   TYPE MY_1D_GRID
      !Variables for the lower bound, upper bound and size of a step
      REAL*8  :: dx, x_min, x_max 
      !Variables for how many intervals
      INTEGER :: size_grid
      !Grid itself
      REAL*8, DIMENSION(:), ALLOCATABLE :: elem_ii
   END TYPE
   
!Define a new type PSI that has the grid size as one argument 
!and second one is the value of wavefunction in that point
   TYPE WAVE_FUNCTION
      TYPE(MY_1D_GRID)                          :: coord_1d
      DOUBLE COMPLEX, DIMENSION(:), ALLOCATABLE :: values
   END TYPE
   
   !Index
   INTEGER, PRIVATE :: ii, jj

CONTAINS

!Constructs a 1d grid given its extremal points and the spacing
FUNCTION CONSTRUCTOR_1D_GRID(x_min, x_max, dx) RESULT(my_grid)
   IMPLICIT NONE 
   
   REAL*8, INTENT(IN) :: dx, x_min, x_max
   TYPE(MY_1D_GRID) :: my_grid
   
   my_grid%x_min = x_min
   my_grid%x_max = x_max
   my_grid%dx    = dx
      
   my_grid%size_grid = NINT((x_max-x_min)/dx) + 1
   
   ALLOCATE(my_grid%elem_ii(my_grid%size_grid))
   DO ii = 0, my_grid%size_grid-1
      my_grid%elem_ii(ii+1) = x_min + ii*dx
   END DO
   
END FUNCTION

!Dump a 1d grid to a file named "filename"
SUBROUTINE PRINT_GRID_TO_FILE(my_grid, filename)
   IMPLICIT NONE 
   TYPE(MY_1D_GRID), INTENT (IN)   :: my_grid
   CHARACTER(*), INTENT (IN)       :: filename
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write", STATUS = "replace")
   DO ii = 1, my_grid%size_grid
      WRITE(25, *) my_grid%elem_ii(ii)
   END DO
   
   CLOSE(25)
   
END SUBROUTINE

!Dump a 1d grid
SUBROUTINE PRINT_GRID_OUTPUT(my_grid)
   IMPLICIT NONE 
   TYPE(MY_1D_GRID), INTENT (IN)   :: my_grid
   
   DO ii = 1, my_grid%size_grid
      WRITE(*, "(G0)") my_grid%elem_ii(ii)
   END DO
   
END SUBROUTINE

!Dump on screen a dcomplex vector in the next format:
!coordinates , dcomplex numb
SUBROUTINE print_dcomplex_vector(w_vector)
   IMPLICIT NONE

   TYPE(WAVE_FUNCTION), INTENT(IN) :: w_vector
   
   DO ii = 1, SIZE(w_vector%values)
      WRITE(*,'(G0, A)', ADVANCE = "NO") w_vector%coord_1d%elem_ii(ii), ","
      WRITE(*,'(G0, A, G0)') REAL(w_vector%values(ii)), ",", AIMAG(w_vector%values(ii))
   END DO
   
END SUBROUTINE

!Dump into a file a dcomplex vector in the next format:
!coordinates , dcomplex numb
SUBROUTINE print_to_file_dcomplex_vector(w_vector, filename)
   IMPLICIT NONE

   TYPE(WAVE_FUNCTION), INTENT(IN) :: w_vector
   CHARACTER(*), INTENT(IN)        :: filename
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write", STATUS = "replace")
   DO ii = 1, SIZE(w_vector%values)
      WRITE(25,'(G0, A)', ADVANCE = "NO") w_vector%coord_1d%elem_ii(ii), ","
      WRITE(25,'(G0, A, G0)') REAL(w_vector%values(ii)), ",", AIMAG(w_vector%values(ii))
   END DO
   CLOSE(25)
   
END SUBROUTINE

!Dump on screen the norm of a dcomplex vector in the next format:
!coordinates , norm value
SUBROUTINE print_dcomplex_vector_sq(w_vector)
   IMPLICIT NONE

   TYPE(WAVE_FUNCTION), INTENT(IN) :: w_vector
   
   DO ii = 1, SIZE(w_vector%values)
      WRITE(*,'(G0, A)', ADVANCE = "NO") w_vector%coord_1d%elem_ii(ii), ","
      WRITE(*,'(G0)') REAL(w_vector%values(ii))**2 + AIMAG(w_vector%values(ii))**2
   END DO
   
END SUBROUTINE

SUBROUTINE print_to_file_dcomplex_vector_sq(w_vector, filename)
   IMPLICIT NONE

   TYPE(WAVE_FUNCTION), INTENT(IN) :: w_vector
   CHARACTER(*), INTENT(IN)        :: filename
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write", STATUS = "replace")
   DO ii = 1, SIZE(w_vector%values)
      WRITE(25,'(G0, A)', ADVANCE = "NO") w_vector%coord_1d%elem_ii(ii), ","
      WRITE(25,'(G0)') REAL(w_vector%values(ii))**2 + AIMAG(w_vector%values(ii))**2
   END DO
   CLOSE(25)
   
END SUBROUTINE

!This function returns only the main diagonal of the potential matrix that is dependent on t
FUNCTION COMPUTE_REAL_POTENTIAL_TIME_T(mass, omega, space_grid, time_inst, time_max) RESULT(potential)
   TYPE(MY_1D_GRID) :: space_grid
   REAL*8 :: mass, omega, time_inst, time_max, t_ratio
   
   REAL*8, DIMENSION(:), ALLOCATABLE :: potential
   
   IF(.not.ALLOCATED(potential)) THEN
       ALLOCATE(potential(space_grid%size_grid ))
   END IF
   
   t_ratio = time_inst/time_max
   DO ii = 1, space_grid%size_grid 
      potential(ii) = 0.5d0*mass*omega*(space_grid%elem_ii(ii) - t_ratio)**2
   END DO
   
END FUNCTION

SUBROUTINE PRINT_COORDINATES_MATRIX_OUTPUT(spacegrid, matrix, filename)
   REAL*8, DIMENSION(:,:), INTENT(IN) :: matrix
   TYPE(MY_1D_GRID), INTENT(IN)       :: spacegrid
   CHARACTER(*), INTENT(IN)           :: filename
   
   OPEN(UNIT = 25, FILE = filename, ACTION = "write", STATUS = "replace")
   DO ii = 1, spacegrid%size_grid
      WRITE(25,'(G0, A)', ADVANCE = "NO") spacegrid%elem_ii(ii), ","
      DO jj = 1, SIZE(matrix,2) - 1 
         WRITE(25,'(G0, A )', ADVANCE = "NO") matrix(ii,jj), ","
      END DO
      WRITE(25,'(G0, A )', ADVANCE = "NO") matrix(ii,SIZE(matrix,2)), char(10)
   END DO
   CLOSE(25)
   
END SUBROUTINE

   
   
END MODULE

PROGRAM TIME_DEP_QA_SCHROD


USE CHECKPOINT_DEBUG_MODULE
USE MY_MATRIX_MODULE
USE HERMITIAN_MATRICES_UTILS
USE DTT_UTILITIES

IMPLICIT NONE

!Debug flags
LOGICAL :: DEBUG = .FALSE.
LOGICAL :: PRINT_OUTPUT = .FALSE.
LOGICAL :: EXIT_ON_ERROR = .TRUE.


!Problem variables
REAL*8, DIMENSION(:), ALLOCATABLE  :: arrays_eigen
INTEGER :: ii
REAL*8  :: k_const, h_bar, omega, mass, const_pi, max_p

!New quantities ex07
REAL*8 :: t_min, t_max, tt_step, time_interval
REAL*8 :: x_min, x_max, dx_step, space_interval
!Potential vector
REAL*8, DIMENSION(:), ALLOCATABLE :: potential_vect_time

!Variables for the wavefunctions
TYPE(WAVE_FUNCTION) :: psi_0, psi_dt
!Temporary variables for the temporary psi, one in pos space and one in mom basis
TYPE(WAVE_FUNCTION) :: temp_psi_mom_sp, temp_psi_pos_sp

!Matrix that contains eigenvectors_sq at different timesteps
REAL*8, DIMENSION(:,:), ALLOCATABLE :: psi_sq_dt_matrix, potential_dt_matrix

!1D grid for space and time
TYPE(MY_1D_GRID) :: space_grid, time_grid, momenta_grid
INTEGER :: nn, nn_half

!Variables for the FT
TYPE(C_PTR) :: plan_forwardT, plan_backwardT


!String variables
CHARACTER(12)  :: x_min_str, x_max_str, dx_step_str
CHARACTER(12)  :: t_min_str, t_max_str, dt_step_str
CHARACTER(120) :: filename_potential, filename_wave_sq 

!Integer that returns info from the diagonalization subroutine
INTEGER :: diagon_info

!Complex matrices and initialization of the problem
TYPE(CMATRIX) :: laplacian_matr, potential_mat, Hamiltonian, kinetic_mat

!Fix the parameters for the model
omega = 1d0
mass  = 1d0
h_bar = 1d0
!Compute pi pi
const_pi = 2.*ASIN(1.)


IF(COMMAND_ARGUMENT_COUNT() == 6) THEN

  CALL GET_COMMAND_ARGUMENT(1, x_min_str)
  READ(x_min_str,*) x_min
  CALL GET_COMMAND_ARGUMENT(2, x_max_str)
  READ(x_max_str,*) x_max
  CALL GET_COMMAND_ARGUMENT(3, dx_step_str)
  READ(dx_step_str,*) dx_step
  CALL GET_COMMAND_ARGUMENT(4, t_min_str)
  READ(t_min_str,*) t_min
  CALL GET_COMMAND_ARGUMENT(5, t_max_str)
  READ(t_max_str,*) t_max
  CALL GET_COMMAND_ARGUMENT(6, dt_step_str)
  READ(dt_step_str,*) tt_step
  
  time_interval  = t_max - t_min
  space_interval = x_max - x_min

  PRINT*, "Space grid will be [", TRIM(x_min_str), ",",TRIM(x_max_str), "] ", " with spacing ", TRIM(dx_step_str)
  PRINT*, "Matrix size is :", INT(space_interval/dx_step) + 1, "x", INT(space_interval/dx_step) + 1
  
  PRINT*, "Total time interval is [",TRIM(t_min_str), ",",TRIM(t_max_str), "] ", " with spacing", TRIM(dt_step_str)
  PRINT*, "Number of intervals considered ", INT(time_interval/tt_step) + 1
   
  
ELSE 

   PRINT*, "Please provide the following arguments when calling:"
   PRINT*, "[x_min (float) ; x_max (float)] and resolution in space dx (float)"
   PRINT*, "[t_min (float) ; t_max (float)] and resolution in time dt (float)"
  CALL EXIT(5)
END IF

filename_potential = TRIM("potential_x_"//TRIM(x_min_str)//TRIM(x_max_str)//"_dx_"&
                     & //TRIM(dx_step_str)//"time_"//TRIM(t_max_str)//"_dt_"//TRIM(dt_step_str)//".dat")
filename_wave_sq = TRIM("sqwave_x_"//TRIM(x_min_str)//TRIM(x_max_str)//"_dx_"&
                     & //TRIM(dx_step_str)//"time_"//TRIM(t_max_str)//"_dt_"//TRIM(dt_step_str)//".dat")

!Construct the grids for the subroutines of DFTT
space_grid = CONSTRUCTOR_1D_GRID(x_min, x_max, dx_step)
nn = space_grid%size_grid 
nn_half = NINT(0.5*(space_grid%size_grid - 1))
IF (DEBUG) THEN
   CALL PRINT_GRID_OUTPUT(space_grid)
END IF

!Construct the time grid
time_grid  = CONSTRUCTOR_1D_GRID(t_min, t_max, tt_step)
IF (DEBUG) THEN
   CALL PRINT_GRID_OUTPUT(time_grid)
END IF

k_const = h_bar*h_bar/(2*mass*dx_step*dx_step)

!Compute the max in frequencies space
max_p = 2d0*const_pi/dx_step
	
!Construct the grid of p moments (i.e. frequencies) but in a different way than the usual constructor
ALLOCATE(momenta_grid%elem_ii(nn))
DO ii = 1, nn_half
    momenta_grid%elem_ii(ii) = (max_p/space_grid%size_grid)*(ii-1)
END DO
DO ii = nn_half, space_grid%size_grid
   momenta_grid%elem_ii(ii) = (max_p/space_grid%size_grid)*(ii-1) - max_p
END DO
momenta_grid%x_max = max_p
momenta_grid%x_min = momenta_grid%elem_ii(1)
momenta_grid%dx = momenta_grid%elem_ii(2) - momenta_grid%elem_ii(1)

!Allocate eigenvalues space
ALLOCATE(arrays_eigen(nn))

!discrete_laplacian_oper(matrix_size)
laplacian_matr = discrete_laplacian_oper(nn)

!discrete_potential_oper(n_points, dx, omega, m, debug)
potential_mat  = discrete_potential_oper( nn , dx_step, omega, mass, DEBUG )
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
   
   DEALLOCATE(kinetic_mat%element)
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

!Up to here it is the code of exercise06, in order to get the psi_0 vector
ALLOCATE(psi_0%values(nn))
ALLOCATE(psi_dt%values(nn))
ALLOCATE(temp_psi_pos_sp%values(nn))
ALLOCATE(temp_psi_mom_sp%values(nn))
ALLOCATE(potential_vect_time(nn))
ALLOCATE(psi_sq_dt_matrix(nn, time_grid%size_grid))
ALLOCATE(potential_dt_matrix(nn, time_grid%size_grid))

!Initializate the |psi_0> and the plan for the FFT
psi_0%coord_1d = space_grid
psi_0%values   = Hamiltonian%element(:,1)/SQRT(dx_step)
DEALLOCATE(Hamiltonian%element)

!Write the coordinates for the wave functions
psi_dt%coord_1d          = space_grid
temp_psi_pos_sp%coord_1d = space_grid
temp_psi_mom_sp%coord_1d = momenta_grid

!Print the ground state -> psi_0
IF (DEBUG) THEN
   PRINT*, "Ground state is: ", char(10)
   CALL print_dcomplex_vector(psi_0)
END IF

PRINT*, "Total time for the evolution of the system is: ", time_grid%x_max

psi_sq_dt_matrix(:,1) = DREAL(psi_0%values(:))**2 + DIMAG(psi_0%values(:))
potential_dt_matrix(:,1) = COMPUTE_REAL_POTENTIAL_TIME_T(mass, omega, space_grid, time_grid%elem_ii(1), time_interval)

DO ii = 2, time_grid%size_grid
   
   PRINT*, "Now considering time", time_grid%elem_ii(ii)
   !Prepare the plans for the the FFTW, this DOES not do the job, need the execute call
   !Forward transform:    positions -> momenta
   CALL dfftw_plan_dft_1d(plan_forwardT, space_grid%size_grid, &
                  & temp_psi_pos_sp%values, temp_psi_mom_sp%values, &
                  & FFTW_FORWARD, FFTW_ESTIMATE)

   !Inverse transform:    momenta -> positions
   CALL dfftw_plan_dft_1d(plan_backwardT, space_grid%size_grid, &
                  & temp_psi_mom_sp%values, temp_psi_pos_sp%values, &
                  & FFTW_BACKWARD, FFTW_ESTIMATE)


   !START preocedure
   potential_vect_time = COMPUTE_REAL_POTENTIAL_TIME_T(mass, omega, space_grid, time_grid%elem_ii(ii), time_interval)
   potential_dt_matrix(:,ii) = potential_vect_time
   temp_psi_pos_sp%values = EXP(DCMPLX(0d0, -0.5d0*tt_step*potential_vect_time(:) )) * psi_0%values(:)
   
   !Call the forward transform
   CALL dfftw_execute_dft(plan_forwardT, temp_psi_pos_sp%values, temp_psi_mom_sp%values)
   temp_psi_mom_sp%values = EXP(DCMPLX(0d0, -tt_step*(momenta_grid%elem_ii(:)**2)*0.5d0)) * temp_psi_mom_sp%values(:)
   
   !Normalize wrt the grid size!!
   temp_psi_mom_sp%values = temp_psi_mom_sp%values/SIZE(temp_psi_mom_sp%coord_1d%elem_ii)

   !Call the inverse transform
   CALL dfftw_execute_dft(plan_backwardT, temp_psi_mom_sp%values, temp_psi_pos_sp%values)
   psi_dt%values = EXP(DCMPLX(0d0, -0.5d0*tt_step*potential_vect_time(:) )) * temp_psi_pos_sp%values(:)
    
   !Check for normalization
   IF (DEBUG) THEN
      PRINT*, SUM( DREAL(psi_dt%values(:) )**2 + DIMAG( psi_dt%values(:) )**2)*psi_dt%coord_1d%dx
   END IF
   
   !Deallocation and destruction part
   CALL dfftw_destroy_plan(plan_forwardT)
   CALL dfftw_destroy_plan(plan_backwardT)
   
   psi_sq_dt_matrix(:,ii) = DREAL(psi_dt%values(:))**2 + DIMAG(psi_dt%values(:))**2
   psi_0%values = psi_dt%values
   
END DO  
   
   !Print psi_dt
IF (DEBUG) THEN
    PRINT*, "Transformed eigenvector is: ", char(10)
    CALL print_dcomplex_vector(psi_dt)
    CALL print_to_file_dcomplex_vector(psi_dt, "eigenv_debug.dat")
END IF

CALL PRINT_COORDINATES_MATRIX_OUTPUT(space_grid, psi_sq_dt_matrix, filename_wave_sq)
CALL PRINT_COORDINATES_MATRIX_OUTPUT(space_grid, potential_dt_matrix, filename_potential)

DEALLOCATE(arrays_eigen)
DEALLOCATE(potential_mat%element)
DEALLOCATE(laplacian_matr%element)
DEALLOCATE(time_grid%elem_ii)
DEALLOCATE(space_grid%elem_ii)

DEALLOCATE(psi_0%values)
DEALLOCATE(psi_dt%values)
DEALLOCATE(temp_psi_mom_sp%values)
DEALLOCATE(temp_psi_pos_sp%values)
DEALLOCATE(potential_vect_time)
DEALLOCATE(psi_sq_dt_matrix)
DEALLOCATE(potential_dt_matrix)


END PROGRAM
