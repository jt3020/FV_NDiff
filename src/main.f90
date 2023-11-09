Program Main

  use Constants_Mod
  use Problem_Mod
  use Materials_Mod
  use MatGen_Mod
  use PETSc_Init_Mod
  use PETSc_Vec_Mod
  use PETSc_Mat_Mod
  use PETScSolver_Mod
  use CRS_Mod
  use Solver_Mod
  use Geometry_Mod
  
  Implicit None
  
  type(t_Problem) :: Problem 
  type(t_material), allocatable, dimension(:) :: Material
  type(t_Geometry) :: Geometry
  type(t_MatGen) :: MatGen
  Real(kind=dp) :: time_start, time_stop
  Real(kind=dp), allocatable, dimension(:) :: Flux

  call cpu_time(time_start)

  !! Read in problem
  call Problem%Create_Problem(Material)

  !! Create Geometry
  call Create_Geometry(Geometry,Problem)

  !! Create the system of equations
  call MatGen%Create(Geometry)

  !! Solve the system of equations
  call MatGen%Solve(Geometry,Material,Problem,Flux)

  !! Destroy problem
  call MatGen%Destroy(Flux)
  call Problem%Destroy_Problem(Material)

  call  cpu_time(time_stop)

  Write(*,'(g0)',advance='no') " >Problem Solved in:"
  Write(*,'(E14.6)',advance='no') time_stop-time_start
  Write(*,'(g0)') " seconds"
End program
