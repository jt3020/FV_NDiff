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
  use Timing_Mod
  use Output_Mod
  
  Implicit None
  
  type(t_Problem) :: Problem 
  type(t_material), allocatable, dimension(:) :: Material
  type(t_Geometry) :: Geometry
  type(t_MatGen) :: MatGen
  Real(kind=dp) :: time_start, time_stop
  Real(kind=dp), allocatable, dimension(:) :: Flux

  time_start = Get_Time_Local()

  !! Read in problem
  call Problem%Create_Problem(Material)
  Write(*,'(g0)') "- Problem Created -"

  !! Create Geometry
  call Create_Geometry(Geometry,Problem)
  Write(*,'(g0)') "- Geometry Created -"

  !! Create and solve the system of equations
  call MatGen%Create(Geometry)
  call MatGen%Solve(Geometry,Material,Problem,Flux)
  Write(*,'(g0)') "- NDE Solved -"

  !! Generate Output
  call GenerateVTU(Problem,Geometry,Flux)

  !! Destroy problem
  call MatGen%Destroy(Flux)
  call Problem%Destroy_Problem(Material)
  Write(*,'(g0)') "- Problem Destroyed -"

  time_stop = Get_Time_Local()

  Write(*,'(g0)',advance='no') "- Code Executed in:"
  Write(*,'(E14.6)',advance='no') time_stop-time_start
  Write(*,'(g0)') " seconds -"
End program
