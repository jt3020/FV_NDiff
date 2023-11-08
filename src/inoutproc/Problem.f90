Module Problem_Mod

  use Constants_Mod
  use Materials_Mod
  use Errors_Mod
  !!Reads through an input deck and passes the required data to external routines

  Implicit None

  type, public :: t_Problem
    Integer :: N_Regions, Ref_x, Ref_y
    Integer :: N_Materials, Eigenvalue, N_Groups
    Integer, dimension(:), allocatable :: Region_Materials
    Integer, dimension(4) :: Boundary_Conditions
    real(kind=dp) :: System_Size_x, System_Size_y, Origin(2) = [0.0_dp, 0.0_dp]
    real(kind=dp), allocatable, dimension(:) :: Boundary_Pos
    character(len=1) :: Preconditioner
  contains
  !!Procedures which handle the storing, calculation and retrieval of material data
    procedure, public :: Create_Problem
    procedure, public :: Get_N_Regions
    procedure, public :: Get_Refinement_x
    procedure, public :: Get_Refinement_y
    procedure, public :: Get_System_Size_x
    procedure, public :: Get_System_Size_y
    procedure, public :: Get_Origin
    procedure, public :: Get_N_Materials
    procedure, public :: Get_N_Groups
    procedure, public :: Get_Eigenvalue
    procedure, public :: Get_Preconditioner
    procedure, public :: Get_Region_Materials
    procedure, public :: Get_Boundary_Pos
    procedure, public :: Get_Boundary_Conditions
    procedure, public :: Destroy_Problem
  end type

contains

Subroutine Create_Problem(this,Material)
  !!Read the Input File
  class(t_Problem) :: this
  type(t_material), allocatable, dimension(:) :: Material
  Integer :: ii
  Integer, parameter :: InputFile = 101
  Character(len=128) :: String_Read
  real(kind=dp) :: Sigma_a, Source


  !!Open input file containing problem specification
  Open(InputFile, File=Trim("./Inputs/default.in"), Status='Old')

  !! Geometry Specification

  !!Read in the number of Materials
  String_Read = ''
  Do While (String_Read .NE. 'Materials:')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Number of Materials not specified')
  EndDo
  Read(InputFile,*) this%N_Materials
  Allocate(Material(this%N_Materials))

  !!Read in the number of Regions
  String_Read = ''
  Do While (String_Read .NE. 'Regions:')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Number of Regions not specified')
  EndDo
  Read(InputFile,*) this%N_Regions

  !!Allocate the relevant arrays
  Allocate(this%Boundary_Pos(this%N_Regions+1))

  !!Read in the boundary positions
  String_Read = ''
  Do While (String_Read .NE. 'Boundaries:')
    Read(InputFile,*) String_Read
  EndDo
  Do ii = 1, this%N_Regions+1
    Read(InputFile,*) this%Boundary_Pos(ii)
  EndDo

  !!Read in the length of the system in the x direction
  String_Read = ''
  Do While (String_Read .NE. 'Length_x:')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Length_x not specified')
  EndDo
  Read(InputFile,*) this%System_Size_x

  !!Read in the length of the system in the y direction
  String_Read = ''
  Do While (String_Read .NE. 'Length_y:')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Length_y not specified')
  EndDo
  Read(InputFile,*) this%System_Size_y

  !!Read in the x refinement in each region
  String_Read = ''
  Do While (String_Read .NE. 'Refinement_x:')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Refinement_x not specified')
  EndDo
  Read(InputFile,*) this%Ref_x

  !!Read in the y refinement in each region
  String_Read = ''
  Do While (String_Read .NE. 'Refinement_y:')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Refinement_y not specified')
  EndDo
  Read(InputFile,*) this%Ref_y

  !!Read in the material names for each region - to be used to match later
  Allocate(this%Region_Materials(this%N_Regions))
  String_Read = ''
  Do While (String_Read .NE. 'Materials:')
    Read(InputFile,*) String_Read
  EndDo
  Do ii = 1, this%N_Regions
    Read(InputFile,*) this%Region_Materials(ii)
  EndDo

  !!Read in the boundary conditions of the problem
  String_Read = ''
  Do While (String_Read .NE. 'Boundary_Conditions:')
    Read(InputFile,*) String_Read
  EndDo
  Do ii = 1, 4
    Read(InputFile,*) String_Read
    If (String_Read == 'Zero') Then 
      this%Boundary_Conditions(ii) = 0
    ElseIf (String_Read == 'Reflective') Then 
      this%Boundary_Conditions(ii) = 1
    ElseIf (String_Read == 'Vacuum') Then 
      this%Boundary_Conditions(ii) = 2
    Else 
      call Stop_Error('Problem - ReadInput: Boundary Condition not recognised')
    EndIf
  EndDo

  print *, ' InputFile: Geometry Specification Complete'

  !! Simulation Specification
  !! Read in the problem type
  String_Read = ''
  Do While (String_Read .NE. 'Problem')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Problem Type not specified')
  EndDo
  Read(InputFile,*) String_Read
  If (String_Read == 'Fixed') Then 
    this%Eigenvalue = 0
  ElseIf (String_Read == 'Eigenvalue') Then 
    this%Eigenvalue = 1
  Else 
    call Stop_Error('Problem - ReadInput: Problem Type not recognised')
  EndIf

  !! Read in the preconditioner
  String_Read = ''
  Do While (String_Read .NE. 'Preconditioner')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Preconditioner not specified')
  EndDo
  Read(InputFile,*) this%Preconditioner

  !! Read in the number of energy groups
  String_Read = ''
  Do While (String_Read .NE. 'Energy_Groups')
    Read(InputFile,*) String_Read
    if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Energy Groups not specified')
  EndDo
  Read(InputFile,*) this%N_Groups

  print *, ' InputFile: Simulation Specification Complete'

  !! Material Specification
  !! Read in the material data
  Do ii = 1, this%N_Materials
    !! Loop to next material
    String_Read = ''
    Do While (String_Read .NE. 'Material')
      Read(InputFile,*) String_Read
      if (String_Read == 'End') call Stop_Error('Problem - ReadInput: Missing Material Specification')
    EndDo
    !! Read in the material name
    Read(InputFile,*) String_Read
    Call Material(ii)%SetName(String_Read)
    !! Read in the material properties
    String_Read = ''
    Do While (String_Read .NE. 'Sigma_a')
      Read(InputFile,*) String_Read
    EndDo
    Read(InputFile,*) Sigma_a
    Do While (String_Read .NE. 'S_0')
      Read(InputFile,*) String_Read
    EndDo
    Read(InputFile,*) Source
    Call Material(ii)%SetProps(Sigma_a, Source)
  End Do 

  print *, ' InputFile: Material Specification Complete'

  Close(InputFile)

End Subroutine Create_Problem

Function Get_N_Regions(this) Result(Res)
    class(t_problem) :: this
    Integer :: Res
    !!Get the number of regions in the problem
    Res = this%N_Regions
End Function Get_N_Regions

Function Get_Refinement_x(this) Result(Res)
    class(t_problem) :: this
    Integer :: Res
    !!Get the x refinement in each region
    Res = this%Ref_x
End Function Get_Refinement_x

Function Get_Refinement_y(this) Result(Res)
    class(t_problem) :: this
    Integer :: Res
    !!Get the y refinement in each region
    Res = this%Ref_y
End Function Get_Refinement_y

Function Get_System_Size_x(this) Result(Res)
    class(t_problem) :: this
    Real :: Res
    !!Get the x size of the problem
    Res = this%System_Size_x
End Function Get_System_Size_x

Function Get_System_Size_y(this) Result(Res)
    class(t_problem) :: this
    Real :: Res
    !!Get the y size of the problem
    Res = this%System_Size_y
End Function Get_System_Size_y

Function Get_Origin(this) Result(Res)
    class(t_problem) :: this
    Real :: Res(2)
    !!Get the origin of the problem
    Res = this%Origin
End Function Get_Origin

Function Get_N_Materials(this) Result(Res)
    class(t_problem) :: this
    Integer :: Res
    !!Get the number of materials in the problem
    Res = this%N_Materials
End Function Get_N_Materials

Function Get_N_Groups(this) Result(Res)
    class(t_problem) :: this
    Integer :: Res
    !!Get the number of energy groups in the problem
    Res = this%N_Groups
End Function Get_N_Groups

Function Get_Eigenvalue(this) Result(Res)
    class(t_problem) :: this
    Integer :: Res
    !!Get the eigenvalue flag
    Res = this%Eigenvalue
End Function Get_Eigenvalue

Function Get_Preconditioner(this) Result(Res)
    class(t_problem) :: this
    character(len=1) :: Res
    !!Get the preconditioner flag
    Res = this%Preconditioner
End Function Get_Preconditioner

Function Get_Region_Materials(this) Result(Res)
    class(t_problem) :: this
    integer, dimension(this%N_Regions) :: Res
    !!Get the material names in each region
    Res = this%Region_Materials
End Function Get_Region_Materials

Function Get_Boundary_Pos(this) Result(Res)
    class(t_problem) :: this
    Real(kind=dp), dimension(this%N_Regions+1) :: Res
    !!Get the positions of the boundaries in the problem
    Res = this%Boundary_Pos
End Function Get_Boundary_Pos

Function Get_Boundary_Conditions(this) Result(Res)
    class(t_problem) :: this
    Integer, dimension(4) :: Res
    !!Get the boundary condition of the problem
    Res = this%Boundary_Conditions
End Function Get_Boundary_Conditions

Subroutine Destroy_Problem(this,Material)
  !!Destroy the data stored in the problem class
  class(t_Problem) :: this
  type(t_material), allocatable, dimension(:) :: Material

  !!Deallocate the relevant arrays
  Deallocate(Material)
  Deallocate(this%Region_Materials,this%Boundary_Pos)
End Subroutine Destroy_Problem


End Module
