Module MatGen_Mod

    use Constants_Mod
    use Materials_Mod
    use Problem_Mod
    use PETSc_Init_Mod
    use PETSc_Vec_Mod
    use PETSc_Mat_Mod
    use PETScSolver_Mod
    use CRS_Mod
    use Solver_Mod
    use Output_Mod
    use Geometry_Mod

    Implicit None

    !!Discretises the problem and sets up the system of equations which can then be solved
    type, public :: t_MatGen
        Integer :: N_Regions
        Real(kind=dp), allocatable, dimension(:) :: Vecb
        class(t_matrix_base), pointer :: matrix
        type(pMat) :: pMatA
        type(pVec) :: pVecb, pVecx
    contains
        procedure, public :: Create
        procedure, public :: Solve
        procedure, public :: Destroy
    end type
contains

Subroutine Create(this,Geometry)
    !!Set up Problem
    class(t_MatGen) :: this
    type(t_Geometry) :: Geometry
    Integer :: N_Els


    !!Extract relevant data from the Problem specification
    N_Els = Get_N_FiniteVolumes(Geometry)
    Allocate(this%Vecb(N_Els))

    !!Initialize PETSc
    call PETSc_Init()

    !!Generate the required PETSc Matrices and Vectors for the problem
    call this%pMatA%Create(N_Els,N_Els,50)
    call this%pVecb%Create(N_Els)
    call this%pVecx%Create(N_Els)
End Subroutine Create


Subroutine Solve(this,Geometry,Material,Problem,Vecx)
    !!Generate the system of equations to be fed into the solver
    class(t_MatGen) :: this
    type(t_Geometry) :: Geometry
    type(t_material), dimension(:) :: Material 
    type(t_Problem) :: Problem
    Integer :: ii, jj, N_Regions, N_Els, Region, Ref_x, Ref_y, FV_ID
    Integer :: MaterialID, N_Materials, Side
    Integer :: Boundary_Conditions(4), Neighbours(4)
    Real(kind=dp) :: A_Value, TotalCurrent, Current(4), b_Value, CentralPos(2), dx, dy
    Real(kind=dp), allocatable, dimension(:) :: Boundary_Pos, Sig_a, Source, D, Vecx


    !!Extract relevant data from the Problem specification
    N_Regions = Get_N_Regions(Problem)
    N_Materials = Get_N_Materials(Problem)
    Boundary_Conditions = Get_Boundary_Conditions(Problem)
    N_Els = Get_N_FiniteVolumes(Geometry)
    Allocate(Vecx(N_Els))


    !!Extract relevant material data from the materials class
    Allocate(Sig_a(N_Materials),Source(N_Materials),D(N_Materials))
    Do ii = 1, N_Materials
        Sig_a(ii) = Material(ii)%GetSig_a()
        Source(ii) = Material(ii)%GetS()
        D(ii) = Material(ii)%GetD()
    EndDo
    
    !! Apply boudnary conditions if relevant
    !! Loop over the regions
    FV_ID = 0
    Do Region = 1, N_Regions
        !! Loop over the nodes in the y direction
        Do ii = 1, Ref_y 
            !! Loop over the nodes in the x direction
            Do jj = 1, Ref_x 
                !! Increment the Finite Volume ID
                FV_ID = FV_ID + 1

                !! Get Finite Volume Information
                Neighbours = Get_FV_Neighbours(Geometry,FV_ID)

                !! Loop over the sides
                Do Side = 1, 4
                    !! Check if boundary
                    If (Neighbours(Side) /= 0) Cycle 
                    !! Check BC
                    If (Boundary_Conditions(Side) == 0) call this%pMatA%InsertVal(FV_ID,FV_ID,1E9_dp)
                End Do 
            End Do 
        End Do 
    End Do
    
    !!Assemble PETSc Matrix and Vector
    call this%pVecb%ConvTo(this%Vecb)
    call this%pMatA%Assemble()
    call this%pVecb%Assemble()

    !!Solve the problem
    call PETSc_Solve(this%pMatA,this%pVecb,this%pVecx)
    call this%pVecx%ConvFrom(Vecx)


End Subroutine Solve


Subroutine Destroy(this,Flux)
    !!Dismantle Problem
    class(t_MatGen) :: this
    Real(kind=dp), allocatable, dimension(:) :: Flux

    If(Allocated(Flux)) Deallocate(Flux)
    If(Allocated(this%Vecb)) Deallocate(this%Vecb)

    !!Destroy the PETSc Matrices and Vectors
    call this%pMatA%Destroy()
    call this%pVecb%Destroy()
    call this%pVecx%Destroy()

End Subroutine Destroy

End Module
