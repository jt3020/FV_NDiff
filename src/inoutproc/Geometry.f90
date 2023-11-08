Module Geometry_Mod

  use Constants_Mod
  use Problem_Mod
  use StdLib_Mod

  implicit none
  private
  
  !! Geometry Type
  public t_Geometry
  
  !! Create and Destroy the type
  public Create_Geometry
  public Destroy_Geometry

  !! Get some information about the geometry
  public Get_N_FiniteVolumes

  !! The geometry type
  type t_Geometry
    integer :: N_FiniteVolumes                                       !! Number of finite volumes
    type(t_FiniteVolumes), allocatable :: FiniteVolumes(:)           !! Array of finite volumes
    logical :: initialised = .False.                                 !! Logical flag to indicate if the geometry has been initialised
  end type

  !! The finite volume type
  type t_FiniteVolumes
    integer :: MaterialID                                            !! Material ID of the finite volume
    real(kind=dp) :: Volume                                          !! Volume of the finite volume
    real(kind=dp) :: dx                                              !! Length of the finite volume in the x-direction
    real(kind=dp) :: dy                                              !! Length of the finite volume in the y-direction
    real(kind=dp), dimension(2) :: CentralPos                        !! Central position of the finite volume
  end type

contains

  Subroutine Create_Geometry(this,Problem)
    !! Create the geometry
    type(t_Geometry), intent(inout) :: this
    type(t_Problem), intent(in) :: Problem
    !! Local variables
    integer :: ii, jj, FV_ID
    integer :: N_x, N_y, N_Total, N_regions 
    integer :: MaterialID, Region
    real(kind=dp) :: Volume, dx, dy, Sys_x, Sys_y, Origin(2)

    !! Get number of finite volumes in system
    N_x = Get_Refinement_x(Problem)
    N_y = Get_Refinement_y(Problem)
    N_Regions = Get_N_Regions(Problem)
    N_Total = N_Regions * N_x * N_y
    allocate(this%FiniteVolumes(N_Total))

    !! Get system size and origin
    Sys_x = Get_System_Size_x(Problem)
    Sys_y = Get_System_Size_y(Problem)
    Origin = Get_Origin(Problem)

    !! Loop over the finite volumes and fill data
    !! Loop over the regions
    FV_ID = 0
    do Region = 1, N_Regions
      !! Loop over the finite volumes in the y-direction
      do ii = 1, N_y
        !! Loop over the finite volumes in the x-direction
        do jj = 1, N_x 
          !! Update FV_ID
          FV_ID = FV_ID + 1
          !! Material ID
          this%FiniteVolumes(FV_ID)%MaterialID = Region
          !! Dimensions and volume
          this%FiniteVolumes(FV_ID)%dx = Sys_x / N_x
          this%FiniteVolumes(FV_ID)%dy = Sys_y / (N_y*N_Regions)
          this%FiniteVolumes(FV_ID)%Volume = this%FiniteVolumes(FV_ID)%dx * this%FiniteVolumes(FV_ID)%dy
          !! Central Position
          this%FiniteVolumes(FV_ID)%CentralPos(0) = Origin(1) + ((jj-1) * this%FiniteVolumes(FV_ID)%dx) + (0.5_dp * this%FiniteVolumes(FV_ID)%dx)
          this%FiniteVolumes(FV_ID)%CentralPos(1) = Origin(2) + ((ii-1) * this%FiniteVolumes(FV_ID)%dy) + (0.5_dp * this%FiniteVolumes(FV_ID)%dy) &
                                                           + ((Region-1) * N_y * this%FiniteVolumes(FV_ID)%dy)
          !! Add some if statements here to check
          !! for boundary conditions and implement them in the 
          !! finite volume type
        end do
      end do
    end do

    this%N_FiniteVolumes = N_Total
    this%initialised = .True.

  End Subroutine Create_Geometry

  Subroutine Destroy_Geometry(this)
    !! Destroy the geometry
    type(t_Geometry), intent(inout) :: this

    deallocate(this%FiniteVolumes)
    this%N_FiniteVolumes = 0
    this%initialised = .False.
  End Subroutine Destroy_Geometry

  Function Get_N_FiniteVolumes(this) result(res)
    !! Get the number of finite volumes
    type(t_Geometry), intent(in) :: this
    integer :: res

    res = this%N_FiniteVolumes
  End Function Get_N_FiniteVolumes

End Module
