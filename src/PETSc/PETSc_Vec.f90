Module PETSc_Vec_Mod

#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscsys.h>
    use petscSys
    use petscVec
    use Constants_Mod
  Implicit None
  type :: PVec
    type(tvec) :: vec
  contains
    procedure :: Create => Create_PETSc_Vec
    procedure :: Destroy => Destroy_PETSc_Vec
    procedure :: Assemble => Assemble_PETSc_Vec
    procedure :: ConvTo => ConvTo_PETSc_Vec
    procedure :: ConvFrom => ConvFrom_PETSc_Vec
    procedure :: GetSize => GetSize_PETSc_Vec
    procedure :: VecView => VecView_PETSc_Vec
    procedure :: Reset => Reset_PETSc_Vec
    procedure :: Insert => InsertValue_PETSc_Vec
    procedure :: InsertVals => InsertValues_PETSc_Vec
    procedure :: Add => AddValue_PETSc_Vec
    procedure :: AddVals => AddValues_PETSc_Vec
    procedure :: SetAll => SetAll_PETSc_Vec
    procedure :: Copy => Copy_PETSc_Vec
    procedure :: Y_AXPY => Y_AXPY_Petsc_Vec
    procedure :: Scale => Scale_Petsc_Vec
    procedure :: ScalarMult => ScalarMult_Petsc_Vec
    procedure :: Norm1 => Norm1_Petsc_Vec
    procedure :: Norm2 => Norm2_Petsc_Vec
    procedure :: VecDot => VecDot_Petsc_vec
  end type

contains



Subroutine Create_PETSc_Vec(This,N_vec)
  Class(PVec), intent(inout) :: This
  Integer, intent(in) :: N_vec
  PetscErrorCode ierr
  call VecCreate(MPI_COMM_SELF,This%vec,ierr); CHKERRQ(ierr)
  call VecSetType(This%vec,'seq',ierr); CHKERRQ(ierr)
  call VecSetSizes(This%vec,PETSC_DECIDE,N_vec,ierr); CHKERRQ(ierr)
End Subroutine Create_PETSc_Vec


Subroutine Destroy_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Call VecDestroy(This%vec,ierr); CHKERRQ(ierr)
End Subroutine Destroy_PETSc_Vec


Subroutine Assemble_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  call VecAssemblyBegin(This%vec,ierr)
  call VecAssemblyEnd(This%vec,ierr)
End Subroutine Assemble_PETSc_Vec


Subroutine ConvTo_PETSc_Vec(This,Val_vec)
  !! Convert fortran vec to Petsc vec
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_Vec, ii
  Integer, allocatable, dimension(:) :: Pos_vec
  Real(kind=dp), intent(inout) :: Val_vec(:)
  N_Vec = Size(Val_vec)
  Allocate(Pos_vec(N_Vec))
  Do ii = 1, N_Vec
    Pos_vec(ii) = ii-1
  EndDo
  call VecSetValues(This%vec,N_vec,Pos_vec,Val_vec,INSERT_VALUES,ierr)
  Deallocate(Pos_vec)
  call This%Assemble()
End Subroutine ConvTo_PETSc_Vec


Subroutine ConvFrom_PETSc_Vec(This,Val_vec)
  !! Convert from petsc vec to fortran vec
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Integer :: N_Vec, ii
  Integer, allocatable, dimension(:) :: Pos_vec
  Real(kind=dp), intent(inout) :: Val_vec(:)
  call This%GetSize(N_Vec)
  If (N_Vec /= Size(Val_vec)) Then
    Write(*,*) "### Error ###"
    Write(*,*) "PVec and Output vec size do not match"
    Stop
  EndIf
  Allocate(Pos_vec(N_Vec))
  Do ii = 1, N_Vec
    Pos_vec(ii) = ii-1
  EndDo
  Call VecGetValues(This%vec,N_vec,Pos_vec,Val_vec,ierr)
  Deallocate(Pos_vec)
  ! call This%Destroy()
End Subroutine ConvFrom_PETSc_Vec


Subroutine GetSize_PETSc_Vec(This,N_GetSize)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Integer, intent(inout) :: N_GetSize
  Call VecGetSize(This%vec,N_GetSize,ierr)
End Subroutine GetSize_PETSc_Vec


Subroutine VecView_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  call VecView(This%vec,PETSC_VIEWER_STDOUT_WORLD,ierr); CHKERRQ(ierr)
End Subroutine VecView_PETSc_Vec


Subroutine Reset_PETSc_Vec(This)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  call VecZeroEntries(this%vec,ierr)
End Subroutine Reset_PETSc_Vec


Subroutine InsertValue_PETSc_Vec(this,Position,Value)
  Class(PVec), intent(inout) :: this
  PetscErrorCode ierr
  Real(kind=dp), intent(in) :: Value 
  Integer, intent(in) :: Position 
  call VecSetValue(this%vec,Position-1,Value,INSERT_VALUES,ierr)
End Subroutine InsertValue_PETSc_Vec


Subroutine InsertValues_PETSc_Vec(this,Positions,Values)
  Class(PVec), intent(inout) :: this
  PetscErrorCode ierr
  Real(kind=dp), dimension(:), intent(in) :: Values
  Integer, dimension(:), intent(in) :: Positions
  !!Indexing
  call VecSetValues(This%vec,Size(Positions),Positions-1,Values,INSERT_VALUES,ierr)
End Subroutine InsertValues_PETSc_Vec


Subroutine AddValue_PETSc_Vec(this,Position,Value)
  Class(PVec), intent(inout) :: this
  PetscErrorCode ierr
  Real(kind=dp), intent(in) :: Value 
  Integer, intent(in) :: Position
  call VecSetValue(this%vec,Position-1,Value,ADD_VALUES,ierr)
End Subroutine AddValue_PETSc_Vec


Subroutine AddValues_PETSc_Vec(this,Positions,Values)
  Class(PVec), intent(inout) :: this
  PetscErrorCode ierr
  Real(kind=dp), dimension(:), intent(in) :: Values
  Integer, dimension(:), intent(in) :: Positions
  !!Indexing
  call VecSetValues(this%vec,Size(Positions),Positions-1,Values,ADD_VALUES,ierr)
End Subroutine AddValues_PETSc_Vec


Subroutine SetAll_PETSc_Vec(This,Val_All)
  Class(PVec), intent(inout) :: This
  PetscErrorCode ierr
  Real(kind=dp), intent(in) :: Val_All
  call VecSet(This%vec,Val_All,ierr)
End Subroutine SetAll_PETSc_Vec


Subroutine Copy_PETSc_Vec(this,NewVec)
  class(pVec), intent(inout) :: this 
  class(pVec), intent(inout) :: NewVec
  PetscErrorCode ierr 
  call VecCopy(NewVec%vec,this%vec,ierr)
End Subroutine Copy_PETSc_Vec


Subroutine Norm1_Petsc_Vec(this,Value)
  !! the one norm, ||v|| = sum_i | v_i |. ||A|| = max_j || v_*j ||, maximum column sum
  class(pVec), intent(inout) :: this 
  Real(kind=dp), intent(inout) :: Value
  PetscErrorCode ierr
  call VecNorm(this%vec,0,Value,ierr)
End Subroutine Norm1_Petsc_Vec


Subroutine Norm2_Petsc_Vec(this,Value)
  !! the two norm, ||v|| = sqrt(sum_i |v_i|^2) 
  class(pVec), intent(inout) :: this 
  Real(kind=dp), intent(inout) :: Value
  PetscErrorCode ierr
  call VecNorm(this%vec,1,Value,ierr)
End Subroutine Norm2_Petsc_Vec


Subroutine Y_AXPY_Petsc_Vec(this,alpha,pVec_x)
  !! y = alpha * x + y
  class(pVec), intent(inout) :: this
  class(pVec), intent(in) :: pVec_x 
  Real(kind=dp), intent(in) :: alpha
  PetscErrorCode ierr 
  call VecAXPY(this%vec,alpha,pVec_x%vec,ierr)
End Subroutine Y_AXPY_Petsc_Vec


Subroutine Scale_Petsc_Vec(this,alpha)
  !! y = alpha * y
  class(pVec), intent(inout) :: this
  Real(kind=dp), intent(inout) :: alpha
  PetscErrorCode ierr 
  call VecScale(this%vec,alpha,ierr)
End Subroutine Scale_Petsc_Vec


Subroutine ScalarMult_Petsc_Vec(this,pVec_b,N)
  !! mult vectors in form in form:
  !! (a1,a2,a3) * (b1,b2,b3) = (a1b1,a2b2,a3b3)
  class(pVec), intent(inout) :: this
  class(pVec), intent(in) :: pVec_b
  Integer, intent(in) :: N
  PetscErrorCode ierr 
  Integer :: ii 
  Integer, dimension(N) :: Pos 
  Real(kind=dp), dimension(N):: Val_a, Val_b
  Do ii = 1, N 
    Pos(ii) = ii-1
  EndDo 
  call VecGetValues(this%vec,N,Pos,Val_a,ierr)
  call VecGetValues(pVec_b%vec,N,Pos,Val_b,ierr)
  Do ii = 1, N 
    Val_a(ii) = Val_a(ii) * Val_b(ii) 
  EndDo 
  call VecSetValues(this%vec,N,Pos,Val_a,INSERT_VALUES,ierr)
End Subroutine ScalarMult_Petsc_Vec


Subroutine VecDot_Petsc_vec(this,pVec_b,Value)
  class(pVec), intent(in) :: this
  class(pVec), intent(in) :: pVec_b
  PetscErrorCode ierr 
  Real(kind=dp), intent(out) :: Value 
  call VecDot(this%vec,pVec_b%vec,Value,ierr)
End Subroutine VecDot_Petsc_vec

End Module
