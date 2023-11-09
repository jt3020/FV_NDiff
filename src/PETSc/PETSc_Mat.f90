Module PETSc_Mat_Mod

#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscsys.h>
    use petscSys
    use petscMat
    use Constants_Mod
    use PETSc_Vec_Mod
    Implicit None

  type :: PMat
    type(tmat) :: mat
  contains
    procedure :: Create => Create_PETSc_Mat
    procedure :: Destroy => Destroy_PETSc_Mat
    procedure :: SwitchAssemble => SwitchAssemble_PETSc_Mat
    procedure :: Assemble => Assemble_PETSc_Mat
    procedure :: InsertVal => InsertVal_PETSc_Mat
    procedure :: AddVal => AddVal_PETSc_Mat
    procedure :: InsertMat => InsertMat_PETSc_Mat
    procedure :: AddMat => AddMat_PETSc_Mat
    procedure :: GetSizeMat => GetSizeMat_PETSc_Mat
    procedure :: GetValMat => GetValMat_PETSc_Mat
    procedure :: GetValsMat => GetValsMat_PETSc_Mat
    procedure :: MultMat => MultMat_PETSc_Mat
    procedure :: MultAddMat => MultAddMat_PETSc_Mat
    procedure :: MatView => MatView_PETSc_Mat
    procedure :: Symmetry => Symmetry_PETSc_Mat
  end type

contains


Subroutine Create_PETSc_Mat(this,N_row,N_col,N_nz)
  Class(PMat), intent(inout) :: this
  Integer :: N_row, N_col, N_nz
  Integer, dimension(N_nz) :: N_nz_arr
  PetscErrorCode ierr
  call MatCreateSeqAIJ(PETSC_COMM_SELF,N_row,N_col,N_nz,PETSC_NULL_INTEGER,this%mat,ierr)
  ! N_nz_arr = N_nz
  ! 
  ! call MatCreate(PETSC_COMM_WORLD,this%mat,ierr)
  ! CALL MatSetSizes(this%mat,PETSC_DECIDE,PETSC_DECIDE,N_row,N_col,ierr)
  
  ! CALL MatSetFromOptions(this%mat,ierr)
  ! CALL MatSetUp(this%mat,ierr)

  ! CALL MatSetType(this%mat,MATSEQAIJ,ierr)
  ! CALL MatSeqAIJSetPreallocation(this%mat,PETSC_DECIDE,PETSC_NULL_INTEGER,ierr)
  ! Create, Set Size, SetType, SeqIJ

End Subroutine Create_PETSc_Mat


Subroutine Destroy_PETSc_Mat(this)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Call MatDestroy(this%mat,ierr); CHKERRQ(ierr)
End Subroutine Destroy_PETSc_Mat


Subroutine SwitchAssemble_PETSc_Mat(this)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  call MatAssemblyBegin(this%mat,MAT_FLUSH_ASSEMBLY,ierr)
  call MatAssemblyEnd(this%mat,MAT_FLUSH_ASSEMBLY,ierr)
End Subroutine SwitchAssemble_PETSc_Mat


Subroutine Assemble_PETSc_Mat(this)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  call MatAssemblyBegin(this%mat,MAT_FINAL_ASSEMBLY,ierr)
  call MatAssemblyEnd(this%mat,MAT_FINAL_ASSEMBLY,ierr)
End Subroutine Assemble_PETSc_Mat


Subroutine InsertVal_PETSc_Mat(this,V_row,V_col,Value)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Integer :: V_row, V_Col
  Real(kind=dp) :: Value
  If ((V_row < 1) .OR. (V_col < 1)) Then
    Write(*,*) "### Error ###"
    Write(*,*) "Inserting pMat Value outside of bounds"
    Stop
  EndIf
  !!Convert the row and col integers to P format
  call MatSetValue(this%mat,V_row-1,V_Col-1,Value,INSERT_VALUES,ierr)
End Subroutine InsertVal_PETSc_Mat


Subroutine AddVal_PETSc_Mat(this,V_row,V_col,Value)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Integer :: V_row, V_Col
  Real(kind=dp) :: Value
  If ((V_row < 1) .OR. (V_col < 1)) Then
    Write(*,*) "### Error ###"
    Write(*,*) "Adding pMat Value outside of bounds"
    Stop
  EndIf
  !!Convert the row and col integers to P format
  call MatSetValue(this%mat,V_row-1,V_col-1,Value,ADD_VALUES,ierr)
End Subroutine AddVal_PETSc_Mat


Subroutine InsertMat_PETSc_Mat(this,V_rows,V_cols,Values)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  Integer :: V_rows(:), V_cols(:)
  Real(kind=dp) :: Values(:,:)
  N_rows = Size(V_rows)
  N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
  V_rows(:) = V_rows(:) - 1
  V_cols(:) = V_cols(:) -1
  call MatSetValues(this%mat,N_rows,V_rows,N_cols,V_cols,Values,INSERT_VALUES,ierr)
  !!Revert to F format
  V_rows(:) = V_rows(:) + 1
  V_cols(:) = V_cols(:) + 1
End Subroutine InsertMat_PETSc_Mat


Subroutine AddMat_PETSc_Mat(this,V_rows,V_cols,Values)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  Integer :: V_rows(:), V_cols(:)
  Real(kind=dp) :: Values(:,:)
  N_rows = Size(V_rows)
  N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
  V_rows(:) = V_rows(:) - 1
  V_cols(:) = V_cols(:) -1
  call MatSetValues(this%mat,N_rows,V_rows,N_cols,V_cols,Values,ADD_VALUES,ierr)
  !!Revert to F format
  V_rows(:) = V_rows(:) + 1
  V_cols(:) = V_cols(:) + 1
End Subroutine AddMat_PETSc_Mat


Subroutine GetSizeMat_PETSc_Mat(this,N_rows,N_cols)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  call MatGetSize(this%mat,N_rows,N_cols,ierr)
End Subroutine GetSizeMat_PETSc_Mat


Function GetValMat_PETSc_Mat(this,V_row,V_col) Result(Value)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  PetscInt V_row, V_col
  PetscScalar Value
  call MatGetValues(this%mat,1,[V_row-1],1,[V_col-1],[Value],ierr)
End Function GetValMat_PETSc_Mat


Subroutine GetValsMat_PETSc_Mat(this,V_rows,V_cols,Values)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Integer :: N_rows, N_cols
  Integer :: V_rows(:), V_cols(:)
  Real(kind=dp), dimension(:,:) :: Values
  N_rows = Size(V_rows)
  N_cols = Size(V_cols)
  !!Temporarily convert the row and col integers to P format
  V_rows(:) = V_rows(:) - 1
  V_cols(:) = V_cols(:) - 1
  call MatGetValues(this%mat,N_rows,V_rows,N_cols,V_cols,Values,ierr)
  !!Revert to F format
  V_rows(:) = V_rows(:) + 1
  V_cols(:) = V_cols(:) + 1
End Subroutine GetValsMat_PETSc_Mat


Subroutine MultMat_PETSc_Mat(this,In_Vec,Out_Vec)
  Class(PMat), intent(inout) :: this
  class(pVec) :: In_Vec, Out_Vec
  PetscErrorCode ierr
  call MatMult(this%mat,In_Vec%vec,Out_Vec%vec,ierr)
End Subroutine MultMat_PETSc_Mat


Subroutine MultAddMat_PETSc_Mat(this,In_Vec,Add_Vec,Out_Vec)
  Class(PMat), intent(inout) :: this
  Class(pVec) :: In_Vec, Add_Vec, Out_Vec
  PetscErrorCode ierr
  call MatMultAdd(this%mat,In_Vec%vec,Add_Vec%vec,Out_Vec%vec,ierr)
End Subroutine MultAddMat_PETSc_Mat


Subroutine MatView_PETSc_Mat(this)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  call MatView(this%mat,PETSC_VIEWER_STDOUT_WORLD,ierr)
End Subroutine MatView_PETSc_Mat


Subroutine Symmetry_PETSc_Mat(this,SymLog)
  Class(PMat), intent(inout) :: this
  PetscErrorCode ierr
  Logical :: SymLog
  call MatIsSymmetric(this%mat,1E-6_dp,SymLog,ierr)
End Subroutine Symmetry_PETSc_Mat

End Module
