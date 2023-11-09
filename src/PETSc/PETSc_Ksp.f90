Module PETSc_Ksp_Mod

#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscsys.h>
    use petscSys
    use petscKsp
    use Constants_Mod
    use PETSc_Mat_Mod
    use PETSc_Vec_Mod
  Implicit None
  type :: pSol
    type(tksp) :: ksp
    Integer :: Max_Its
    Real(kind=dp) :: Rel_Tol, Abs_Tol
  contains
    procedure :: Create => Create_PETSc_Ksp
    procedure :: Destroy => Destroy_PETSc_Ksp
    procedure :: Analysis => Analysis_PETSc_Ksp
    procedure :: Solve => Solve_PETSc_Ksp
  end type

contains



Subroutine Create_PETSc_Ksp(this,A,PC_type,Rel_Tol,Abs_Tol,Max_Its)
  class(pSol) :: this
  class(pMat) :: A
  PC               Precon
  PetscErrorCode ierr
  Real(kind=dp) :: Rel_Tol, Abs_Tol
  Integer :: Max_Its
  Character :: PC_type

  this%Rel_Tol = Rel_Tol
  this%Abs_Tol = Abs_Tol
  this%Max_Its = Max_Its
  ! call omp_set_num_threads(4)
  call KSPCreate(MPI_COMM_SELF,this%ksp,ierr)
  call KSPSetOperators(this%ksp,A%mat,A%mat,ierr)
  call KSPGetPC(this%ksp,Precon,ierr)
  select case(PC_Type)
  case('J')
    call PCSetType(precon,PCJACOBI,ierr)
  case('C')
    call PCSetType(precon,PCICC,ierr)
  case('L')
    call PCSetType(precon,PCILU,ierr)
  case('H')
    call PCSetType(precon,PCHYPRE,ierr)
  case('S')
    call PCSetType(precon,PCSOR,ierr)
  case('G')
    call PCSetType(precon,PCGAMG,ierr)
  case('N')
    call PCSetType(precon,PCNONE,ierr)
  end select
  ! call KSPSetType(this%KSP,'KSPRICHARDSON')
  ! PetscCallA(KSPSetType(this%ksp,'python',ierr))
  call KSPSetTolerances(this%ksp,this%Rel_Tol,this%Abs_Tol,PETSC_DEFAULT_REAL,this%Max_Its,ierr)
  call KSPSetFromOptions(this%ksp,ierr)

  if (.false.) print *, 'false'

End Subroutine Create_PETSc_Ksp


Subroutine Destroy_PETSc_Ksp(this)
  class(pSol) :: this
  PetscErrorCode ierr
  call KSPDestroy(this%ksp,ierr)
End Subroutine Destroy_PETSc_Ksp


Subroutine Analysis_PETSc_Ksp(this)
  class(pSol) :: this
  PetscErrorCode ierr
  Integer :: Its, Reason
  Real(kind=dp) :: RNorm
  Call KSPGetIterationNumber(this%ksp,Its,ierr)
  Call KSPGetResidualNorm(this%ksp,RNorm,ierr)
  Call KSPGetConvergedReason(this%ksp,Reason,ierr)
  If (Reason < 0) Then
    Write(*,*) "---KSP Convergence Failed---"
    Write(*,*) "Failed after iterations:", Its, "with residual norm:", RNorm, "for reason:", Reason
    Write(*,*) "----------------------------"
    Select Case(Reason)
    Case(-3)
      Write(*,*) "Reason => Did not converge after required iterations"
      Write(*,*) "-------------------------------"
    Case(-4)
      Write(*,*) "Reason => Residual norm increased by Divtol"
      Write(*,*) "-------------------------------"
    Case(-5)
      Write(*,*) "Reason => Breakdown in method"
      Write(*,*) "-------------------------------"
    Case(-6)
      Write(*,*) "Reason => Initial residual orth to preconditioned initial residual"
      Write(*,*) "-------------------------------"
    Case(-7)
      Write(*,*) "Reason => Asymmetric matrix"
      Write(*,*) "-------------------------------"
    Case(-9)
      Write(*,*) "Reason => Residual term becan NaN"
      Write(*,*) "-------------------------------"
    Case Default
      Write(*,*) "Reason => Description not implemented"
      Write(*,*) "-------------------------------"
    End Select
  Else
    Write(*,*) "---KSP Convergence Succeeded---"
    Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
    Write(*,'(g0)',advance='no') Its
    Write(*,'(g0)',advance='no') "  with residual norm:"
    Write(*,'(E14.6)',advance='no') RNorm
    Write(*,'(g0)',advance='no') "  for reason  :"
    Write(*,'(g0)') Reason
    Write(*,*) "-------------------------------"
    Select Case(Reason)
    Case(2)
      Write(*,*) "Reason => Passed Relative Tolerance"
      Write(*,*) "-------------------------------"
    Case(3)
      Write(*,*) "Reason => Passed Absolute Tolerance"
      Write(*,*) "-------------------------------"
    Case Default
      Write(*,*) "Reason => Description not implemented"
      Write(*,*) "-------------------------------"
    End Select
  EndIf


End Subroutine Analysis_PETSc_Ksp


Subroutine Solve_PETSc_Ksp(this,b,x)
  Class(pSol) :: this
  class(pVec) :: b,x
  PetscErrorCode ierr
  ! call omp_set_num_threads(4)
  print *, 'Solving!' 
  call KSpSolve(this%ksp,b%vec,x%vec,ierr); CHKERRQ(ierr)
#   ifdef DEBUG
      call this%analysis()
#   endif

End Subroutine Solve_PETSc_Ksp



End Module
