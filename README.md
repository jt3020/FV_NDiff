# Finite Volume Neutron Diffusion Code

## Description
A functional Neutron Diffusion code that discretises a domain into structured orthogonal cartesian mesh of finite volumes.

## Inputs
Input is in simple text format, with all information about the geometry, simulation and material data stored in one '$.in.$' file which '$Problem.f90$' reads through. Currently, the code just defaults to reading the file '$default.in$'.

## Structure of the code
The code is loosely divided into 4 seperate folders within the '$src/$' directory:

### inoutproc
Processes inputs and generates outpus. Effectively encapsulates the important parts of the code which aren't directly relevant to solving the NDE.
- $Problem.f90$: Reads in and stores information about the problem
- $Materials.f90$: Material class for sotrage of properties
- $Geometry.f90$: Calculates geometry parameters for later use

### centralproc
All code relevant to creating, storing and solving the NDE.
- $MatGen.f90$: Generates the system of equations used to solve the NDE
- $Matrix$ _ $Base.f90$: Matrix interface relevant for Non-PETSc solving 
- $CRS.f90$: Compressed Row Storage module relevant for Non-PETSc solving
- $Solver.f90$: Non-PETSc version of the solver
- $PETScSolver.f90$: PETSc version of the solver

### stdlib
A collection of modules containing vaguely useful routines such as distance calculations and timings.
- $Constants.f90$: Stortes relevant constants
- $Error.f90$: Formatted error routines
- $Progress.f90$: Formatted progress bars
- $StdLib.f90$: Library of useful routines that don't belong to a specific class
- $Timing.f90$: Timing module, avoids cpu_time issues if running parallel code

### PETSc
All of the PETSc modules required so solve an $Ax=b$ equation.
- $PETSc$ _ $Init.f90$: Initialises PETSc
- $PETSc$ _ $Ksp.f90$: PETSc Solver
- $PETSc$ _ $Mat.f90$: PETSc Matrix Routines
- $PETSc$ _ $Mat.f90$: PETSc Vector Routines

## Outputs

Output modules have been removed from the code as VTU format is unlikely what is required if this code is simply being used as an intermediate step for a point-kinetics solve. The solution flux is generated in both PETSc Vector and Fortran Vector format at the end of the solve, so should work for whatever application is needed.

## Adding further features

### Non-Cartesian geometries
This can be added in the matgen loop by adding the $r^c$ terms in as stated by Nakamura. Routines have already been written to calculate the central position of each FV so should be easy enough to add on.

### Eigenvalue and Multigroup
Encapsulate matgen in a loop over whatever iterations required. Scattering and fission can be added to material and problem input. 

### Other stuff
Everything should be extensible to whatever other applications would be required. This is structured such that it could be turned into a fully fledged 3D unstructured grid FV code with all the standard bells and whistles (MG, Adjoint, Eigenvalue etc.).