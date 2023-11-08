Module StdLib_Mod

    use Constants_Mod
    Implicit None
    !!Standard Library for widely used routines

contains


Function factorial (n) result (res)
  !!From Rosetta code
  Integer, intent (in) :: n
  Integer :: res
  Integer :: i

  res = product ((/(i, i = 1, n)/))

End Function factorial


Function choose (n, k) result (res)
  !!From Rosetta code
  Integer, intent (in) :: n
  Integer, intent (in) :: k
  Integer :: res

  res = factorial (n) / (factorial (k) * factorial (n - k))

End Function choose


Recursive Function det(a,n,permanent) result(accumulation)
  !!Setting permanent to 1 computes the permanent.
  !!Setting permanent to -1 computes the determinant.
  Real(kind=dp), dimension(n,n), intent(in) :: a
  Integer, intent(in) :: n, permanent
  Real(kind=dp), dimension(n-1, n-1) :: b
  Real(kind=dp) :: accumulation
  Integer :: i, sgn
  If (n == 1) Then
    accumulation = a(1,1)
  Else
    accumulation = 0
    sgn = 1
    Do i=1, n
      b(:, :(i-1)) = a(2:, :i-1)
      b(:, i:) = a(2:, i+1:)
      accumulation = accumulation + sgn * a(1, i) * det(b, n-1, permanent)
      sgn = sgn * permanent
    Enddo
  Endif
End Function det


Subroutine Unique_IntList(vec,vec_unique)  
  !!Return only the unique values from vec.
  integer,dimension(:), intent(in) :: vec  
  integer,dimension(:), allocatable, intent(out) :: vec_unique

  integer :: i,num  
  logical, dimension(size(vec)) :: mask

  mask = .false.

  do i=1,size(vec)

      !count the number of occurrences of this element:  
      num = count( vec(i)==vec )

      if (num==1) then  
          !there is only one, flag it:  
          mask(i) = .true.  
      else  
          !flag this value only if it hasn't already been flagged:  
          if (.not. any(vec(i)==vec .and. mask) ) mask(i) = .true.  
      end if

  end do

  !return only flagged elements:  
  allocate( vec_unique(count(mask)) )  
  vec_unique = pack( vec, mask )
End Subroutine Unique_IntList


Function Get_Quadrilaterial_Area(Node1, Node2, Node3, Node4) Result(Area)
  !! Get the area of a quadrilaterial given the positions of four nodes with pos (x,y) or (x,y,z)
  !! Uses Brahmagupta's Formula
  Real(kind=dp), dimension(2), intent(in) :: Node1, Node2, Node3, Node4
  Real(kind=dp) :: Area, a, b, c, d, s, p , q

  !!Side lengths of quadrilateral
  a = SQRT(dot_product(Node1(:) - Node2(:), Node1(:) - Node2(:)))
  b = SQRT(dot_product(Node2(:) - Node3(:), Node2(:) - Node3(:)))
  c = SQRT(dot_product(Node3(:) - Node4(:), Node3(:) - Node4(:)))
  d = SQRT(dot_product(Node4(:) - Node1(:), Node4(:) - Node1(:)))

  p = SQRT(dot_product(Node1(:) - Node3(:), Node1(:) - Node3(:)))
  q = SQRT(dot_product(Node2(:) - Node4(:), Node2(:) - Node4(:)))

  !!Semi-perimeter of quadrilateral
  s = (a + b + c + d) / 2._dp

  !!Brahmagupta's Formula
  Area = 0.25_dp * SQRT( ( 4._dp*(p**2)*(q**2) ) - ( ( (a**2) + (c**2) - (b**2) - (d**2) )**2 )  )
End Function Get_Quadrilaterial_Area


Function Get_Average_Position_1D(Nodes) Result(Position)
  !!Get the average point of a polygon using nodal positions
  !!with N by M nodes and L dimensions
  Real(kind=dp), dimension(:,:), intent(in) :: Nodes
  Real(kind=dp), dimension(Size(Nodes,1)) :: Position
  Real(kind=dp) :: Sum
  Integer :: N, L, ii, jj

  N = Size(Nodes,2)
  L = Size(Nodes,1)
  Do ii = 1, L
    Sum = 0._dp 
    Do jj =1, N 
      Sum = Sum + Nodes(ii,jj)
    EndDo
    Sum = Sum / Real(N,dp)
    Position(ii) = Sum
  EndDo
End Function Get_Average_Position_1D


Function Get_Average_Position_List(Nodes) Result(Position)
  !!Get the average point of a polygon using nodal positions
  !!with N nodes and L dimensions
  Real(kind=dp), dimension(:,:), intent(in) :: Nodes
  Real(kind=dp), dimension(Size(Nodes,1)) :: Position
  Real(kind=dp) :: Sum
  Integer :: N, L, ii, jj, kk

  N = Size(Nodes,2)
  L = Size(Nodes,1)
  Do ii = 1, L
    Sum = 0._dp 
    Do jj =1, N 
      Sum = Sum + Nodes(ii,jj)
    EndDo
    Sum = Sum / Real(N,dp)
    Position(ii) = Sum
  EndDo
End Function Get_Average_Position_List


Function Get_Average_Position_List_Flip(Nodes) Result(Position)
  !!Get the average point of a polygon using nodal positions
  !!with N nodes and L dimensions
  Real(kind=dp), dimension(:,:), intent(in) :: Nodes
  Real(kind=dp), dimension(Size(Nodes,2)) :: Position
  Real(kind=dp) :: Sum
  Integer :: N, L, ii, jj, kk

  N = Size(Nodes,1)
  L = Size(Nodes,2)
  Do ii = 1, L
    Sum = 0._dp 
    Do jj =1, N 
      Sum = Sum + Nodes(jj,ii)
    EndDo
    Sum = Sum / Real(N,dp)
    Position(ii) = Sum
  EndDo
End Function Get_Average_Position_List_Flip


Function Get_Average_Position(Nodes) Result(Position)
  !!Get the average point of a polygon using nodal positions
  !!with N by M nodes and L dimensions
  Real(kind=dp), dimension(:,:,:), intent(in) :: Nodes
  Real(kind=dp), dimension(Size(Nodes,1)) :: Position
  Real(kind=dp) :: Sum
  Integer :: N, M, L, ii, jj, kk

  N = Size(Nodes,2)
  M = Size(Nodes,3)
  L = Size(Nodes,1)
  Do ii = 1, L
    Sum = 0._dp 
    Do jj =1, N 
      Do kk = 1, M
        Sum = Sum + Nodes(ii,jj,kk)
      EndDo
    EndDo
    Sum = Sum / Real(N*M,dp)
    Position(ii) = Sum
  EndDo
End Function Get_Average_Position


Function Get_Angle_2D_Radians(V1,V2) Result(Angle)
  !!Finds the angle between two vectors
  !!Given vectors of value (x,y)
  Real(kind=dp), dimension(2), intent(in) :: V1, V2
  Real(kind=dp) :: Angle
  Real :: Angle_Temp

  !! Method calculates the angle using single precision then converts
  !! to a double precision value, avoiding issues where 
  !! fn performs acos(+/- >1) due to machine precision
  Angle_Temp = dot_product(V1,V2) / ( ( SQRT( (V1(1)**2) + (V1(2)**2) ) ) * ( SQRT( (V2(1)**2) + (V2(2)**2) ) )  ) 
  Angle = ACOS(Real(Angle_Temp,dp))
End Function Get_Angle_2D_Radians


Function Get_Angle_2D_Degrees(V1,V2) Result(Angle)
  !!Finds the angle between two vectors
  !!Given vectors of value (x,y)
  Real(kind=dp), dimension(2), intent(in) :: V1, V2
  Real(kind=dp) :: Angle
  Real :: Angle_Temp

  Angle_Temp = dot_product(V1,V2) / ( ( SQRT( (V1(1)**2) + (V1(2)**2) ) ) * ( SQRT( (V2(1)**2) + (V2(2)**2) ) )  ) 
  Angle = ACOS(Real(Angle_Temp,dp))
  Angle = Angle*(180/PI)
End Function Get_Angle_2D_Degrees


Function Get_Angle_Cosine_2D(V1,V2) Result(Angle)
  !!Finds the cosine of the angle between two vectors
  !!Given vectors of value (x,y)
  Real(kind=dp), dimension(2), intent(in) :: V1, V2
  Real(kind=dp) :: Angle

  Angle = dot_product(V1,V2) / ( ( SQRT( (V1(1)**2) + (V1(2)**2) ) ) * ( SQRT( (V2(1)**2) + (V2(2)**2) ) )  ) 
End Function Get_Angle_Cosine_2D


Function Get_Normal_Vector(V1) Result(V2)
  !!Returns the normal of vector V1 in (x,y)
  Real(kind=dp), dimension(2), intent(in) :: V1
  Real(kind=dp), dimension(2) :: V2

  V2(1) = -V1(2)
  V2(2) = V1(1)
End Function Get_Normal_Vector


Function Get_Unit_Vector_2D(V1) Result(V2)
  !!Returns the unit of vector V1 in (x,y)
  Real(kind=dp), dimension(2), intent(in) :: V1
  Real(kind=dp), dimension(2) :: V2

  V2 = V1/SQRT(V1(1)**2 + V1(2)**2)
End Function Get_Unit_Vector_2D


Function Get_Unit_Vector_3D(V1) Result(V2)
  !!Returns the unit of vector V1 in (x,y)
  Real(kind=dp), dimension(3), intent(in) :: V1
  Real(kind=dp), dimension(3) :: V2

  V2 = V1/SQRT(V1(1)**2 + V1(2)**2 + V1(3)**2)
End Function Get_Unit_Vector_3D


Function Get_Distance_2D(Pos1,Pos2) Result(Dist)
  !!Returns the distance between two points
  Real(kind=dp), dimension(2), intent(in) :: Pos1, Pos2
  Real(kind=dp) :: Dist

  Dist = SQRT( (Pos1(1) - Pos2(1))**2 + (Pos1(2) - Pos2(2))**2 )
End Function Get_Distance_2D


Subroutine Stack_Increase_i1(Stack,NewSize)
  !!Increases the size of a stack
  !!Stack is a 1D array of integers
  Integer, allocatable, dimension(:), intent(inout) :: Stack
  Integer, intent(in) :: NewSize
  Integer, dimension(Size(Stack)) :: Stack_Temp

  Stack_Temp = Stack
  Deallocate(Stack)
  Allocate(Stack(NewSize))
  Stack = Stack_Temp
End Subroutine Stack_Increase_i1


Subroutine Stack_Increase_i2(Stack,NewSizeD1,NewSizeD2)
  !!Increases the size of a stack
  !!Stack is a 2D array of integers
  Integer, allocatable, dimension(:,:), intent(inout) :: Stack
  Integer, intent(in) :: NewSizeD1, NewSizeD2
  Integer, dimension(Size(Stack,1),Size(Stack,2)) :: Stack_Temp

  Stack_Temp = Stack
  Deallocate(Stack)
  Allocate(Stack(NewSizeD1,NewSizeD2))
  Stack = Stack_Temp
End Subroutine Stack_Increase_i2


Subroutine Stack_Increase_r1(Stack,NewSize)
  !!Increases the size of a stack
  !!Stack is a 1D array of reals
  Real(kind=dp), allocatable, dimension(:), intent(inout) :: Stack
  Integer, intent(in) :: NewSize
  Real(kind=dp), dimension(Size(Stack)) :: Stack_Temp

  Stack_Temp = Stack
  Deallocate(Stack)
  Allocate(Stack(NewSize))
  Stack = Stack_Temp
End Subroutine Stack_Increase_r1


Subroutine Stack_Increase_c1(Stack,NewSize)
  !!Increases the size of a stack
  !!Stack is a 1D array of characters
  Character(len=128), allocatable, dimension(:), intent(inout) :: Stack
  Integer, intent(in) :: NewSize
  Character(len=128), dimension(Size(Stack)) :: Stack_Temp

  Stack_Temp = Stack
  Deallocate(Stack)
  Allocate(Stack(NewSize))
  Stack = Stack_Temp
End Subroutine Stack_Increase_c1


Subroutine Sort_Array(Array,IDs)
  !! Sort the arrays in ascending order
  !! Returns the sorted array and the IDs of the sorted array
  Real(kind=dp), dimension(:), intent(inout) :: Array
  Integer, dimension(:), intent(inout) :: IDs
  !! Array is a 1D array of reals
  !! IDs is a 1D array of integers
  Integer :: ii, jj, Temp_ID
  Real(kind=dp) :: Temp_Value

  !! Perform a simple selection sort
  do ii = 1, size(Array) - 1
    do jj = ii + 1, size(Array)
      if (Array(jj) < Array(ii)) then
        !! Swap the values in the 'arr' and 'sorted_ids' arrays
        Temp_Value = Array(ii)
        Array(ii) = Array(jj)
        Array(jj) = Temp_Value

        Temp_ID = IDs(ii)
        IDs(ii) = IDs(jj)
        IDs(jj) = Temp_ID
      end if
    end do
  end do
End Subroutine Sort_Array

End Module
