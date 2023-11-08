module Timing_Mod
  use StdLib_Mod

  Implicit None

  contains 


  Function Get_Time_CPU() Result(time)
    Real(kind=dp) :: time

    call CPU_time(time)
  End Function Get_Time_CPU

  Function Get_Time_Local() Result(time)
    Integer, dimension(8) :: Date_Time
    Real(kind=dp) :: time
    Character(len=10), dimension(3) :: Char_Output

    !!Day: Date_Time(3) 1-31
    !!Hour: Date_Time(5) 1-23
    !!Minute: Date_Time(6) 1-59
    !!Seconds: Date_Time(7) 0-60
    !!Milliseconds: Date_Time(8) 0-999

    call date_and_time(Char_Output(1),Char_Output(2),Char_Output(3),Date_Time)

    time = Real((Date_Time(3)*24*60*60) &
        +  (Date_Time(5)*60*60) &
        +  (Date_Time(6)*60) &
        +  Date_Time(7),dp) &
        +  Real(Date_Time(8),dp)/1000._dp

  End Function Get_Time_Local

end module
