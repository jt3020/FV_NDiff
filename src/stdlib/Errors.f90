module Errors_Mod

  Implicit None

  contains 


  Subroutine Stop_Error(Message)
    !!Critical Error - Displays associated Message and stop code
    Character(len=*), intent(in) :: Message

    Write(*,*) "####################"
    Write(*,*) "     STOP ERROR     "
    Write(*,*) Message
    Write(*,*) "####################"
    Stop
  End Subroutine Stop_Error


  Subroutine Warning_Error(Message)
    !!Possible Error - Displays associated Message
    Character(len=*), intent(in) :: Message

    Write(*,*) "--------------------"
    Write(*,*) "    WARNING ERROR   "
    Write(*,*) Message
    Write(*,*) "--------------------"
  End Subroutine Warning_Error

end module
