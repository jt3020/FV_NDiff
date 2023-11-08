module Progress_Mod

  Implicit None

  contains 


  Subroutine Progress_Start(Description)
    character(len=128), intent(in) :: Description
    integer :: bar_length
    character(128) :: bar

    bar_length = 24 + len(trim(Description))
    bar =  '- '//trim(Description)//': [' //  repeat(' ', bar_length ) // '] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
  End Subroutine Progress_Start

  Subroutine Progress_Start_Numeric(Description, total)
    character(len=128), intent(in) :: Description
    integer, intent(in) :: total
    integer :: bar_length
    character(128) :: bar
    character(32) :: prog_str, tot_str

    write(prog_str, '(i0)') 0
    write(tot_str, '(i0)') total

    bar_length = 24 + len(trim(Description))
    bar = '- '//trim(Description)//': [ Progress: '// trim(prog_str) // '/'// trim(tot_str) //' ] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
  End Subroutine Progress_Start_Numeric

  Subroutine Progress_Update(Description, progress, total)
    character(len=128), intent(in) :: Description
    integer, intent(in) :: progress, total
    integer :: bar_length, n_bars
    character(128) :: bar

    bar_length = 24 + len(trim(Description))
    n_bars = (real(progress) / real(total)) * real(bar_length)
    bar = '- '//trim(Description)//': [' // repeat('#', n_bars) // repeat(' ', bar_length - n_bars) // '] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
  End Subroutine Progress_Update

  Subroutine Progress_Update_Custom(Description,custom_char,  progress, total)
    character(len=128), intent(in) :: Description
    character(1), intent(in) :: custom_char
    integer, intent(in) :: progress, total
    integer :: bar_length, n_bars
    character(128) :: bar

    bar_length = 24 + len(trim(Description))
    n_bars = (real(progress) / real(total)) * real(bar_length)
    bar = '- '//trim(Description)//': [' // repeat(custom_char, n_bars) // repeat(' ', bar_length - n_bars) // '] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
  End Subroutine Progress_Update_Custom

  Subroutine Progress_Update_Numeric(Description, progress, total)
    character(len=128), intent(in) :: Description
    integer, intent(in) :: progress, total
    integer :: bar_length, n_bars
    character(128) :: bar
    character(32) :: prog_str, tot_str

    write(prog_str, '(i0)') progress
    write(tot_str, '(i0)') total

    bar_length = 24 + len(trim(Description))
    n_bars = (real(progress) / real(total)) * real(bar_length)
    bar = '- '//trim(Description)//': [ Progress: '// trim(prog_str) // '/'// trim(tot_str) //' ] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
  End Subroutine Progress_Update_Numeric

  Subroutine Progress_End(Description)
    character(len=128), intent(in) :: Description
    integer :: bar_length
    character(128) :: bar

    bar_length = 24 + len(trim(Description))
    bar = '- '//trim(Description)//': [' //  repeat('#', bar_length ) // '] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
    Write(*,*)
  End Subroutine Progress_End

  Subroutine Progress_End_Custom(Description,custom_char)
    character(len=128), intent(in) :: Description
    character(1), intent(in) :: custom_char
    integer :: bar_length
    character(128) :: bar

    bar_length = 24 + len(trim(Description))
    bar = '- '//trim(Description)//': [' //  repeat(custom_char, bar_length ) // '] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
    Write(*,*)
  End Subroutine Progress_End_Custom

  Subroutine Progress_End_Numeric(Description, total)
    character(len=128), intent(in) :: Description
    integer, intent(in) :: total
    integer :: bar_length
    character(128) :: bar
    character(32) :: tot_str

    write(tot_str, '(i0)') total

    bar_length = 24 + len(trim(Description))
    bar = '- '//trim(Description)//': [ Progress: '// trim(tot_str) // '/'// trim(tot_str) //' ] -'
    Write(*,'(2g0)',advance="no") achar(13), trim(bar)
    Write(*,*)
  End Subroutine Progress_End_Numeric

end module
