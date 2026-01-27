module M_test_suite_M_overload
use, intrinsic :: iso_fortran_env, only : integer_kinds, int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
use M_framework__msg
use M_framework__verify, only : unit_test_level
use M_overload
!use M_overload,              only : operator(==)
!use M_compare_float_numbers, only : operator(.EqualTo.)
!use M_overload,              only : operator(/=)
!use M_overload,              only : operator(+)
private
public test_suite_m_overload
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! BUG REPORTS: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=103782
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_overload()
use M_framework__verify,   only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify,   only : unit_test_level
use M_framework__approx,   only : almost
implicit none
character(len=:),allocatable :: cmd

!! setup
   unit_test_level=5
   if(command_argument_count().eq.0)then
      call test_boolean_equal()
      call test_boolean_notequal()
      call test_dble_s2v()
      call test_dbles_s2v()
      call test_int_s2v()
      call test_ints_s2v()
      call test_real_s2v()
      call test_reals_s2v()
      call test_sign()
      call test_append()
      call test_oz()
      call test_zo()
      call test_fmt()
      call test_to()
!$!      call get_command(cmd,realloc=.true.)
!$!      cmd=cmd//' -x -y "hello there" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
!$!      call execute_command_line(cmd)
!$!   else
!$!      call test_get_command()
!$!      call test_get_command_argument()
   endif
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmt()
   call unit_test_start('fmt','convert intrinsic scalar value to a formatted string')
   ! add 0+ to avoid gfortran-11 bug
   !call unit_test( 'fmt', 1234.fmt.'"[",i0,"]"' == '[1234]' )
   !call unit_test( 'fmt', '1234'.fmt.'"[",i0,"]"' == '[1234]' )
   call unit_test_done('fmt', msg='')
end subroutine test_fmt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_to()
   call unit_test_start('to','generate range of adjacent whole numbers over specified range')
   call unit_test( 'to', all([11.to.14] == [11,12,13,14]  ),'[11,12,13,14]')
   call unit_test( 'to', all([-14.to.-11] == [-14,-13,-12,-11]),'[-14,-13,-12,-11]')
   call unit_test( 'to', all(to(11,14) == [11,12,13,14]  ),'[11,12,13,14]')
   call unit_test( 'to', all(to(-14,-11) == [-14,-13,-12,-11]),'[-14,-13,-12,-11]')
   call unit_test_done('to', msg='')
end subroutine test_to
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_append()
   character(len=:),allocatable :: carr(:)
   character(len=:),allocatable :: expected(:)
   expected=[ character(len=20) :: 'a' , 'list' , 'of' , 'many words' , 'and strings' ]

   call unit_test_start('append','append strings into an array')

   carr='a' + 'list' + 'of' + 'many words' + 'and strings'
   call unit_test( 'append', all(carr == expected  ), 'check +')

   carr='a' .append. 'list' .append. 'of' .append. 'many words' .append. 'and strings'
   call unit_test( 'append', all(carr == expected  ), 'check .append.')

   carr=append('a','list') + append('of','many words') .append. 'and strings'
   call unit_test( 'append', all(carr == expected  ), 'check append')

   call unit_test_done('append', msg='')

end subroutine test_append
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_oz()
   call unit_test_start('oz','convert logical expression to integer with 1 for TRUE')
   ! add 0+ to avoid gfortran-11 bug
   call unit_test('oz',oz(10 > 5).eq.1.and.oz( 10 < 5).eq.0)
   call unit_test('oz',all(oz([10 > 5, 10 < 5, 5 == 5, 5 < 5]) == [1,0,1,0]) , oz(10 > 5)+0,oz(10 < 5)+0,oz(5 == 5)+0,oz(5 < 5)+0 )
   call unit_test_done('oz',msg='')
end subroutine test_oz
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zo()
   call unit_test_start('zo','convert logical expression to integer with 0 for TRUE')
   ! add 0+ to avoid gfortran-11 bug
   call unit_test('zo',zo(10 > 5).eq.0.and.zo( 10 < 5).eq.1)
   call unit_test('zo',all(zo([10 > 5, 10 < 5, 5 == 5, 5 < 5]) == [0,1,0,1]), zo(10 > 5)+0,zo(10 < 5)+0,zo(5 == 5)+0, zo(5 < 5)+0 )
   call unit_test_done('zo',msg='')
end subroutine test_zo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sign()
   call unit_test_start('sign','overload SIGN to take a single argument')
   ! add 0+ to avoid gfortran-11 bug
   call unit_test('sign',sign(10_int8).eq.1.and.sign(-10_int8).eq.-1,'sign(+-10_int8)',0+sign(10_int8),0+sign(-10_int8))
   call unit_test('sign',sign(10_int16).eq.1.and.sign(-10_int16).eq.-1,'sign(+-10_int16)',0+sign(10_int16),0+sign(-10_int16))
   call unit_test('sign',sign(10_int32).eq.1.and.sign(-10_int32).eq.-1,'sign(+-10_int32)',0+sign(10_int32),0+sign(-10_int32))
   call unit_test('sign',sign(10_int64).eq.1.and.sign(-10_int64).eq.-1,'sign(+-10_int64)',0+sign(10_int64),0+sign(-10_int64))
   call unit_test('sign',sign(10.0_real32).eq.1.and.sign(-10.0_real32).eq.-1,&
   & 'sign(+-10_real32)',0+sign(10.0_real32),0+sign(-10.0_real32))
   call unit_test('sign',0+sign(10.0_real64).eq.1.and.sign(-10.0_real64).eq.-1,&
   & 'sign(+-10_real64)',0+sign(10.0_real64),0+sign(-10.0_real64))
   call unit_test('sign',0+sign(10.0_real128).eq.1.and.0+sign(-10.0_real128).eq.-1,&
   & 'sign(+-10_real128)',0+sign(10.0_real128),0+sign(-10.0_real128))
   call unit_test_done('sign',msg='')
end subroutine test_sign
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_equal()
   call unit_test_start('boolean_equal','overload == to take LOGICAL arguments')
   call unit_test('boolean_equal',.true. == .true. ,'== works like .eqv. for LOGICAL values')
   call unit_test_done('boolean_equal',msg='')
end subroutine test_boolean_equal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_notequal()
   call unit_test_start('boolean_notequal','overload /= to take LOGICAL arguments')
   call unit_test('boolean_notequal', (.true. /= .false. ),'/= works like .neqv. for LOGICAL values')
   call unit_test_done('boolean_notequal',msg='')
end subroutine test_boolean_notequal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble_s2v()
!use M_overload,              only : int, real, dble
   call unit_test_start('dble_s2v','overload dble() to take string arguments')
   if(dble('0.3570726221234567').eq. 0.3570726221234567d0)then
      call unit_test_good('dble_s2v')                                             ! string passed to dble
!   elseif(dble('0.3570726221234567') .EqualTo. 0.3570726221234567d0 )then
   elseif(abs(dble('0.3570726221234567') - 0.3570726221234567d0 ).lt.2*epsilon(0.0d0))then
      call unit_test_good('dble_s2v')                                             ! string passed to real but not exactly
   else
      call unit_test_bad('dble_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_dble_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dbles_s2v()
doubleprecision,allocatable :: vals(:)
   call unit_test_start('dbles_s2v','overload dble() to take string arguments')

   vals=dble(['10.0d0','20.0d0'])
   vals=vals-[10.0d0,20.0d0]
   
   if(all(vals.eq.0.0d0))then
      call unit_test_good('dbles_s2v')                                             ! string passed to dble
   elseif(all(vals.lt.2*epsilon(0.0d0)))then
      call unit_test_good('dbles_s2v')                                             ! string passed to real but not exactly
   else
      call unit_test_bad('dbles_s2v')                                              ! returned value not equal to expected value
   endif
!!   ENCOUNTERS GFORTRAN 10.3.0 BUG
!!   if(all(abs(dble(['10.0d0','20.0d0']).eq.[10.0d0,20.0d0])))then
!!      call unit_test_good('dbles_s2v')                                             ! string passed to dble
!!!   elseif(all(dble(['10.0d0','20.0d0']) .EqualTo. [10.0d0,20.0d0]))then
!!   elseif(all(abs(dble(['10.0d0','20.0d0']) - [10.0d0,20.0d0]).lt.2*epsilon(0.0d0)))then
!!      call unit_test_good('dbles_s2v')                                             ! string passed to real but not exactly
!!   else
!!      call unit_test_bad('dbles_s2v')                                              ! returned value not equal to expected value
!!   endif
end subroutine test_dbles_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_s2v()
   call unit_test_start('int_s2v','overload INT() to take string arguments')
   call unit_test('int_s2v',int('1234').eq.1234,'string passed to int')
   call unit_test_done('int_s2v',msg='')
end subroutine test_int_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ints_s2v()
integer,allocatable :: ibug(:)
   call unit_test_start('ints_s2v','overload INT() to take string arguments')
   !!if(all(int(['100','200']).eq. [100,200]))then
   ibug=int(['100','200'])
   if(all(ibug.eq. [100,200]))then
      call unit_test_good('ints_s2v')                                             ! string passed to int
   else
      call unit_test_bad('ints_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_ints_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_s2v()
   call unit_test_start('real_s2v','overload REAL() to take string arguments')
   if(REAL('0.357072622').eq. 0.357072622)then
      call unit_test_good('real_s2v')
   elseif(almost(real('0.357072622'), 0.357072622,7.0) )then
      call unit_test_good('real_s2v')                                          ! string passed to real but not exactly
   else
      call unit_test_bad('real_s2v')                                           ! returned value not equal to expected value
   endif
end subroutine test_real_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reals_s2v()
real,allocatable :: rbug(:)
   call unit_test_start('reals_s2v','overload REAL() to take string arguments')
   rbug=real(['0.357072622','200.0      '])
   if(all(rbug.eq. [0.357072622,200.0]))then
      call unit_test_good('reals_s2v')                                             ! string passed to int
!   elseif(all(real(['0.357072622','200.0      ']) .EqualTo. [0.357072622,200.0]))then
   elseif(all(abs(real(['0.357072622','200.0      ']) - [0.357072622,200.0]).lt.2*epsilon(0.0d0)))then
      call unit_test_good('reals_s2v')                                             ! string passed to real but not exactly
   else
      call unit_test_bad('reals_s2v')                                              ! returned value not equal to expected value
   endif

end subroutine test_reals_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!$!subroutine test_get_command()
!$!   call unit_test_start('get_command','&
!$!         & -description "overload GET_COMMAND() to take allocatable string" &
!$!         & -section 3                    &
!$!         & -library libGPF               &
!$!         & -filename `pwd`/M_overload.FF &
!$!         & -documentation y              &
!$!         &  -prep         y              &
!$!         &  -ccall        n              &
!$!         &  -archive      GPF.a          &
!$!         & ')
!$!   call unit_test_done('get_command',msg='')
!$!end subroutine test_get_command
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!$!subroutine test_get_command_argument()
!$!integer                      :: istat, ilen, i
!$!character(len=:),allocatable :: argument, arguments
!$!character(len=10)            :: regular
!$!integer,allocatable          :: istats(:), ilens(:)
!$!! assuming called with cmd//' -x -y "hello there" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
!$!   call unit_test_start('get_command_argument','&
!$!    & -description "overload GET_command_argument() to take allocatable string" &
!$!    & -section 3                    &
!$!    & -library libGPF               &
!$!    & -filename `pwd`/M_overload.FF &
!$!    & -documentation y              &
!$!    &  -prep         y              &
!$!    &  -ccall        n              &
!$!    &  -archive      GPF.a          &
!$!    & ')
!$!   
!$!   ! unallocated allocatable variable and realloc=.true.
!$!   allocate(integer :: istats(0),ilens(0))
!$!   arguments=''
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, argument, length=ilen, status=istat, realloc=.true.)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//argument//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_test('get_command_argument',all(istats.eq.0),'check if all status returns are zero')
!$!   call unit_test('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_test('get_command_argument',arguments.eq.'-x -y hello there xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
!$!
!$!   ! allocated allocatable variable and realloc=.true.
!$!   if(allocated(istats))deallocate(istats)
!$!   if(allocated(ilens))deallocate(ilens)
!$!   if(allocated(argument))deallocate(argument)
!$!   allocate(integer :: istats(0),ilens(0))
!$!   argument='12345678901234'
!$!   arguments=''
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, argument, length=ilen, status=istat, realloc=.true.)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//argument//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_test('get_command_argument',all(istats.eq.0),'check if all status returns are zero')
!$!   call unit_test('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_test('get_command_argument',arguments.eq.'-x -y hello there xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
!$!
!$!   ! allocatable variable already allocated and no realloc=.true.
!$!   deallocate(istats,ilens,argument)
!$!   allocate(integer :: istats(0),ilens(0))
!$!   arguments=''
!$!   allocate(character(len=10) :: argument)
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, argument, length=ilen, status=istat)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//argument//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_test('get_command_argument',all(istats.eq.[0,0,-1,-1]),'check if get truncations as expected')
!$!   call unit_test('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_test('get_command_argument',arguments.eq.'-x         -y         hello ther xxxxxxxxxx')
!$!
!$!   ! regular
!$!   deallocate(istats,ilens)
!$!   allocate(integer :: istats(0),ilens(0))
!$!   arguments=''
!$!   do i=1, command_argument_count()
!$!      call get_command_argument(i, regular, length=ilen, status=istat)
!$!      istats=[istats,istat]
!$!      ilens=[ilens,ilen]
!$!      arguments=arguments//regular//' '
!$!   enddo
!$!   write(*,*)'ISTATS=',istats
!$!   write(*,*)'ILENS=',ilens
!$!   write(*,*)'ARGUMENTS=',arguments
!$!   call unit_test('get_command_argument',all(istats.eq.[0,0,-1,-1]),'check if get truncations as expected')
!$!   call unit_test('get_command_argument',all(ilens.eq.[2,2,11,36]),'check lengths')
!$!   call unit_test('get_command_argument',arguments.eq.'-x         -y         hello ther xxxxxxxxxx')
!$!
!$!   call unit_test_done('get_command_argument',msg='')
!$!end subroutine test_get_command_argument
!===================================================================================================================================
end subroutine test_suite_M_overload
end module M_test_suite_M_overload

program runtest
use M_framework__msg
use M_framework__verify, only : unit_test_level, unit_test_stop
use M_test_suite_M_overload
implicit none
   unit_test_level=0
   call test_suite_M_overload()
   call unit_test_stop('')
end program runtest
