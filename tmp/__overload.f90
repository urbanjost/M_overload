program overload
use M_verify,                 only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_math,                  only : almost
use m_compare_float_numbers, only : operator(.EqualTo.)

use M_overload,              only : int, real, dble
use M_overload,              only : operator(==)
use M_overload,              only : operator(/=)
implicit none
!===================================================================================================================================
   ! Use goodbad(1) to indicate the test sequence was begun
   call unit_check_start('int',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('int',int('1234').eq.1234,'string passed to int')
   call unit_check_good('int','-section 3')
!===================================================================================================================================
   call unit_check_start('real',' &
         & -description "overload REAL() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(real('0.357072622').eq. 0.357072622)then
      call unit_check_good('real')
   elseif(almost(real('0.357072622'), 0.357072622,7.0) )then
      call unit_check_good('real')                                          ! string passed to real but not exactly
   else
      call unit_check_bad('real')                                           ! returned value not equal to expected value
   endif
   call unit_check_good('real','-section 3 ')
!===================================================================================================================================
   call unit_check_start('dble',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(dble('0.3570726221234567').eq. 0.3570726221234567d0)then
      call unit_check_good('dble')                                             ! string passed to dble
   elseif(dble('0.3570726221234567') .EqualTo. 0.3570726221234567d0 )then
      call unit_check_good('dble')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dble')                                              ! returned value not equal to expected value
   endif
   call unit_check_good('dble','-section 3 ')
!===================================================================================================================================
   call unit_check_start('boolean_equal',' &
         & -description "overload == to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_equal',.true. == .true. ,'passing LOGICAL1 == LOGICAL2')
   call unit_check_good('boolean_equal','-section 3 ')
!===================================================================================================================================
   call unit_check_start('boolean_notequal',' &
         & -description "overload /= to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -ufpp         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_notequal',.true. /= .false. ,'passing LOGICAL1 /= LOGICAL2')
   call unit_check_good('boolean_notequal','-section 3 ')
!===================================================================================================================================
!===================================================================================================================================
end program overload
