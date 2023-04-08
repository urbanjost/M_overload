program testit
use M_overload, only : operator(.fmt.)
!use M_intrinsics, only : operator(.fmt.)
character(len=:), allocatable :: string
   !string=1234.fmt.'"[",i0,"]"'
   !write(*,*)string
   ! tests
   !write(*,*)1234.fmt.'"[",i0,"]"' ! .eq. '[1234]' 
   !write(*,*)'1234'.fmt.'"[",g0,"]"' ! .eq. '[1234]' 
end program testit
