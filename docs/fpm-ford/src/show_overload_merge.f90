program show_overload_merge

use, intrinsic :: iso_fortran_env, only : integer_kinds, int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128

! allow strings of different length on merge
use M_overload, only : merge
implicit none
character(len=*), parameter :: gen='(*("[",g0,"]":,","))'

   write(*,gen)merge('a','bbbbb',1.eq.1)
   write(*,gen)merge('a','bbbbb',1.eq.2)
   write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)

end program show_overload_merge
