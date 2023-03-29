program demo_show_string2number

use, intrinsic :: iso_fortran_env, only : integer_kinds, int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128

use M_overload, only : int       ! allow strings to be converted to integers
use M_overload, only : real,dble ! allow strings to be converted to floating point

use M_overload, only : merge
implicit none
character(len=*), parameter :: gen='(*("[",g0,"]":,","))'

   if(int('1234')                                          .eq. 1234          ) write(*,*)'int("STRING") works '
   if(abs(real('1234.56789') - 1234.56789)                 .lt. 2*epsilon(0.0)) write(*,*)'real("STRING") works '
   if(abs(dble('1234.5678901234567')- 1234.5678901234567d0).lt. epsilon(0.0d0)) write(*,*)'dble("STRING") works '

   write(*,*) merge('int works for .FALSE.','int fails for .FALSE.',int(.FALSE.).ne.0)
   write(*,*) merge('int works for .TRUE.','int fails for .TRUE.',int(.TRUE.).eq.0)

end program demo_show_string2number
