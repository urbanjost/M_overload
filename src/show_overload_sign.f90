program demo_show_overload_sign

use, intrinsic :: iso_fortran_env, only : integer_kinds, int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128

! overload can take a single argument
use M_overload, only : sign
implicit none

   write(*,*) merge('sign works','sign fails', sign(10_int8).eq.1        .and. sign(-10_int8).eq.-1        )
   write(*,*) merge('sign works','sign fails', sign(10_int16).eq.1       .and. sign(-10_int16).eq.-1       )
   write(*,*) merge('sign works','sign fails', sign(10_int32).eq.1       .and. sign(-10_int32).eq.-1       )
   write(*,*) merge('sign works','sign fails', sign(10_int64).eq.1       .and. sign(-10_int64).eq.-1       )
   write(*,*) merge('sign works','sign fails', sign(10.0_real32).eq.1.0  .and. sign(-10.0_real32).eq.-1.0  )
   write(*,*) merge('sign works','sign fails', sign(10.0_real64).eq.1.0  .and. sign(-10.0_real64).eq.-1.0  )
   write(*,*) merge('sign works','sign fails', sign(10.0_real128).eq.1.0 .and. sign(-10.0_real128).eq.-1.0 )

end program demo_show_overload_sign
