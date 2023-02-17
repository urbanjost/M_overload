program main
   ! rename intrinsics

   ! shorter names
   use M_overload, only : cmd=>execute_command_line
   use M_overload, only : getenv=> get_environment_variable
   !use M_overload, only : elements=> size

   ! longer names
   use M_overload, only : cosine=>cos, sine=>sin, tangent=>tan

   implicit none
   character(len=4096) :: home

   call cmd('echo Hello')
   call getenv('HOME',home)
   write(*,*)trim(home)
   !write(*,*)elements([10,20,30])

   write(*,*)cosine(1.0),sine(1.0),tangent(1.0)

end program main
