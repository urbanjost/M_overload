module M_extend ! self-contained example program
   use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
   use, intrinsic :: iso_fortran_env, only : real32, real64, real128
   implicit none
   private
   public dble                      ! extend intrinsics to accept CHARACTER values and LOGICALS
   interface dble
      module procedure anyscalar_to_double
   end interface
contains

   pure elemental function anyscalar_to_double(valuein) result(d_out)
      use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
      implicit none

!$@(#) M_anything::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision

      class(*),intent(in)       :: valuein
      doubleprecision           :: d_out
      doubleprecision,parameter :: big=huge(0.0d0)
      character(len=3)          :: nanstring
      select type(valuein)
       type is (integer(kind=int8));   d_out=real(valuein,kind=real64)
       type is (integer(kind=int16));  d_out=real(valuein,kind=real64)
       type is (integer(kind=int32));  d_out=real(valuein,kind=real64)
       type is (integer(kind=int64));  d_out=real(valuein,kind=real64)
       type is (real(kind=real32));    d_out=real(valuein,kind=real64)
       type is (real(kind=real64));    d_out=real(valuein,kind=real64)
       type is (real(kind=real128))
         if(valuein.gt.big)then
            !!write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
            nanstring='NaN'
            read(nanstring,*) d_out
         else
            d_out=real(valuein,kind=real64)
         endif
       type is (complex(kind=real32)); d_out=abs(valuein)
       type is (complex(kind=real64)); d_out=abs(valuein)
       type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
       type is (character(len=*));     read(valuein,*) d_out
       class default
         !!stop '*M_anything::anyscalar_to_double: unknown type'
         nanstring='NaN'
         read(nanstring,*) d_out
      end select
   end function anyscalar_to_double

end module M_extend

program testit
   use M_extend
   implicit none
   ! make sure normal stuff still works
   write(*,*)'##CONVENTIONAL'
   write(*,*)'INTEGER         ', dble(10)
   write(*,*)'INTEGER ARRAY   ', dble([10,20])
   write(*,*)'REAL            ', dble(10.20)
   write(*,*)'DOUBLEPRECISION ', dble(100.20d0)
   ! extensions
   write(*,*)'##EXTENSIONS'
   write(*,*)'CHARACTER       ', dble('100.30')
   write(*,*)'CHARACTER ARRAY ', dble([character(len=10) :: '100.30','400.500'])
   ! call a function with a metamorphic argument
   write(*,*)'METAMORPHIC     ', promote(1,1.0,1.0d0)
   write(*,*)'METAMORPHIC     ', promote('3',(2.0,0.0),.true.)
   write(*,*)'METAMORPHIC     ', promote('3','3','3')
   write(*,*)'METAMORPHIC     ', promote(.true.,.false.,.true.)
   write(*,*)'METAMORPHIC     ', promote((3.0,4.0),0.0,0)
   ! settle this once and for all
   write(*,*)'LOGICAL TRUE    ', dble(.true.)
   write(*,*)'LOGICAL FALSE   ', dble(.false.)
   write(*,*)'LOGICAL ARRAY   ', dble([.false., .true., .false., .true.])
contains
   function promote(value1,value2,value3)
      class(*),intent(in) :: value1
      class(*),intent(in) :: value2
      class(*),intent(in) :: value3
      doubleprecision,allocatable :: promote
      promote=sum([dble(value1),dble(value2),dble(value3)])
   end function promote

end program testit
