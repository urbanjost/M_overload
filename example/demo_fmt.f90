        program demo_fmt
        use :: M_overload, only : f=>fmt
        use :: M_overload, only : operator(.fmt.)
        use :: M_overload, only : operator( // )
        implicit none
        character(len=:), allocatable :: output
        character(len=:), allocatable :: string
        ! some formats
        character(len=*), parameter   :: bracket='"[",g0.5,"]"'
        character(len=*), parameter   :: five='g0.5'
        character(len=*), parameter   :: g0='g0'
        ! for statements
        character(len=*), parameter   :: gen='(*(g0:,1x))'
        character(len=*), parameter   :: asis='(*(g0:))'
        character(len=*), parameter   :: comma='(*(g0:,","))'
        real :: x
        integer :: i
        real,allocatable :: arr(:)

          ! print will do, just want to control format of a number
           print gen,'result is',10,'and',f(10.0/3.0,'f0.3')
          ! complex formats can be in nicely named strings, of course
           write(*,*)'result is ', f(10.0/3.0,bracket)
          ! you can build a string without using an internal write
           output=f(.true.,"'The final answer is [',g0,']'")
           write(*,*)'the string is now:',output
           x=1234.5680088
           i=543
           ! operator style
           string=1234.4567 .fmt.'g0.0'
           write(*,*)string
           ! function style
           string=f(1234.4567, 'g0.0')
           write(*,*)string
           ! concatenation style
           string=10//' is an integer and '//(11.00,22.00)//' is a complex'
           write(*,*)string
           ! list-directed I/O leaves column 1 blank and often prints more
           ! digits than warranted for the precision of a value.
           !
           ! combined with a g0 format line "asis" you get something very
           ! similar to list-directed I/O accept no unexpected line breaks
           ! and starts in column 1 and lets you tweek float values and
           ! not have to remember a space goes between values unless they
           ! are adjacent strings, and you may or may not get while space
           ! after numeric values to make them all the same length for a
           ! particular type for nice tables or compact for use in composed text
           !
           ! this is nearly as simple as list-directed but more predictable:
           print asis,'The value is ',f(x,'f0.3'),' or there-abouts'

           ! and combine multiple formats for use in a single line without
           ! using non-advancing I/O
           write(*,comma)x,f(x),f(x,g0),f(x,five),f(x,bracket)
           !
           !not yet! write(*,asis)'['//f(arr,comma)//']'

        end program demo_fmt
