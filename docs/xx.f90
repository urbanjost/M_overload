program testit
character(len=256) :: line
      line=repeat(char(0),len(line))
      line=repeat('+',len(line))
      write(line,'(256("+"),t1,a,f0.3,t50,1x,a)')' string ',123.456,' another'
      write(*,*)trim(line)
      write(line,'(256("+"),(t1,a,f0.3,t50,1x),a)')' string ',123.456,' another'
      write(*,*)trim(line)
      write(line,'((256("+"),(t1,a,f0.3,t50,1x),a)("END"),a)')' string ',123.456,''
      write(*,*)trim(line)
      write(line,'((256("+"),(t1,a,f0.3,t50,1x),a)("END"),a)')' string ',123.456,''
      write(*,*)trim(line)
      write(line,'((256("+"),T1,(*(g0,1x)),a)("AAA"),a)')[1,2,3,4],' string ',123.456,''
      write(*,*)trim(line)
end program testit
