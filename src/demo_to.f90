     program demo_to
     use M_overload, only: to, operator(.to.)
     implicit none
        print*,[11.to.16]
        print*,2.5 * [1.to.4]
        print*,2.5 * to(1,4)+10
     end program demo_to
