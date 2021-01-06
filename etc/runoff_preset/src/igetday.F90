      program getday
! ================================================
! to count number of days in month
! by nhanasaki
! on 20th Dec 2003
! at IIS,UT
! ================================================
      implicit none
! in
      integer             ::  iyear
      integer             ::  imon
      integer             ::  igetday

      character*16        ::  buf
! ================================================
! Calculation for months except February
! ================================================
      call getarg(1,buf)
       read(buf,*) iyear
      call getarg(2,buf)
       read(buf,*) imon

      if(imon.eq.4.or.imon.eq.6.or.imon.eq.9.or.imon.eq.11)then
        igetday=30
      else
        igetday=31
      end if
! ================================================
! Calculation for February
! ================================================
      if(imon.eq.2)then
        if(mod(iyear,4).eq.0)then
          igetday=29
          if(mod(iyear,100).eq.0)then
            igetday=28
            if(mod(iyear,400).eq.0)then
              igetday=29
            end if
          end if
        else
          igetday=28
        end if
      end if
! fix
      if(iyear.eq.0.and.imon.eq.2)then
        igetday=28
      end if
      print *, igetday
!
      end program getday
