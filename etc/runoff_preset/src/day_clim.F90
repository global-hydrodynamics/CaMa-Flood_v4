      program calc_rivout
! ================================================
      implicit none
! ==============================
! input files
      character*256       ::  buf
      character*256       ::  diminfo                 !! dimention info file
      character*256       ::  roffdir                 !! runoff directory
      character*256       ::  roffpre                 !! runoff prefic
      character*256       ::  roffsuf                 !! runoff suffix
      character*256       ::  cclmdir                 !! climatology dir
      character*256       ::  crunoff
      character*256       ::  crofclm                 !! runoff climatology

      data                    diminfo /'./map/diminfo-1deg.txt'/  !! sample, fixed by arguments
      data                    roffdir /'./runoff_ori/'/           
      data                    roffpre /'Roff____'/  
      data                    roffsuf /'.one'/  

! dimention
      integer             ::  nxin, nyin              !! input map dimention

      integer             ::  syear, eyear
      integer             ::  iyear, imon, iday, nday
      integer             ::  idate
      character*16        ::  cdate, cmmdd

! input runoff
      real,allocatable    ::  roffin(:,:)             !! runof (input) [mm/day]
      real,allocatable    ::  roffave(:,:)             !! runof (input) [mm/day]
      real                ::  nyear
! function
      integer             ::  igetday
! ================================================
! read parameters from arguments
      call getarg(1,diminfo)
      call getarg(2,roffdir)
      call getarg(3,roffpre)
      call getarg(4,roffsuf)
      call getarg(5,cclmdir)

      call getarg(6,buf)
       read(buf,*) syear
      call getarg(7,buf)
       read(buf,*) eyear
      call getarg(8,buf)
       read(buf,*) imon

      print *, trim(diminfo)
      open(11,file=diminfo,form='formatted')
      read(11,*) !!nx
      read(11,*) !!ny
      read(11,*) !!nflp
      read(11,*) nxin
      read(11,*) nyin
      read(11,*) !!inpn
      read(11,'(a)') !!cinpmat
      read(11,*) !!west
      read(11,*) !!east
      read(11,*) !!north
      read(11,*) !!south
      close(11)

      allocate(roffin(nxin,nyin),roffave(nxin,nyin))

      nday=igetday(2001,imon)  !! negrect 29Feb
      do iday=1, nday
        roffave(:,:)=0
        nyear=0
        do iyear=syear, eyear
          idate=iyear*10000 + imon*100 + iday
          write(cdate,'(i8.8)') idate
          write(cmmdd,'(i4.4)') imon*100+iday

          crunoff=trim(roffdir)//'/'//trim(roffpre)//trim(cdate)//trim(roffsuf)
          print *, trim(crunoff)
          open(21,file=crunoff,form='unformatted',access='direct',recl=4*nxin*nyin)
          read(21,rec=1) roffin
          close(21)
          roffave(:,:)=roffave(:,:)+roffin(:,:)
          nyear=nyear+1
        end do
        roffave(:,:)=roffave(:,:)/nyear

        crofclm=trim(cclmdir)//'/roff_dayclm_'//trim(cmmdd)//trim(roffsuf)
        print *, "output:", trim(crofclm)
        open(31,file=crofclm,form='unformatted',access='direct',recl=4*nxin*nyin)
        write(31,rec=1) roffave
        close(31)
      end do



      end program calc_rivout


      integer function igetday(iyear,imon)
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
! ================================================
! Calculation for months except February
! ================================================
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
!
      return
      end function igetday

