      program wrte_ctl
! ===============================================
      implicit none
! vars
      integer            ::  nx,  ny
      real*8             ::  gsize

      real*8             ::  west, east, south, north      !! center of lower left pixel
      character*256      ::  buf,  xy,  type, ctype
      real*8             ::  shift
! file
      character*256      ::  binfile, ctlfile
! ===============================================
      call getarg(1,binfile)
      call getarg(2,ctlfile)
      call getarg(3,type)
      call getarg(4,xy)

      call getarg(5,buf)
       read(buf,*) west
      call getarg(6,buf)
       read(buf,*) east
      call getarg(7,buf)
       read(buf,*) south
      call getarg(8,buf)
       read(buf,*) north

      call getarg(9,buf)
      read(buf,*) gsize

      nx=nint( (east -west )/gsize )
      ny=nint( (north-south)/gsize )

      shift=gsize*dble(0.5)

      if( trim(type)=='int4' )then
        ctype=' 1 -1,40,4,-1 '
      elseif( trim(type)=='int2' )then
        ctype=' 1 -1,40,2,-1 '
      elseif( trim(type)=='int1' )then
        ctype=' 1 -1,40,1,-1 '
      elseif( trim(type)=='real' )then
        ctype=' 1 99 '
      elseif( trim(type)=='fldp' )then
        ctype=' 10 99 '
      endif

      open(21,file=ctlfile,form='formatted')
        write(21,'(a6,a)'  )              'dset ^', binfile
        if( trim(type)=='int1' )then
          write(21,'(a)')                   'undef -9'
        else
          write(21,'(a)')                   'undef -9999'
        endif
        write(21,'(a)')                   'title Flow Direction'
        write(21,'(a)')                   'options yrev little_endian'
        write(21,'(a5,i6,a8,f22.16,f22.16)') 'xdef ', nx, ' linear ', west+ shift ,gsize
        write(21,'(a5,i6,a8,f22.16,f22.16)') 'ydef ', ny, ' linear ', south+shift ,gsize
        write(21,'(a)')                   'tdef 1 linear 00Z01jan2000 1yr'

        if( trim(type)=='fldp' )then
          write(21,'(a)')                   'zdef 10 linear 1 1'
          write(21,'(a)')                   'vars 1'
          write(21,'(a6,a,a)')              'var   ', trim(ctype), ' ** '        
        elseif( trim(xy)=='xy' )then
          write(21,'(a)')                   'zdef 1 linear 1 1'
          write(21,'(a)')                   'vars 2'
          write(21,'(a6,a,a)')              'varx  ', trim(ctype), ' ** '        
          write(21,'(a6,a,a)')              'vary  ', trim(ctype), ' ** '        
        else
          write(21,'(a)')                   'zdef 1 linear 1 1'
          write(21,'(a)')                   'vars 1'
          write(21,'(a6,a,a)')              'var   ', trim(ctype), ' ** '        
        endif
        
        write(21,'(a)')                   'ENDVARS'
      close(21)

      end program wrte_ctl

