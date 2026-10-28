      program wrte_ctl
! ===============================================
      implicit none
      character*256      ::  params
      parameter             (params='../params.txt')

      integer            ::  nXX, nYY                      !! river netwrok domain (nXX,nYY)
      real               ::  gsize                         !! grid size [degree]
      real               ::  west, east, north, south

! ===============================================
      open(11,file=params,form='formatted')
      read(11,*) nXX
      read(11,*) nYY
      read(11,*) 
      read(11,*) gsize
      read(11,*) west
      read(11,*) east
      read(11,*) south
      read(11,*) north
      close(11)

      write(*,'(4f12.3,f20.15)') west, east, south, north, gsize

      end program wrte_ctl
