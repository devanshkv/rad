c==============================================================================
      program epn2ascii
c==============================================================================
c
c     Sample program to print the bins from an EPN file
c
      implicit none 

      include 'epnhdr.inc'
      real prmax
      integer narg, lfil, iargc, nepnrec, maxrecs, i, j
      character*1 num*80
      logical first
      data first/.true./

      readwri=-1
      narg=iargc()
      if (narg.lt.1) then
         write(*,'(''No options were specified!'')')
         write(*,'(''usage: epn2ascii [filename] [num]'')')
         write(*,'(''You must specify the EPN filename!'')')
         write(*,'(''num: is the required record to display'')')
         stop
      else
         call getarg(1,filename)
         lfil=index(filename,' ')-1
      endif

      maxrecs=nepnrec(filename)

      if (maxrecs.le.0) stop 'File does not exist!'

      recno=1
      if (narg.eq.2) then
         call getarg(2,num)
         read(num,*) recno
         if (recno.gt.maxrecs) then
            write(*,'(''Requested record too large - using: '',i4)')
     &      maxrecs
            recno=maxrecs
         endif
      endif

      call rwepn(filename,readwri,recno,padout)

      open(unit=10,file=filename(1:lfil)//'.asc',status='unknown')
      if (npol.eq.1) write(10,'(a)') 'Total Power only'
      if (npol.eq.4) write(10,'(a)') 'Stokes I Q U V'
      write(10,'(a)') ' Time (s)               Data...'
      prmax=-1.0e32
      do i=1,nbin
           prmax=max(prmax,rawdata(1,i))
      enddo
      prmax=1.0
      do i=1,nbin
         if (npol.eq.1)
     &    write(10,*) float(i)*float(tbin)/1.0e6,rawdata(1,i)/prmax
         if (npol.eq.4)
     &    write(10,*) float(i)*float(tbin)/1.0e6,rawdata(1,i)/prmax,
     &                rawdata(2,i)/prmax,rawdata(3,i)/prmax,
     &                rawdata(4,i)/prmax
      enddo
      close(unit=10)
      write(*,'(a)') filename(1:lfil)//'.asc'
      end


