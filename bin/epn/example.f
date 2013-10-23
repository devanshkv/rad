c==============================================================================
      program example
c==============================================================================
c
c     Sample program to read an EPN data file. 
c      
c==============================================================================
c
      implicit none
c
c     You need this file to define the EPN variables and common blocks...
c      
      include 'epnhdr.inc'
c
c     Local variables
c
      integer i,narg,iargc,lfil,nrec,nepnrec,rstart,rrec
      character uprow*3,option*256
c
c     Handy character to avoid scrolling..
c      
      uprow = char(27)//'[A'
c
c     Start, check for command line arguments. Prompt user if nothing given.
c      
      write(*,*) 'EXAMPLE: A Shell program that does nothing!'
      narg=iargc()
      if (narg.lt.1) then
         write(*,'('' No options were specified!'')')
         write(*,'('' usage: example [filename] -r[startrec]'')')
         write(*,'('' N.B. Input file must be in EPN format!!'')')
	 write(*,'('' -r: set start record number (def=1)'')')
         stop 
      else
         call gtarg(1,filename)
         lfil=index(filename,' ')-1
         nrec=nepnrec(filename)
         if (nrec.eq.0) then 
           write(*,'('' EPN file: '',a,'' not found!'')')
     &     filename(1:lfil)
           stop
         else
  	   write(*,'(a,i5,a)') ' Input file: "'//filename(1:lfil)//
     &     '" contains: ',nrec,' records.'
         endif
      endif
c
c     Initial values for reading the EPN file
c      
      padout=.false.
      readwri=-1
      rstart=0
c
c     Check to see if other arguments have been given...
c      
      if (narg.ge.2) then
         do i=2,narg
  	   call gtarg(i,option)
           if (index(option,'-r').gt.0) then
              read(option(3:),'(i5)') rstart
              rstart=rstart-1
              if (rstart.lt.0) rstart=0
           endif
         enddo
      endif

      write(*,*)
      do recno=rstart,nrec

        rrec=recno
        call rwepn(filename, readwri, rrec, padout)
        write(*,*) uprow,'Read record:',rrec
        
      enddo
      write(*,*) uprow,'Done!' 
      
      end
