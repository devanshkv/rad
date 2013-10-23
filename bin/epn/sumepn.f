c==============================================================================
      program sumepn
c==============================================================================
c
c     program to combine EPN formatted files by summing each channel
c
c   - still to be done: determine number of pulses, integration time and
c                       bandwidth of sum
c
      implicit none 

      include 'epnhdr.inc'

      integer narg, nfiles, i, j, nrecs, irec, iblk, iadd, sumnbin,
     &     sumnfreq, sumnpol,iargc, nepnrec, sumnint, rebin, nbinnew
      real sumdata(maxblk,maxbin), sumf0(maxblk), work(maxbin)
      
      real*8 sumtbin
      
      character outfile*80, options*80, sumid*8(maxblk)

      logical first
      data first/.true./


      readwri=-1
      narg=iargc()
      nfiles=0
      rebin=0
      
      if (narg.gt.0) then
         i=0
 10      continue
            i=i+1
            call getarg(i,options)
            if (options(1:1).ne.'-') then
               nfiles=nfiles+1
               if (i.lt.narg) goto 10
            endif
 20      continue
         write(*,*) nfiles-1,' files to sum...'
      endif

      if (nfiles.lt.3) then
         write(*,'('' SUMEPN : A program to sum EPN files'')')
         write(*,'('' Not enough files were specified!'')')
         write(*,'('' usage: sumepn <file1> <file2>... <outfile>'')')
         write(*,'('' Options at end: [-r<rebib>]'')')
         stop
      endif
      
      call getarg(nfiles,outfile)

      do i=nfiles+1, narg
         call getarg(i,options)
         if (options(1:2).eq.'-r') then
            read(options(3:),*) rebin
            write(*,*) 'Rebinning by ', rebin
         endif
      enddo

      write(*,*) 'Initializing...'

      do i=1, maxblk
         do j=1, maxbin
            sumdata(i,j)=0.0
         enddo
      enddo

      iadd=0
      do i=1, nfiles-1 ! loop through the input files
         call getarg(i,filename)
         write(*,*) 'Loading: ',filename(1:40)
         nrecs=nepnrec(filename)
         
         if (nrecs.eq.0) stop 'File does not exist!'
         if (nrecs.eq.-1) stop 'Error reading file!'

         do irec=1, nrecs ! loop through record
            write(*,*) 'Record: ',irec
            call rwepn(filename,readwri,irec,padout)
            write(*,*) 'Number of pol:',npol
            write(*,*) 'Number of sub-bands per pol:',nfreq
            write(*,*) 'Number of pulses: ',nint
             if (first) then
               sumnbin=nbin
               sumtbin=tbin
               sumnpol=npol
               sumnfreq=nfreq
               sumnint=nint
            else
               sumnint=sumnint+nint
               if (sumnbin.ne.nbin) stop
     $              'nbin inconsistent'
c               if (sumtbin.ne.tbin) stop
c     $              'tbin inconsistent'
               if (sumnpol.ne.npol) stop
     $              'npol inconsistent'
               if (sumnfreq.ne.nfreq) stop
     $              'nfreq inconsistent'
            endif
            do iblk=1,npol*nfreq
               if (first) then
                  sumid(iblk)=idfield(iblk)
                  sumf0(iblk)=f0(iblk)
               else
                  if (sumid(iblk).ne.idfield(iblk)) stop
     $                 'IDs inconsistent'
                  if (abs(sumf0(iblk) - f0(iblk)) .gt. 1. ) then
                     write(*,*) sumf0(iblk), f0(iblk)
                     stop 'Frequencies inconsistent'
                  endif
               endif
               do j=1, nbin
                  sumdata(iblk,j)=sumdata(iblk,j)+rawdata(iblk,j)
               enddo
            enddo
            first=.false.
            iadd=iadd+1
         enddo
      enddo

      filename=outfile
      write(*,*) 'Writing:', filename(1:40)
      write(*,*)

      nint=sumnint
      do i=1, npol*nfreq
         do j=1, nbin
            work(j)=sumdata(i,j)/float(iadd)
         enddo
         nbinnew=nbin
         if (rebin.gt.0) call rebinit(rebin,work,nbin,nbinnew)
c         write(*,*) nbin, nbinnew, tbin, tres
         do j=1, nbinnew
            rawdata(i,j)=work(j)
         enddo
      enddo
      readwri=+1
      recno = 1
      if (rebin.gt.0) then
         nbin=nbinnew
         tbin=tbin*float(rebin)
         tres=tres*float(rebin)
      endif
      call rwepn(filename,readwri,recno,padout)

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rebinit(rebin, spctrmY, nfreq, newnfreq)

      implicit none

      integer nfreq, newnfreq, rebin, i, ic, j, jlast
      real spctrmY(nfreq), sumx, sumy

      ic=1
      do i=1, nfreq, rebin 
         sumy=0.0
         do j=i, i+rebin-1
            sumY=sumY+spctrmY(j)
            jlast=j
         enddo
         ic=ic+1
         spctrmY(ic)=sumY/float(rebin)
      enddo
      if (jlast.lt.nfreq) then
         sumy=0.0      
         do i=jlast+1, nfreq
            sumY=sumY+spctrmY(i)
         enddo
         ic=ic+1
         spctrmY(ic)=sumY/float(nfreq-jlast)
      endif
      newnfreq=ic-1

      end

