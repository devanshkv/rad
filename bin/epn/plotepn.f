c==============================================================================
      program plotepn
c==============================================================================
c
c     Sample program to plot EPN data
c
      implicit none

      include 'epnhdr.inc'
      
      real xmin,xmax,ymin,ymax,x(maxbin),snr,tmp(maxbin),nsig
      real ii(maxbin),ll(maxbin),vv(maxbin),th(maxbin),et(maxbin)
      real ltmp(maxbin),vtmp(maxbin),ttmp(maxbin),etmp(maxbin),sigma
      integer narg, lfil, i, j, k,ipol, nskip, nread,iargc,nbins
      logical binlab,ovelay,newplot,chflag(maxblk),select,loop,
     &    zoom,zoomed,plotpol,lth(maxbin),lt2(maxbin),move,hard
      integer nepnrec,nrec,binsta,binend,nplt
      character*24 fs,crms,csnr,option*80,xlabel,crec*8
c
c     Initialise some variables...
c
      padout=.false.
      readwri=-1
      recno=1
      binlab=.false.
      ovelay=.false.
      select=.false.
      newplot=.true.
      plotpol=.false.
      loop=.false.
      move=.false.
      zoom=.false.
      do i=1,maxblk
      chflag(i)=.false.
      enddo
      sigma=2.0
c
c     Startup... prompt user if no command line inputs specified...
c
      write(*,'('' PLOTEPN : A program to plot EPN data'')')
      narg=iargc()
      if (narg.lt.1) then
         write(*,'('' No options were specified!'')')
         write(*,'('' usage: plotepn [filename] <options>'')')
	 write(*,'('' -r: set start record number (def=1)'')')
         write(*,'('' -b: label in terms of bin number'')')
         write(*,'('' -o: overlay channels'')')
	 write(*,'('' -s: set start plotting bin (def=1)'')')
	 write(*,'('' -f: set final plotting bin (def=nbin)'')')
         write(*,'('' -c: channel to write (def=all)'')')
         write(*,'('' -p: polarisation plot'')')
         write(*,'('' -z: attempt to zoom in on pulse'')')
         write(*,'('' -l: infinite loop mode'')')
         write(*,'('' -m: movie loop mode'')')
         write(*,'('' -h: produce a hardcopy file (PostScript)'')')
         stop 
      else
         call gtarg(1,filename)
         lfil=index(filename,' ')-1
         nrec=nepnrec(filename)
         if (nrec.eq.0) then 
           write(*,'('' EPN file: '',a,'' not found!'')')
     &     filename(1:lfil)
           stop
         else if (nrec.eq.-1) then
           write(*,'('' Error reading EPN file: '',a,''!'')')
     &     filename(1:lfil)
           stop
         else
           write(*,'('' EPN file: '',a,'' contains '',i5,
     &     '' records...'')') filename(1:lfil),nrec
         endif
      endif
c
c     Read the file in first to establish number of channels and bins
c
      call rwepn(filename, readwri, recno, padout)
c
c     Set defaults....
c
      nskip=0
      nread=0
      recno=0
      binsta=1
      hard=.false.
      binend=nbin
c
c     Loop through other command line inputs if given...
c
      if (narg.gt.1) then
         do i=2,narg
            call gtarg(i,option)
            if (index(option,'-r').gt.0) then
               read(option(3:),'(i5)') recno
               recno=recno-1
               if (recno.lt.0) recno=1
            else if (index(option,'-c').gt.0) then
              read(option(3:5),'(i2)') j
              select=.true.
              if (j.ge.1.and.j.le.npol) chflag(j)=.true.
            else if (index(option,'-b').gt.0) then
               binlab=.true.
            else if (index(option,'-o').gt.0) then
               ovelay=.true.
            else if (index(option,'-p').gt.0) then
               if (npol.ne.4) then
                  write(*,*) 'Require four channels I,Q,U,V',
     &                       ' for polarisation plot!'
               else
                  write(*,*) 'Plotting in polarisation mode..'
                  plotpol=.true.
               endif
            else if (index(option,'-s').gt.0) then
               read(option(3:),'(i4)') binsta
               if (binsta.lt.0) binsta=1
            else if (index(option,'-f').gt.0) then
               read(option(3:),'(i4)') binend
               if (binend.gt.nbin) binend=nbin
            else if (index(option,'-l').gt.0) then
               loop=.true.
            else if (index(option,'-m').gt.0) then
               move=.true.
            else if (index(option,'-h').gt.0) then
               hard=.true.
            else if (index(option,'-z').gt.0) then
               zoom=.true.
               nsig=2.0
	       if (option(3:).ne.' ') read(option(3:),'(f6.3)')nsig
            endif
         enddo
      endif
c
c     Establish what will be plotted
c
      if (plotpol) then
         nplt=1
      else
        nplt=0
        write(*,*) 'The following channels will be plotted...'
        do i=1,npol
           if (.not.select) chflag(i)=.true.
           if (chflag(i)) then
              nplt=nplt+1
              write(*,'(1x,a,i2,a)') 'Channel: ',i,
     &        ' ID-Field : '//idfield(i)
           endif
        enddo
      endif
c
c     Abort if no channels selected...
c
      if (nplt.eq.0) stop 'No channels selected!'
      if (ovelay) nplt=1
c
c     Open PGPLOT
c
      if (hard) then
      call pgbegin(0,filename(1:index(filename,' ')-1)//'.ps/ps',1,nplt)
      else
      call pgbegin(0,'?',1,nplt)
      endif
      call pgvport(0.15,0.85,0.30,0.85)
      call pgsch(float(nplt)*1.1)
      call pgscf(2)
c
c     Main loop
c
      do while(.true.)

        recno=recno+1
        if (recno.gt.nrec.and.(loop.or.move)) recno=1
	snr=0.0
        call rwepn(filename, readwri, recno, padout)
c	do i=1,nbin
c	  ii(i)=rawdata(1,i)
c        enddo
c	call shift_prof(ii,nbin,12)
c	do i=1,nbin
c	  rawdata(1,i)=ii(i)
c        enddo
        zoomed=.false.
        if (recno.eq.-999) goto 999
        write(crec,'(i5.5)') recno
        if (recno.gt.0) then
          do ipol=1,npol
            if (chflag(ipol).or.(plotpol.and.ipol.eq.1)) then

              if (plotpol)
     &        call getppars(rawdata,sigma,nbin,ii,ll,vv,th,et,lth)
              
              xmin=1.0e32
              xmax=-1.0e32
              ymin=1.0e32
              ymax=-1.0e32
              nbins=0
              
              if (zoom.and.(.not.zoomed)) then
                binsta=1
                binend=nbin
                do i=1,nbin
                  tmp(i)=rawdata(ipol,i)
                enddo
                if (nbin.gt.20) call zoomin(tmp,binsta,binend,nsig)
                zoomed=.true.
              endif

              do i=binsta,binend
                 nbins=nbins+1
                 x(nbins)=float(i)*tbin/1.0e3
                 if (binlab) x(nbins)=float(i)
                 xmin=min(xmin,x(nbins))
                 xmax=max(xmax,x(nbins))
                 tmp(nbins)=rawdata(ipol,i)
                 ymin=min(ymin,tmp(nbins))
                 ymax=max(ymax,tmp(nbins))
                 if (plotpol) then
                    ltmp(nbins)=ll(i)
                    ymin=min(ymin,ltmp(nbins))
                    ymax=max(ymax,ltmp(nbins))
                    vtmp(nbins)=vv(i)
                    ymin=min(ymin,vtmp(nbins))
                    ymax=max(ymax,vtmp(nbins))
                    ttmp(nbins)=th(i)
                    etmp(nbins)=et(i)
                    lt2(nbins)=lth(i)
                 endif
              enddo
              if (ipol.eq.1.and.rms(1).gt.0.0) then
	        snr=ymax/rms(1)
	        write(csnr,'(''SNR = '',f8.1)')snr
              endif
              
              if (plotpol) then
c                 ymax=ymax*1.1
              else
c                 ymax=ymax*1.3
              endif
    
c              if (ymin.lt.0.0) ymin=ymin*1.1
c              if (ymin.gt.0.0) ymin=ymin*0.9
              call pgwindow(xmin,xmax,ymin,ymax)
		write(*,*) xmin,xmax,ymin,ymax

              if (newplot) then
                if (.not.move) then
                call pgask(.true.)
                call pgadvance
                else
                do k=1,1000000
                enddo
                call pgask(.false.)
                call pgadvance
                endif
                call pgbbuf
                if (plotpol) then
                   call pgvport(0.15,0.85,0.30,0.85)
                   call pgbox('bcst',0.0,0,'bc',0.0,0)
                else
                   call pgbox('bcnst',0.0,0,'bcnst',0.0,0)
                endif
              endif
              newplot=(.not.ovelay)
            
c	      call pgsls(3)
c	      call pgmove(xmin,0.0)
c             call pgdraw(xmax,0.0)
c	      call pgsls(1)
              call pgline(nbins,x,tmp)

              if (plotpol) then
               call pgsls(2)
               call pgsci(7)
               call pgline(nbins,x,ltmp)
               call pgsls(3)
               call pgsci(9)
               call pgline(nbins,x,vtmp)
               call pgsls(1)
               call pgsci(1)
              endif

              if (newplot.and.(.not.plotpol))  then
                call pgwindow(0.0,1.0,0.0,1.0)
	        write(crms,'(''RMS = '',f8.3)')rms(ipol)
	        call pgtext(0.01,0.9,idfield(ipol))
                call pgtext(0.75,0.9,'Record: '//crec)
              endif
            
              write(fs,'(1x,f12.8,a8)') f0(1),f0u(1)
              fs=' '
              xlabel='Time (milliseconds)'
              if (binlab) xlabel='Bin number'

              if (plotpol) then
               call pgvport(0.15,0.85,0.15,0.85)
               call pglabel(xlabel,' ',cname//telname//fs)
               call pgvport(0.15,0.85,0.30,0.85)
               call pglabel(' ','Intensity',' ')
               call pgvport(0.15,0.85,0.15,0.85)
              else if (ovelay) then
                call pglabel(xlabel,'Intensity',' ')
              else if (npol.eq.1.or.nplt.eq.1) then
                call pglabel(xlabel,'Intensity',
     &          cname//telname//fs)
              else if (ipol.eq.npol) then
                call pglabel(xlabel,'Intensity',' ')
              else
                call pglabel(' ','Intensity',' ')
              endif

              if (plotpol) then
                call pgvport(0.15,0.85,0.15,0.30)
                call pglabel(' ','P.A.',' ')
                call pgwindow(xmin,xmax,0.0,180.0)
                call pgbox('bcnst',0.0,0,'bc',0.0,0)
                call pgsci(3)
                if (sigma.le.0.0) then
                  call pgline(nbins,x,ttmp)
                else
                  do j=1,nbins
                    if (lt2(j)) call pgerry(1,x(j),ttmp(j)-etmp(j),
     &                                   ttmp(j)+etmp(j),1.0)
                  enddo
                endif
                call pgsci(1)
              endif
              call pgebuf
           endif
          enddo
        endif
        if (ovelay.and.recno.lt.nrec) then
            call pgask(.true.)
            call pgadvance
            call pgbox('bcnst',0.0,0,'bc',0.0,0)
        endif
      enddo

 999  call pgend

      end
      subroutine zoomin(tmp,bsta,bend,nsig)
      implicit none
      real tmp(*),nsig
      integer bsta,bend
      
      integer i
      real sum,ssq,mean,rms,npt,n10,s10,m10
      
      sum=0.0
      ssq=0.0
      npt=0.0
      n10=0.0
      s10=0.0
      do i=bsta,bend
        sum=sum+tmp(i)
        s10=s10+tmp(i)
        ssq=ssq+tmp(i)*tmp(i)
        npt=npt+1.0
        n10=n10+1.0
        mean=sum/npt
        rms=sqrt(ssq/npt)
        if (n10.eq.10) then
          m10=s10/n10
          n10=0.0
          s10=0.0
        endif
        if (npt.gt.10.0.and.m10.gt.mean+nsig*rms) goto 1
      enddo
1     bsta=i-1  
      sum=0.0
      ssq=0.0
      npt=0.0
      n10=0.0
      s10=0.0
      do i=bend,bsta,-1
        sum=sum+tmp(i)
        s10=s10+tmp(i)
        ssq=ssq+tmp(i)*tmp(i)
        npt=npt+1.0
        n10=n10+1.0
        mean=sum/npt
        rms=sqrt(ssq/npt)
        if (n10.eq.10) then
          m10=s10/n10
          n10=0.0
          s10=0.0
        endif
        if (npt.gt.10.0.and.m10.gt.mean+nsig*rms) goto 2
      enddo
2     bend=i+1      
      end
