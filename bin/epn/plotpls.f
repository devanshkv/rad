c==============================================================================
      program plotpls
c==============================================================================
c
c     Sample program to plot EPN data
c
      implicit none

      include 'epnhdr.inc'
      real x(maxbin),iprf(maxblk,maxbin),tmp(maxblk,maxbin),y(maxbin)
      real data,gscale,ex,pyl,pyh
      integer nepnrec,nrec,nch,ichan,bleft,blast,nbins
      integer narg, lfil,i,j,nskip, nread,iargc,ln,n2plt,ndone
      logical chflag(maxblk),intprf,box
      real dmin,dmax,scfac(maxblk),xmin,xmax,ymin,ymaxl,ymaxh,
     &      xd,nl,nh,p
      character*1 option*80,ichar,frequenz*32
      
      xmin=0.15
      xmax=0.85
      ymin=0.25
      ymaxl=0.75
      ymaxh=0.85

      n2plt=25
      gscale=1.0
      do i=1,maxblk
        scfac(i)=0.0
      enddo

      write(*,'('' PLOTPLS : A program to plot single pulses'')')
      narg=iargc()
      if (narg.lt.1) then
         write(*,'('' No options were specified!'')')
         write(*,'('' usage: plotpls [filename] <options>'')')
         write(*,'('' N.B. Input file must be in EPN format!!'')')
	 write(*,'('' -r: set start record number (def=1)'')')
	 write(*,'('' -c: display channel number(s) optional'')')
	 write(*,'('' -s: global scale factor (def=1.0)'')')
	 write(*,'('' -n: number of pulses per page (def=25)'')')
         write(*,'('' -b: beginning bin no. to plot (def=1)'')')
         write(*,'('' -e: end bin no. to plot (def=nbin)'')')
	 write(*,'('' -i: show integrated pulse profile'')')
         stop 
      else
         call gtarg(1,filename)
         lfil=index(filename,' ')-1
         nrec=nepnrec(filename)
         if (nrec.eq.0) then 
           write(*,'('' EPN file: '',a,'' not found!'')')
     &     filename(1:lfil)
           stop
         endif
      endif

      padout=.false.
      readwri=-1
      recno=1

      recno=1
      call rwepn(filename, readwri, recno, padout)
      p=float(pbar)
      nskip=0
      nread=0
      recno=0
      nch=1
      do i=1,maxblk
         chflag(i)=.false.
      enddo
      chflag(1)=.true.
      intprf=.false.
      bleft=0
      blast=0

      if (narg.ge.2) then
         do i=2,narg
  	   call gtarg(i,option)
           if (index(option,'-r').gt.0) then
              read(option(3:),'(i5)') recno
              recno=recno-1
              if (recno.lt.0) recno=0
           else if (index(option,'-n').gt.0) then
              read(option(3:),'(i5)') n2plt
           else if (index(option,'-i').gt.0) then
              intprf=.true.
           else if (index(option,'-s').gt.0) then
              read(option(3:),'(f7.2)') gscale
           else if (index(option,'-b').gt.0) then
               read(option(3:),'(i5)') bleft
           else if (index(option,'-e').gt.0) then
               read(option(3:),'(i5)') blast
           else if (index(option,'-c').gt.0) then
              nch=0
              do j=1,npol
                 write(ichar,'(i1)') j
                 chflag(j)=(index(option(3:),ichar).gt.0)
                 if (chflag(j)) nch=nch+1
              enddo
           endif
         enddo
      endif

      xd=(xmax-xmin)/float(nch)
      
      call pgbegin(0,'?',1,1)
      call pgpaper(7.0,1.618)
      call pgscf(2)

      if (bleft.eq.0) bleft=1
      if (blast.eq.0) blast=nbin

      nbins=0
      do i=1,nbin
        if (i.ge.bleft.and.i.le.blast) then
           nbins=nbins+1
           x(nbins)=float(nbins)*float(tbin)/1000.0
        endif
      enddo
      
      ndone=n2plt
      ex=float(n2plt)*0.1
	ex=0.0
      if (.not.intprf) ymaxl=ymaxh

      nh=float(recno)
      nl=nh-1.0*float(n2plt)
      box=.true.
        
      do while(recno.ne.-999)

        if (ndone.eq.n2plt) then
          call pgask(.true.)
          call pgadvance
          nl=nl+float(n2plt)
          nh=nh+float(n2plt)
          call pgwindow(float(tbin)/1000.0,
     &                  float(nbins)*float(tbin)/1000.0,nl,nh+ex)
          ndone=0
          do i=1,nbin
           do j=1,maxblk
           tmp(j,i)=0.0
           iprf(j,i)=0.0
           enddo
          enddo
          box=.true.
        endif

        recno=recno+1
        call rwepn(filename, readwri, recno, padout)

        if (recno.gt.0) then
           ln=index(cname,' ')-1
           nch=0

           do ichan=1,npol
             if (chflag(ichan)) then
               nch=nch+1
               if (box) then
                 call pgvport(xmin,xmax,ymin,ymaxl)
                 if (n2plt.eq.1) then
                   call pgbox('bcnst',0.0,0,'bcnst',0.0,0)
                 else
                   call pgbox('bcnst',0.0,0,'bcnst',0.0,0)
                 endif
                 call pgwindow(0.0,1.0,p*nl,p*(nh+ex))
                 call pgbox('bc',0.0,0,'bcm',0.0,0)
                 call pgvport(xmin,xmax,ymin,ymaxh)
                 call pgwindow(0.0,1.0,0.0,1.0)
                 call pgptext(1.1,0.5,90.0,0.5,'Time (seconds)')
		 call pgsch(1.0)
                 if (npol.eq.1) then
                   call pglabel('Pulse Phase (ms)','Pulse Number',' ')
c                   call pglabel('','Pulse Number',' ')
                 else
                   call pglabel(' ','Pulse Number',' ')
                 endif
                 call pgsch(1.0-float(nch)/5.0)
                 box=.false.
               endif
               call pgvport(xmin+float(nch-1)*xd,xmin+float(nch)*xd,
     &                ymin,ymaxl)
               call pgwindow(float(tbin)/1000.0,
     &         float(nbins)*real(tbin)/1000.,nl,nh+ex)

               if (scfac(ichan).eq.0.0) then
                 dmax=-1.0e32
                 dmin=+1.0e32
                 do i=1,nbin
                   dmax=max(dmax,rawdata(ichan,i))
                   dmin=min(dmin,rawdata(ichan,i))
                 enddo
                 scfac(ichan)=gscale/dmax
               endif

               nbins=0
               do i=1,nbin
                 data=rawdata(ichan,i)*scfac(ichan)+float(ndone+1)+nl
                 iprf(ichan,i)=iprf(ichan,i)+data-float(ndone+1)-nl
                 if (data.gt.tmp(ichan,i)) then
                   tmp(ichan,i)=data
                 endif
                 if (i.ge.bleft.and.i.le.blast) then
                   nbins=nbins+1
                   y(nbins)=tmp(ichan,i)
                 endif
               enddo
               call pgline(nbins,x,y)
               if (ndone.eq.n2plt-1) then
                  if (intprf) then
                  call pgvport(xmin+float(nch-1)*xd,xmin+float(nch)*xd,
     &            ymaxl,ymaxh)
c                  if (nch.eq.1) then
                     pyl=+1.0e32
                     pyh=-1.0e32
c                  endif
                  nbins=0
                  do i=1,nbin
                     if (i.ge.bleft.and.i.le.blast) then
                       nbins=nbins+1
                       y(nbins)=iprf(ichan,i)
                       pyl=min(pyl,y(nbins))
                       pyh=max(pyh,y(nbins))
                     endif
                  enddo
          call pgwindow(0.0,float(nbins)*real(tbin)/1000.0,pyl,pyh*1.1)
                  call pgbox('bc',0.0,0,'bc',0.0,0)
                  call pgline(nbins,x,y)
                  endif
                 if (telname.eq.' ') telname=idfield(ichan)
                 write(frequenz,'(f6.2,a8)') f0(ichan),f0u(ichan)
c                 call pglabel(' ',' ',telname(1:8)
c     &                        //frequenz)
c                 call pglabel(' ',' ',telname)
                 if (telname.eq.idfield(ichan)) telname= ' '
               endif
             endif
           enddo
        endif

        ndone=ndone+1

      enddo
        
      call pgend
      end


