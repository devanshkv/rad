c==============================================================================
      program ascii2epn
c==============================================================================
c
c     Sample program to convert ascii files into EPN format
c     Call for baseline subtraction included
c     Flux calibration included
c
      implicit none

      include 'epnhdr.inc'
      integer narg,lfil,i,j,k,iargc,istat,date(3)
      real prmax,degmax,dum,s1400,sum(4)
      double precision ra,dec,raj,decj,strad,sarad,twopi,pi
      character*12 cpostn,chra,chde
      
      pi=3.1415927
      twopi=pi*2.0
      strad = twopi/86400.0
      sarad = strad/15.0

      narg=iargc()
      if (narg.lt.1) then
      write(*,*)'ASCII2EPN: A Program to convert ASCII profiles -> EPN'
      write(*,*) 'No options were specified!'
      write(*,*) 'usage: ascii2epn [filestem]'
      write(*,*) 'the ASCII profile is contained in filestem.asc'
      write(*,*) 'Header information must be in filestem.inf'
      stop
      else
         call gtarg(1,filename)
         lfil=index(filename,' ')-1
      endif

c
c     set up the EPN variables... These are contained in the ".inf" file
c
     
      open(unit=10,file=filename(1:lfil)//'.inf',status='old',err=998)

      read(10,'(a)') history
      read(10,'(a12)') jname
      read(10,'(a12)') cname

      read(10,*) pbar
      read(10,*) dm
      read(10,*) rm

      read(10,'(a6)') catref
      read(10,'(a6)') bibref

      read(10,*) ra
      read(10,*) dec
      
      ra=ra*pi/180.0
      dec=dec*pi/180.0

      raj = ra/strad
      decj = dec/sarad
      chra = cpostn(raj,2,0)
      chde = cpostn(decj,3,1)
	
 1    format(i2.2)
 2    format(f4.2)
 3    format(i3.2)
      read(chra(1:2),1) rah
      read(chra(4:5),1) ram
      read(chra(7:11),2)ras
      read(chde(1:3),3) ded
      read(chde(5:6),1) dem
      read(chde(8:12),2) des 

      read(10,'(a8)') telname
      read(10,*) xtel
      read(10,*) ytel
      read(10,*) ztel

      read(10,*) epoch
      read(10,*) opos
      read(10,'(a1)') paflag
      read(10,'(a1)') timflag
      read(10,'(a1)') fluxflag
      fluxflag=' '
      
c
c     This next system call is system dependent - to get the current
c     date (i.e. created data of EPN file) from the system clock...
c     for compatibility, I've just set it with today's date (DRL)
c
c      call idate(cdm,cdd,cdy) !HP UX
      cdy=98
      cdm=11
      cdd=28
      cdy=cdy+1900
      call idate(date)
      cdd=date(1)
      cdm=date(2)
      cdy=date(3)

      scanno=1
      subscan=1
      read(10,*) npol, nfreq

      nint=1
      ncal=0
      lcal=0
      fcal=1.0

c
c     Sub-header variables...
c
      do i=1,npol
         read(10,'(a8)') idfield(i)
         nband(i)=i
         navg(i)=0
         read(10,*) f0(i)
         read(10,'(a8)') f0u(i)
         read(10,*) df(i)
         read(10,'(a8)') dfu(i)
         read(10,*) tstart(i)
         read(10,*) papp(i)
      enddo
      read(10,*) degmax
      read(10,*) s1400

      if (s1400.gt.0.0) fluxflag='F'
      if (npol.eq.4) fluxflag='F'

      close(unit=10)

c      all variables now been written in

      prmax=-1.0e32
      open(unit=10,file=filename(1:lfil)//'.asc',status='old',err=997)
      do i=1,maxbin
         if (npol.eq.1) read(10,*,iostat=istat) dum,rawdata(1,i)
         if (npol.eq.4)
     &   read(10,*,iostat=istat) dum,rawdata(1,i),rawdata(2,i),
     &                           rawdata(3,i),rawdata(4,i)
         if (istat.ne.0) goto 996
         prmax=max(prmax,rawdata(1,i))
      enddo
      close(unit=10)
c     write(*,'(a)') 'Before:' //rawdata(1,5)

 996  nbin=i-1
      tbin=pbar*1.0e6/float(nbin)*degmax/360.0  ! bin time in us for EPN fmt
      tres=tbin
      tres=300.0

c      tres=8.3e9*dm*(df(1)/32.0)/f0(1)/f0(1)/f0(1)
c      tres=sqrt(tbin*tbin+tres*tres)     
c      do i=1,nbin
c	do j=1,npol
c          rawdata(j,i)=rawdata(j,i)/prmax
c        enddo
c      enddo


c     calibrate to s1400 flux density or normalise to prmax
     
    
      if (npol.eq.1) then
         call baseline(rawdata,nbin,1)
         sum(1)=0.0
         do i=1,nbin
            sum(1)=sum(1)+rawdata(1,i)
         enddo


         if (s1400.gt.0) then
            do i=1,nbin
               rawdata(1,i)=(rawdata(1,i)*s1400*nbin)/(sum(1))
            enddo
         else if (s1400.eq.0) then
            do i=1,nbin
               rawdata(1,i)=rawdata(1,i)/prmax
            enddo
         endif
      endif
c
c Use following "do" to sum I bins for calibration check
c

      sum(1)=0.0
      do i=1,nbin
         sum(1)=sum(1)+rawdata(1,i)
      enddo

c Arecibo data in Jy not mJy therefore x1000

      sum(1)=(sum(1)/nbin)*1000

      if (s1400.gt.0) then
      open(unit=11,file='fluxes.dat',status='old',access='append')
      write(11,*) sum(1)," ",s1400
      close(unit=11)
      endif

      padout=.false.
      readwri=+1
      recno=1
      
      filename=filename(1:lfil)//'.epn'

c      write(*,'(a)') 'After:' //rawdata(1,5)

      call rwepn(filename,readwri,recno,padout)

      write(*,'(a)') ' Converted ASCII data to '//filename(1:lfil+4)
      stop
 997  write(*,'(a)') ' Could not find '//filename(1:lfil)//'.asc'
      stop
 998  write(*,'(a)') ' Could not find '//filename(1:lfil)//'.inf'
      stop
 999  write(*,'(a)') ' Could not find ASCII file: '//filename(1:lfil)
      stop
      end


c *************************************************************
      character*(*) function cpostn(pos1,ndec,isign)
c *************************************************************
c
c form a character string for a position pos1 in seconds with ndec
c decimal places. if isign.eq.0 supress + sign.
c
      character fmt*13
      double precision pos,pos1,dround,x
c
c round the position to the requested number of decimal places.
c
      nd=min(27,max(0,ndec))
      pos=dround(pos1,nd)
c
c deal with sign.
c
      cpostn=' '
      if(pos.lt.0.0) then
        cpostn(1:1)='-'
        ic=1
      elseif(isign.ne.0) then
        cpostn(1:1)='+'
        ic=1
      else
        ic=0
      endif
c
c delimiters.
c
      cpostn(3+ic:3+ic)=':'
      cpostn(6+ic:6+ic)=':'
c
c hours or degrees.
c
      x=abs(pos)
      i=x/3600.0
      write(cpostn(1+ic:2+ic),100) i
c
c minutes.
c
      x=x-3600.0*i
      i=x/60.0
      write(cpostn(4+ic:5+ic),100) i
c
c seconds without fractional part.
c
      x=x-60.0*i
      if(nd.le.0) then
        i=nint(x)
        write(cpostn(7+ic:8+ic),100) i
c
c seconds with fractional part.
c
      else
        write(fmt,101) nd+3,ndec
        write(cpostn(7+ic:nd+9+ic),fmt) x
c
c replace spaces with zeros.
c
        do 1 i=7+ic,8+ic
          if(cpostn(i:i).eq.' ') cpostn(i:i)='0'
    1   continue
      endif
      return
c
c formats.
c
  100 format(ss,i2.2)
  101 format(ss,'(ss,f',i3,'.',i3,')')
c
c end of character*(*) function cpostn.
c
      end
c
********************************************************************
      double precision function dround(dval,ndec)
********************************************************************
c
c this routine returns the value of dval rounded to ndec decimal places
c in the same way as the fortran output routines.  dval and the
c function value are both double precision.
c
      character buffer*30,fmt*8
      double precision dval,d
      save fmt
      data fmt/'(f30.??)'/
c
c adjust the format to the requested number of decimal places.
c
      nd=min(27,max(0,ndec))
      write(fmt(6:7),800) nd
c
c write dval into buffer with this number of decimal places.
c
      write(buffer,fmt,err=100,iostat=i) dval
c
c write successful, set dround to this value.
c
      read(buffer,fmt,err=100,iostat=i) d
      dround=d
      return
c
c write or read failed, return an arithmetically rounded value.
c
  100 continue
      d=nint(dval*10**nd)*10**(-nd)
      return
c
c format statement.
c
  800 format(i2)
c
c end of double precision function dround.
c
      end


