c==============================================================================
      program viewepn
c==============================================================================
c
c     Sample program to view EPN formatted files...
c
      implicit none 

      include 'epnhdr.inc'
      integer narg, lfil, nrec, iargc, nepnrec, maxrecs, hh, mm,
     &        yy, dd, nn, i, stat
      double precision ss, date, jul, x1, x2, x3, ffrac
      character*1 dsign, options*80, num*80
      logical first, show
      data first/.true./

      write(*,'('' VIEWEPN : A program to read EPN headers'')')
      readwri=-1
      narg=iargc()
      if (narg.lt.1) then
         write(*,'('' No options were specified!'')')
         write(*,'('' usage: viewepn [filename] [bdehunoptv]'',
     &             '' [num]'')')
         write(*,'('' You must specify the EPN filename!'')')
         write(*,'('' The optional extras are as follows:'')')
         write(*,'('' b: show bin information'')')
         write(*,'('' d: show pulsar DM'')')
         write(*,'('' e: show epoch'')')
         write(*,'('' f: show frequency'')')
         write(*,'('' h: show comments/history'')')
         write(*,'('' i: show ID tags'')')
         write(*,'('' n: show pulsar name'')')
         write(*,'('' c: show channel and calibration info'')')
         write(*,'('' o: show observed number of pulses'')')
         write(*,'('' p: show pulsar period'')')
         write(*,'('' t: show telescope'')')
         write(*,'('' v: show format version'')')
         write(*,'('' num: is the required record to display'')')
         stop
      else
         call gtarg(1,filename)
         lfil=index(filename,' ')-1
      endif

      if (narg.gt.1) call gtarg(2,options)
      if (narg.eq.1.or.options.eq.'-') options='vhnpditefo'

      maxrecs=nepnrec(filename)

      if (maxrecs.eq.0) stop 'File does not exist!'
      if (maxrecs.eq.-1) stop 'Error reading file!'

      recno=1
      if (narg.eq.3) then
         call gtarg(3,num)
         read(num,'(i5)') recno
         if (recno.gt.maxrecs) then
            write(*,'('' Requested record too large - using: '',i4)')
     &      maxrecs
            recno=maxrecs
         endif
      endif 

      call rwepn(filename,readwri,recno,padout)

c      write(*,*) papp(1)
c      write(*,*) f0(1)


      show=narg.gt.1.or.(narg.eq.1.and.recno.eq.1)
      if (recno.gt.0) nrec=recno
      if (recno.gt.0.and.show) then
      if (first) then
        write(*,'('' Contents of EPN data file: '',a,''...'')')
     &          filename(1:lfil)
      write(*,'('' @-----------------------------------------------'',
     &          ''------------------------------'')')
        first=.false.
      endif
      write(*,'('' Main Header Number '',i4)')recno
      if (index(options,'v').gt.0) 
     &write(*,'('' EPN format version '',a8)')version
      if (index(options,'h').gt.0) 
     &write(*,'('' -> '',a72)')history
      if (index(options,'n').gt.0) then
        write(*,'('' PSR '',a12,'' PSR '',a12)')jname,cname
        write(*,'('' RAJ '',i2.2,'':'',i2.2,'':'',f4.1)') rah,ram,ras
        dsign='+'
        if (ded.lt.0) dsign='-'
        if (ded.lt.0) ded=-1*ded
        if (dem.lt.0) dsign='-'
        if (dem.lt.0) dem=-1*dem
        if (des.lt.0) dsign='-'
        if (des.lt.0) des=-1*des
        write(*,'('' DEJ '',a1,i2.2,'':'',i2.2,'':'',f4.1)') 
     &      dsign,ded,dem,des
      endif
      write(*,'('' SCAN: '',i4.4)') scanno
      if (index(options,'p').gt.0) then
        write(*,'('' Pbar '',f11.6,'' ms'')')pbar*1000.0
        write(*,'('' Ptop '',f11.6,'' ms'')')papp(1)*1000.0
      endif
      if (index(options,'o').gt.0) then
        write(*,'('' Observed number of pulses '',i8)') nint
      endif
      if (index(options,'d').gt.0) 
     &write(*,'('' DM '',f8.3,'' pc/cc'')')dm
      if (index(options,'r').gt.0) 
     &write(*,'('' RM '',f10.3,'' rad/m2'')')rm
      if (index(options,'f').gt.0)then 
	if (index(f0u(1),'MHz').gt.0) then
	  f0u(1)=' GHz'
	  f0(1)=f0(1)/1000.0
        endif
        write(*,'('' Obs. freq: '',f12.8,a8)')f0(1),f0u(1)
        write(*,'('' Bandwidth: '',f12.6,a8)')df(1),dfu(1)
      endif
      if (index(options,'b').gt.0)then 
        write(*,'('' N bins: '',i6)')nbin
        write(*,'('' Tbin: '',f12.6,'' ms'')')tbin/1000.0
        if (tres.gt.0.0)
     &  write(*,'('' Tres: '',f12.6,'' ms'')')tres/1000.0
      endif
      if (index(options,'i').gt.0)then 
        write(*,'('' Cat Ref: '',a8)')catref
        write(*,'('' Bib Ref: '',a8)')bibref
      endif
      if (index(options,'t').gt.0) then
        write(*,'('' Telescope: '',a8)')telname
        if (xtel.ne.0.0.and.ytel.ne.0.0.and.ztel.ne.0.0) then
           write(*,'('' Topocentric x-position [m]: '',f17.5)')xtel
           write(*,'('' Topocentric y-position [m]: '',f17.5)')ytel
           write(*,'('' Topocentric z-position [m]: '',f17.5)')ztel
        else
           write(*,'('' Telescope coordinates not available!'')')
        endif
      endif
      if (index(options,'e').gt.0) then
      if (epoch.gt.0.0) then   
      date=epoch+tstart(1)/8.64e10
      write(*,'('' EPOCH (MJD): '',f16.9)')date

c     -- replace by routine that is working - mk
c      jul=date+2400000.5d0
c      call juca(jul,x1,x2,x3)
c      yy= x3
c      nn= x2
c      dd= x1 
      call julcal(date, yy, nn, dd, ffrac, stat)

      write(*,'('' Date: '',i2.2,''/'',i2.2,''/'',i4)') dd,nn,yy
      call dattim(date,hh,mm,ss)
      if (ss.ge.10.0) then
        write(*,'('' Time: '',i2.2,'':'',i2.2,'':'',f8.5)')hh,mm,ss
      else
        write(*,'('' Time: '',i2.2,'':'',i2.2,'':0'',f7.5)')hh,mm,ss
      endif
      else
        write(*,'('' No EPOCH written in EPN file!'')')
      endif
      if (timflag.eq.'U') then
         write(*,'('' Time stamp is topocentric UT'')')
      else if (timflag.eq.'B') then
         write(*,'('' Time stamp is barycentric UT'')')
      else 
         write(*,'('' No useful timing information!!'')')
      endif
      endif
      if (index(options,'c').gt.0) then
        write(*,*) 'Number of channels:',npol
        write(*,*) 'Number of sub-bands per polarization:',nfreq
        do i=1,npol*nfreq
         write(*,'(1x,a,i2,a,f10.4)') 'Channel: ',i,
     &   ' ID-Field : '//idfield(i)//' RMS : ',rms(i)
        enddo
        if (paflag.eq.'A'.and.npol.eq.4) then
          write(*,'('' Absolute polarisation PA available'')')
          write(*,'('' OPOS (Deg): '',f8.3)')opos
        else if (npol.eq.4) then
          write(*,'('' Relative polarisation PA available'')')
          write(*,'('' OPOS (Deg): '',f8.3)')opos
        else
          write(*,'('' No polarisation information available'')')
        endif
        if (fluxflag.eq.'F') then
          write(*,'('' Data are flux calibrated'')')
        else 
          write(*,'('' Data are not flux calibrated'')')
        endif
      endif
      write(*,'('' @-----------------------------------------------'',
     &          ''------------------------------'')')
      endif
      write(*,'('' Total number of records: '',i5)')maxrecs
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE julcal(DJM, IY, IM, ID, FD, J)
*+
*     - - - - -
*      D J C L
*     - - - - -
*
*  Modified Julian Date to Gregorian year, month, day,
*  and fraction of a day.
*
*  Given:
*     DJM      dp     modified Julian Date (JD-2400000.5)
*
*  Returned:
*     IY       int    year AD
*     IM       int    month
*     ID       int    day
*     FD       dp     fraction of day
*     J        int    status:
*                       0 = OK
*                      -1 = unacceptable date (before 4701BC March 1)
*
*  The algorithm is derived from that of Hatcher 1984
*  (QJRAS 25, 53-55).
*
*  P.T.Wallace   Starlink   2 April 1990
*-

      IMPLICIT NONE

      DOUBLE PRECISION DJM
      INTEGER IY,IM,ID
      DOUBLE PRECISION FD
      INTEGER J

      DOUBLE PRECISION F,D
      INTEGER JD,N4,ND10



*  Check if date is acceptable
      IF (DJM.LE.-2395522D0.OR.DJM.GE.1D9) THEN
         J=-1
      ELSE
         J=0

*     Separate day and fraction
         F=MOD(DJM,1D0)
         IF (F.LT.0D0) F=F+1D0
         D=NINT(DJM-F)

*     Express day in Gregorian calendar
         JD=NINT(D)+2400001

         N4=4*(JD+((6*((4*JD-17918)/146097))/4+1)/2-37)
         ND10=10*(MOD(N4-237,1461)/4)+5

         IY=N4/1461-4712
         IM=MOD(ND10/306+2,12)+1
         ID=MOD(ND10,306)/10+1
         FD=F

         J=0

      END IF

      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

