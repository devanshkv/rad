      program toaEPN

      implicit none

      integer iargc, pwrtwo, n1, n2, nnew, ndata, nmax, i, nsite,
     +        factor, recnum
      character comment1*12, comment2*12, comment3*20
      real*8  MJDstart, usecstart

      parameter (nmax=4096)

      real y1(nmax), y2(nmax), res, freq, y1max, y2max, restempl
      character profileName*80, templateName*80, line*80
      logical verbose, movetempl, quiet

      include 'epnhdr.inc'

      data verbose/.false./
      data movetempl/.false./

      if (iargc().lt.2) then
         write(*,*)' USAGE: toaepn <profile> <template> [-r<rec>]',
     +         ' [-v] [-m]'
         write(*,*) '  -v    verbose mode'
         write(*,*) '  -m    move template peak to bin#1'
         stop
      endif
      
      call getarg(1,profileName)
      call getarg(2,templateName)

      recnum=1
      do i=3, iargc()
         call getarg(i,line)
         if (line(1:2).eq.'-v') verbose=.true.
         if (line(1:2).eq.'-m') movetempl=.true.
         if (line(1:2).eq.'-r') then
            read(line(3:),*) recnum
         endif
         if (line(1:2).eq.'-m') movetempl=.true.
      enddo

c     readepn-files....

c     -- profile --
      recno=recnum
      if (verbose) write(*,*) 'Record: ', recno
      call rwepn(profilename,-1,recno,.false.)
      do i=1,nbin
         y1(i)=rawdata(1,i)
      enddo
      n1=nbin

c     -- check timing information for profile and get parameters
      if (timflag.ne.'U') stop 'No timing information for profile.'
      MJDstart=epoch
      usecstart=tstart(1)

      comment1=cname(1:8)
      write(comment2,100) scanno
 100  format(' Scan_',i6.6)
c      comment3=history(1:20)
      comment3=' '

c     - resolution
      res=sngl(tbin)

c     - frequency
      freq=sngl(f0(1))
      if (f0u(1).ne.'MHz') freq=freq*1000.0

c     -- site
      if (telname(1:1).eq.'E') then
         nsite=16
      else if (telname(1:3).eq.'lov') then 
         nsite=8
      else
         if (verbose) 
     +        write(*,'(a)') 'C UNKNOWN telescope! Assuming Effelsberg'
         nsite=16
      endif


c     -- template --
      recno=1
      call rwepn(templatename,-1,recno,.false.)
      do i=1,nbin
         y2(i)=rawdata(1,i)
      enddo
      n2=nbin
c     - resolution
      restempl=sngl(tbin)

      if (n1.ne.n2) then
         if (n1.gt.n2) then
            factor=n1/n2
            if (verbose) write(*,*) 'rebinning profile...',factor
            call rebin(y1,n1,factor,y1,nnew)
            n1=nnew
            res=res*factor
         endif
         if (n2.gt.n1) then
            factor=n2/n1
            if (verbose) write(*,*) 'rebinning template...',factor
            call rebin(y2,n2,factor,y2,nnew)
            n2=nnew
            restempl=restempl*factor
         endif
      endif

c     -- next power of two
      pwrtwo=2**(int(log10(float(n1))/log10(2.0)))
      if (pwrtwo.gt.1024) then
         factor=2
         if (verbose)
     +        write(*,*) 'rebinning profile & template factor of 2'
         call rebin(y1,n1,factor,y1,nnew)
         call rebin(y2,n2,factor,y2,nnew)
         n1=nnew
         res=res*factor
         restempl=restempl*factor
      endif
      
c     -- number of bins used for further processing
      ndata=n1
      
c     -- subtract baseline (not in toacalc anymore...for zero padding..)
      call suboffset(y1,ndata)  ! subtract flat baseline
      call suboffset(y2,ndata)
         
c     -- scale data after offset
      y1max=y1(1)
      y2max=y2(1)
      do i=2, ndata
         if (y1(i).gt.y1max) y1max=y1(i)
         if (y2(i).gt.y2max) y2max=y2(i)
      enddo
      do i=1, ndata
         y1(i)=y1(i)/y1max
         y2(i)=y2(i)/y2max
      enddo

c     -- zero padding --
c      do i=1, ndata
c         write(42,*) i, y1(i), y2(i)
c      enddo
      do i=ndata+1, pwrtwo
         y1(i)=0.0
         y2(i)=0.0
      enddo
      ndata=max(ndata,pwrtwo)

      if (verbose) then
         write(*,*) 'Final profile resolution:  ',res
         write(*,*) 'Final template resolution: ',restempl
      endif

      call calcTOA(y1, y2, ndata, 
     +             MJDstart,usecstart, res, nsite,
     +             freq, comment1, comment2, comment3,verbose,
     +             movetempl)
      
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      include 'rwepn.f'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine rebin(array,nsams,factor,result,num)

      implicit none

      integer nsams, num, factor, ntemp, i, j, ic
      real array(nsams), result(4096), sum

      num=int(nsams/factor)
      if (num.lt.3) then
         num=nsams
         do i=1, num
            result(i)=array(i)
         enddo
         return
      endif

      ntemp=num*factor
      ic=0
      do i=1, ntemp, factor
         sum=0.0
         do j=0, factor-1
            sum=sum+array(i+j)
         enddo
         ic=ic+1
         result(ic)=sum/float(factor)
      enddo

      if (ntemp.lt.nsams) then
         sum=0.0
         do i=ntemp+1, nsams
            sum=sum+array(i)
         enddo
         ic=ic+1
         result(ic)=sum/float(nsams-ntemp)
      endif

      num=ic
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine calcTOA(work, template, nbins, 
     +                   MJDstart,usecstart, res, nsite,
     +                   freq, comment1, comment2, comment3, 
     +                   verbose, movetempl)

c     input:   work(nbins)  - profile
c              template(ndata) - template
c              MJDstart - MJD of time stamp
c              usecstart - usec of timestamp - MJDstart
c              res - resolution in usec
c              nsite - obsys.dat telescope code
c              freq - observing frequency
c              comment1*12, comment2*12, comment3*20
c              verbose on/off
c              movetempl - move template peak to bin #1

      implicit none

      integer nbins, nsite, nbinsMax, i

      parameter (nbinsMax=4096)

      real work(nbins), template(nbins)
      real res, error, eshift,shift,snr,esnr, freq
      real amp(nbinsMax/2), pha(nbinsMax/2)
      real*8  MJDstart, MJDtoa, usecstart, usectoa, fracDayToa

      character comment1*12, comment2*12, comment3*20, asite*1

      logical verbose, movetempl

      if (verbose.and.movetempl) write(*,*) 'moving template'
      call makeFtempl(amp,pha,template,nbins,movetempl) 

c-----Do the frequency domain template fit
      call fftfit(work,amp,pha,nbins,shift,eshift,snr,esnr)
      if (verbose) write(*,*) 'Offset: ',shift, eshift

      error=eshift*res    ! error in usec
      usectoa=usecstart+dble(shift)*dble(res)
      
      fracDayToa=usectoa/86400.0d6   ! convert in fractional days
      mjdtoa=mjdstart
      if (int(fracDayToa).gt.0.0) then
         write(*,*) 'C Warning: usec-offset larger than one day!'
         mjdtoa=mjdtoa+int(fracDayToa)
         fracDayToa=fracDayToa-int(fracDayTOA)
      endif

c      write(*,*) mjdtoa, fracdaytoa

      if (nsite.lt.10) then
         asite=char(nsite+48)
      else
         asite=char(nsite+87)
      endif

      write(*,100) asite,comment1,freq, int(mjdTOA), fracDayTOA,
     +     error, comment2 !, comment3
 100  format(a1,1x,a12,1x,f9.3,x,i5,f14.13,2x,f7.2,1x,a12) !,10x,a20)

      return

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine suboffset(array,ndata)

c     new baseline routine - idea:
c     try to separate data into small protions of sizes stepped
c     in factors of two. Determing the rms in each segment and
c     go for the smallest one.

      implicit none
      
      real array, rms, rmsmin, mean, meanmin
      integer ndata, size, minsize, num, i, j, k
      
      dimension array(ndata)

      
      minsize=ndata/30
      rmsmin=1.e30
      meanmin=0.0
      
      size=ndata/2
      if (size.lt.minsize) then
         write(*,*) ndata, num, minsize
         write(*,*) 'no baseline subtracted!'
         return
      endif

      num=ndata/size

 100  continue
         do i=1, num
            rms=0.0
            mean=0.0
            do j = 1, size
               k=(i-1)*size+j
               mean=mean+array(k)
            enddo
            mean=mean/float(size)
            do j = 1, size
               k=(i-1)*size+j
               rms=rms+(array(k)-mean)**2
            enddo
            rms=rms/float(size-1)
            if (rms.lt.rmsmin) then
               rmsmin=rms
               meanmin=mean
            endif
         enddo
         if (size/2.gt.minsize) then
            size=size/2
            goto 100
         endif
 200  continue

      do j = 1,ndata
         array(j) = array(j) - meanmin
      enddo

      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine makeFtempl(amp,pha,da,nbins,movetempl)
      
C     Converts a standard profile into normalized harmonic-squared form
c
c     based on a subroutine by AW
c     
      
      implicit none
      
      integer maxsam,i,nh,ipos, nbins
      real fixoff, twopi,da,amp,pha,pmax
      logical movetempl
      
      parameter (MAXSAM=4096,twopi=6.2831853,fixoff=0.0)

      dimension da(nbins),amp(MAXSAM/2),pha(MAXSAM/2)

      complex cstd(0:MAXSAM/2)


c-------Normalize time-domain template to unit amplitude
      pmax=-1.e30
      do i=1,nbins
         if(da(i).gt.pmax) then
            pmax=da(i)
            ipos=i
         end if
      end do
      do i=1,nbins
         da(i)=da(i)/pmax
      end do

      if (movetempl) then
c
c---  move highest peak to bin #1
         ipos=1-ipos
         if(ipos.ne.0) call wrap(da,nbins,ipos)
      endif
      
      nh=nbins/2
      
c-------Computefrequency-domaintemplate
      call cprof(da,nbins,nh,cstd,amp,pha)
      
c-------DefineTOAsuch that phase of fundamental = 0
      do i=1,nh
         pha(i)=mod(pha(i)-float(i)*pha(1),twopi)	
      enddo
      

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine cprof(y,nmax,nh,c,amp,pha)

C  Compute FFT of profile in array y(nmax), and return amplitude and phase
C  in arrays amp(nh) and pha(nh).  Note that nh=nmax/2, and that the DC term
C  is returned in c(0), fundamental in c(1), ..., Nyquist freq in c(nh).

	parameter(MAXSAM=4096)
	real*4 y(nmax),amp(nh),pha(nh)
	complex c(0:nh),temp(MAXSAM)

	do 10 i=1,nh
10	 temp(i)=cmplx(y(2*i-1),y(2*i))
	call ffft(temp,nmax,1,1)
	c(0)=temp(1)
	do 20 i=1,nh
	 c(i)=temp(i+1)
	 amp(i)=cabs(c(i))
	 pha(i)=0.
20	if(amp(i).gt.0.) pha(i)=aimag(clog(c(i)))
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine fftfit(prof,s,phi,nmax,shift,eshift,snr,esnr)

C  Fourier transform domain routine for determining pulse TOAs.
C  Input data:
C	prof(nmax)	profile
C	s(nh)		standard profile amplitude
C	phi(nh)	standard profile phase
C	nmax		length of prof

C  Outputs:
C	shift		shift required to align std with prof, in bins
C	eshift	uncertainty in shift
C	snr		signal to noise ratio of prof
C	esnr		uncertainty in snr

C  Method:
C  It is assumed that prof(j)=a + b*std(j-tau) + noise(j), where the
C  (j-tau) subscript is supposed to mean that the time-shift between the
C  observed and standard profiles need not be an integer number of bins.
C  The algorithm is a straightforward Chi-squared minimization with respect
C  to a, b, and tau.  (The result for a, being of no physical or instrumental
C  interest, is not actually evaluated -- though it easily could be.)
C  First and second partial derivatives of Chisqr with respect to b and tau
C  are computed, and used to evaluate s, snr, and their "standard errors,"
C  or one-sigma uncertainties.  The only special trick is that the expression
C  for the best-fit value of tau defines it implicitly, and cannot be solved
C  analytically.  It is solved numerically instead, finding the minimum of
C  Chisqr near a best guess from a CCF at 32 lags done in the Fourier domain.

C  Also note that it may
C  be desirable to return the scale factor b relating prof to std, instead
C  of snr.  In that case you could also return the noise estimate rms.
c
c  Changed a lower bound to iterate the transcendental equation from 5 to 4,
c  in accord with changing nprof from 64 to 32 in fccf32.f.
c  AW, March 1992.

	parameter (twopi=6.2831853,MAXSAM=4096)
	real*4 prof(MAXSAM),p(MAXSAM/2),theta(MAXSAM/2)
	real*4 s(MAXSAM/2),phi(MAXSAM/2),r(MAXSAM/2),tmp(MAXSAM/2)
	complex cp(0:MAXSAM/2)
	logical low,high

	if (phi(1).ne.0) then
	  write (0,*) ' Phase of fundamental not zero, check .hm file'
	  stop
	end if

	nh=nmax/2
	call cprof(prof,nmax,nh,cp,p,theta)
	do 10 k=1,nh
	 tmp(k)=p(k)*s(k)
10	 r(k)=theta(k)-phi(k)

        fac=nmax/twopi

	call fccf(tmp,r,shift)


C  The "DO 60" loop solves the transcendental equation yielding the best-fit
C  value of tau.  Here the number starts at 16 (number used in CCF)
C  and increases by factors of 2, at each step finding the function
C  zero closest to the starting point from the previous iteration.

	tau=shift

	do 60 isum=4,99
	 nsum=2.0**isum
	 if(nsum.gt.nh) go to 70
	 dtau=twopi/(nsum*5)
	 edtau=1./(2.*nsum+1.)
	 if (nsum.gt.(nh/2.+.5)) edtau=1.e-4

	 ntries=0
	 low=.false.
	 high=.false.
50	 ftau=dchisqr(tau,tmp,r,nsum)
	 ntries=ntries+1
	 if(ftau.lt.0.0) then
	   a=tau
	   fa=ftau
	   tau=tau+dtau
	   low=.true.
	 else
	   b=tau
	   fb=ftau
	   tau=tau-dtau
	   high=.true.
	 end if
	 if (ntries.gt.10) then
	   shift=0.
	   eshift=999.
	   snr=0.
	   esnr=0.
	   return
	 end if
	 if (low.neqv.high) go to 50
	 tau=zbrent(a,b,fa,fb,edtau,tmp,r,nsum)
60	continue

70	s1=0.
	s2=0.
	s3=0.
	do 80 k=1,nh
	 cosfac=cos(-r(k)+k*tau)
	 s1=s1 + tmp(k)*cosfac
	 s2=s2 + s(k)**2
80	 s3=s3 + k**2 *tmp(k)*cosfac
	b=s1/s2
	s1=0.
	do 90 k=1,nh
	 sq=p(k)**2-2.*b*p(k)*s(k)*cos(r(k)-k*tau)+(b*s(k))**2
90	 if(s(k).ne.0.) s1=s1+sq
	rms=sqrt(s1/nh)
	errb=rms/sqrt(2.0*s2)
	errtau=rms/sqrt(2.0*b*s3)
	snr=2.0*sqrt(2.0*nh)*b/rms

	shift=fac*tau
	eshift=fac*errtau

	esnr=snr*errb/b

	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine fccf(amp,pha,shift)

C  Calculates CCF in Fourier domain using 8 harmonics of amp and pha arrays
C  amp=p*s,pha=theta-phi.  Finds maximum of CCF at 32 lags over the pulsar
C  period, and returns value of shift in radians.
c
c  Changed nprof from 64 to 32 (and all the appropriate hardwired constants)
c  to deal with 32-point profiles.  AW -- March 1992

	parameter (nprof=32,nprof1=nprof-1)
	parameter (MAXSAM=4096,twopi=6.2831853)
	real*4 amp(MAXSAM/2),pha(MAXSAM/2)
	complex ccf(0:nprof1)

	nh=nprof/2
	ccf(0)=(0.,0.)
	do 10 i=1,nh/2
	 ccf(i)=cmplx(amp(i)*cos(pha(i)),amp(i)*sin(pha(i)))
10	 ccf(nprof-i)=conjg(ccf(i))
	do 20 i=nh/2+1,nh
	 ccf(i)=(0.,0.)
20	 ccf(nprof-i)=(0.,0.)
	call ffft(ccf,nprof,-1,0)
	cmax=-1.e30
	do 30 i=0,nprof1
	 rc=real(ccf(i))
	 if (rc.gt.cmax) then
	   cmax=rc
	   imax=i
	 end if
30	continue

	fb=cmax
	ia=imax-1
	if(ia.eq.-1) ia=nprof-1
	fa=real(ccf(ia))
	ic=imax+1
	if(ic.eq.nprof) ic=0
	fc=real(ccf(ic))
	if ((2*fb-fc-fa).ne.0) then
	  shift=imax+0.5*(fa-fc)/(2*fb-fc-fa)
	else
	  shift=imax
	end if
c	if(shift.gt.nh) shift=shift-nprof
	shift=shift*twopi/nprof

	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine ffft(d,npts,isign,ireal)

C  Fourier transform of length npts=2**k, performed in place.
C  Input data in array d, treated as complex if ireal=0, and as real if ireal=1.
C  In either case the transform values are returned in array d, treated as
C  complex. The DC term is d(1), and d(npts/2+1) is the term at the Nyquist
C  frequency.  The basic algorithm is the same as Norm Brenner's FOUR1, and
C  uses radix-2 transforms.

C  J. H. Taylor, Princeton University.

	parameter(MAXSAM=4096)
	complex d(MAXSAM),t,w,wstep,tt,uu
	data pi/3.14159265/

C  Shuffle the data to bit-reversed order.

	imax=npts/(ireal+1)
	irev=1
	do 5 i=1,imax
	if(i.ge.irev) go to 2
	t=d(i)
	d(i)=d(irev)
	d(irev)=t
2	mmax=imax/2
3	if(irev.le.mmax) go to 5
	irev=irev-mmax
	mmax=mmax/2
	if(mmax.ge.1) go to 3
5	irev=irev+mmax

C  The radix-2 transform begins here.

	api=isign*pi/2.
	mmax=1
6	istep=2*mmax
	wstep=cmplx(-2.*sin(api/mmax)**2,sin(2.*api/mmax))
	w=1.
	do 9 m=1,mmax

C  This in the inner-most loop -- optimization here is important!
	do 8 i=m,imax,istep
	t=w*d(i+mmax)
	d(i+mmax)=d(i)-t
8	d(i)=d(i)+t

9	w=w*(1.+wstep)
	mmax=istep
	if(mmax.lt.imax) go to 6

	if(ireal.eq.0) return

C  Now complete the last stage of a doubled-up real transform.

	jmax=imax/2 + 1
	wstep=cmplx(-2.*sin(isign*pi/npts)**2,sin(isign*pi/imax))
	w=1.0
	d(imax+1)=d(1)

	do 10 j=1,jmax
	uu=cmplx(real(d(j))+real(d(2+imax-j)),aimag(d(j)) - 
     +    aimag(d(2+imax-j)))
	tt=w*cmplx(aimag(d(j))+aimag(d(2+imax-j)),-real(d(j)) +
     +    real(d(2+imax-j)))
	d(j)=uu+tt
	d(2+imax-j)=conjg(uu-tt)
10	w=w*(1.+wstep)

	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	function zbrent(x1,x2,f1,f2,tol,tmp,pha,nsum)

C Brent's method root finding, calls dchisqr(x,tmp,r,nsum) function for fftfit
C Fit refined till output accuracy is tol

	parameter (itmax=100,eps=6.e-8,MAXSAM=4096)
	real*4 tmp(MAXSAM/2),pha(MAXSAM/2)

	a=x1
	b=x2
	fa=f1
	fb=f2
	fc=fb
	do 11 iter=1,itmax
	if(fb*fc.gt.0.) then
	  c=a
	  fc=fa
	  d=b-a
	  e=d
	end if
	if(abs(fc).lt.abs(fb)) then
	  a=b
	  b=c
	  c=a
	  fa=fb
	  fb=fc
	  fc=fa
	end if
	tol1=2.*eps*abs(b)+0.5*tol
	xm=.5*(c-b)
	if(abs(xm).le.tol1 .or. fb.eq.0.) then
	  zbrent=b
	  return
	end if
	if(abs(e).ge.tol1 .and. abs(fa).gt.abs(fb)) then
	  s=fb/fa
	  if(a.eq.c) then
	    p=2.*xm*s
	    q=1.-s
	  else
	    q=fa/fc
	    r=fb/fc
	    p=s*(2.*xm*q*(q-r)-(b-a)*(r-1.))
	    q=(q-1.)*(r-1.)*(s-1.)
	  end if
	  if(p.gt.0.) q=-q
	  p=abs(p)
	  if(2.*p .lt. min(3.*xm*q-abs(tol1*q),abs(e*q))) then
	    e=d
	    d=p/q
	  else
	    d=xm
	    e=d
	  end if
	else
	  d=xm
	  e=d
	end if
	a=b
	fa=fb
	if(abs(d) .gt. tol1) then
	  b=b+d
	else
	  b=b+sign(tol1,xm)
	end if
	fb=dchisqr(b,tmp,pha,nsum)
11	continue
	zbrent=b
	return
	end

	function dchisqr(tau,tmp,r,nsum)

	parameter (MAXSAM=8192)
	real*4 tmp(MAXSAM/2),r(MAXSAM/2)

	s=0.
	do 40 k=1,nsum
40	 s=s+k*tmp(k)*sin(-r(k)+k*tau)
	dchisqr=s
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine wrap(data,nbin,nshift)
c
c     Routine to perform a circular shift of the pulse profile by
c     an integer number of bins.
c
      implicit none


      integer nbin, nshift, i, j, nbinMax
      real data, work

      parameter (nbinMax=4096)

      dimension data(nbin), work(nbinMax)
c
      do i=1,nbin
         j=i+nshift
         if(j.gt.nbin) j=j-nbin
         if(j.lt.1) j=j+nbin
         work(j)=data(i)
      end do

      do i=1,nbin
         data(i)=work(i)
      end do
c
      end


      

      


