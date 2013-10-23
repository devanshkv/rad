c-----------------------------------------------------------------------

      subroutine maxmin(x,ndata,xmin,xmax)

      implicit none
      
      integer i,ndata,nmax
      real x,xmin,xmax

      parameter (nmax=2080)

      dimension x(nmax)
      
      xmin=x(1)
      xmax=x(1)

      do i=1,ndata
         if (x(i).gt.xmax) then
            xmax=x(i)
         elseif (x(i).lt.xmin) then
            xmin=x(i)
         endif
      enddo

      end

c---------------------------------------------------------------------

      subroutine text(txt)

      character txt*(*),oldtext*80,cd*80
      integer lw,ci

      call pgqlw(lw)
      call pgqci(ci)
      call pgslw(20)
      call pgsci(0)
      call pgmtext('T',.5,0.,0.,cd)
      call pgsci(ci)
      call pgslw(lw)
      call pgmtext('T',.5,0.,0.,txt)

      oldtext=txt

      return

      end

c-------------------------------------------------------------------

      subroutine get_rms(data,np,rms)

      integer np
      real*4 data(2080),rms,mean
         
      mean=0.
      rms=0.
      do i=1,np
         mean=mean+data(i)
      enddo
      
      mean=mean/float(np)
      
      do i=1,np
         rms=rms+(data(i)-mean)**2.
      enddo
      
      rms=sqrt(rms/float(np))

      return

      end

c------------------------------------------------------------------

      SUBROUTINE MDIAN1(Y,N,XMED)      
      implicit real (a-h,o-z)

      DIMENSION Y(N), X(1500)

      do i=1, n
         x(i)=y(i)
      enddo

      CALL SORT(N,X)
      N2=N/2
      IF(2*N2.EQ.N)THEN
        XMED=0.5*(X(N2)+X(N2+1))
      ELSE
        XMED=X(N2+1)
      ENDIF
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE SORT(N,RA)
      implicit real (a-h,o-z)

      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END

c----------------------------------------------------------------------

c This subroutine calculates the error of the positionangle by using
c a formula taken from Turlo el al. 1985 A&A

      subroutine error(stq,stu,bl,err)

      implicit none

      integer npmax,ndata,nbl
      real pi
      parameter (npmax=2080)
      parameter (pi=3.141592654)

      integer i,bl(2),j
      real stq(npmax),stu(npmax),rmsq,rmsu,err(npmax)
      real dummy(npmax)

      do i=npmax-1,1,-1
         if (stq(i).ne.0..and.stu(i).ne.0..and.stq(i+1).eq.0..and.
     $        stu(i+1).eq.0.)then
            ndata=i
            goto 11
         endif
      enddo

 11   nbl=bl(2)-bl(1)+1
      j=0
      do i=bl(1),bl(2)
         j=j+1
         dummy(j)=stQ(i)
      enddo
      call get_rms(dummy,nbl,rmsq)

      j=0
      do i=bl(1),bl(2)
         j=j+1
         dummy(j)=stU(i)
      enddo
      call get_rms(dummy,nbl,rmsu)

c-----Calculation of error

      do i=1,ndata
         err(i)=(sqrt((stq(i)*rmsu)**2.+(stu(i)*rmsq)**2.))/
     $           (2.*(stq(i)**2.+stu(i)**2.))
         err(i)=err(i)*180./pi
      enddo

      return

      end


