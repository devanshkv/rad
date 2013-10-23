      subroutine baseline(array,ndata,ipol)

c     -- nice little subroutine which automatically determines
c        and substracts an offset - got the basic idea from the
c        Jodrell search code.. - mk
c        adjusted for 2d array 26/10/04 - dmt

      implicit none
      
      real array, s,smin, r
      integer ndata, ksm, j, k, ipol
      
      dimension array(8,4096)

      ksm=ndata/2.5+0.5
      smin=1.e30

      do j = 1,ndata
         s=0.0
         do k = 1, ksm
            s = s + array(ipol,mod(j+k-1,ndata)+1)
         enddo
         if(s.lt.smin) smin=s
      enddo

      smin=smin/float(ksm)


      do j = 1,ndata
         array(ipol,j) = array(ipol,j) - smin
      enddo

      end

