      subroutine offset(array,ndata)

c     -- nice little subroutine which automatically determines
c        and substracts an offset - got the basic idea from the
c        Jodrell search code.. - mk

      implicit none
      
      real array, s,smin, r
      integer ndata, ksm, j, k
      
      dimension array(ndata)

      ksm=ndata/2.5+0.5
      smin=1.e30

      do j = 1,ndata
         s=0.0
         do k = 1, ksm
            s = s + array(mod(j+k-1,ndata)+1)
         enddo
         if(s.lt.smin) smin=s
      enddo

      smin=smin/float(ksm)


      do j = 1,ndata
         array(j) = array(j) - smin
      enddo

      end


