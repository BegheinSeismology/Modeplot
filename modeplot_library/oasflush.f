
c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine oasflush()
      save


      integer EWOULDBLOCK
      parameter (EWOULDBLOCK=35)
      include 'oasstat.h'
      call cnoblock(ichan(jukeno),1,ires,ierrno)
      ierrno=0
      do while(ierrno.ne.EWOULDBLOCK)
        call cread(ichan(jukeno),cc,1,ires,ierrno)
      enddo

      call cnoblock(ichan(jukeno),0,ires,ierrno)
      end
