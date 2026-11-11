c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine oasclose()
      save


      include 'oasstat.h'

      call cclose(ichan,ires,ierrno)
      return
      end
