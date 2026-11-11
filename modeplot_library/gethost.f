c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      character*80 function gethost(nbyts)
      call cgethost(gethost,nbyts)
      do i=nbyts+1,80
        gethost(i:i)=' '
      enddo
      return
      end
