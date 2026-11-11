c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      character*80 function getenv(ident,nbyts)
      character*(*) ident
      call cgetenv(ident,getenv,nbyts)
      do i=nbyts+1,80
        getenv(i:i)=' '
      enddo
      return
      end
