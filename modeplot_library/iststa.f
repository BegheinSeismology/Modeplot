c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      function iststa(string)
      character*(*) string
      k=len(string)
      iststa=1
      do i=1,k
        if(string(i:i).ne.' ') goto 10
        iststa=1+iststa
      enddo
   10 continue
      return
      end
