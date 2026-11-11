c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine strip(mess,lmess)
      character*1 null,nl
      parameter (nl=char(10))
      character*(*) mess
      null=char(0)
      do while (lmess.gt.0)
        if(mess(lmess:lmess).ne.'>'.and.mess(lmess:lmess).ne.nl
     1  .and.mess(lmess:lmess).ne.null
     1   .and.mess(lmess:lmess).ne.' ') goto 10
        lmess=lmess-1
      enddo
   10 continue
      return
      end
