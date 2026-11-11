c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine oassend(command,response,lr,iwt)
      save
      character*(*) command,response
      include 'oasstat.h'
      call coassend(ichan,command,response,lr,iwt)
      end
