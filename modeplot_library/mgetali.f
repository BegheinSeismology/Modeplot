
c-------------------------------------------------------

      subroutine mgetali(iarg,string)
      character*(*) string
        
      include 'margli.h'
      string=lineli(ilev)(iptli(iarg+1,ilev):iptli(iarg+2,ilev)-1)
      return
      end
