
c-------------------------------------------------------------------
      character*80 function getmli(ident,nni,nbyts)
      character*(*) ident
      character*80 getgnl

      include 'margli.h'

      getmli=getgnl(ident,nni,nbyts
     1  ,swnam(1,ilev),cline(ilev),deflt(1,ilev)
     1  ,nswt(ilev),lswnam(1,ilev),ldeflt(1,ilev),ifreq(1,ilev)
     1  ,nmin(1,ilev),nmax(1,ilev),icnt(1,ilev),iopn(1,ilev)
     1  ,iptr(1,ilev),itable(1,1,ilev),lcoms(ilev))

      return
      end
