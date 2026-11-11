      parameter (MXLEVLL=10)
      character*200 lineli
      common/margch/lineli(MXLEVLL)
      common/margln/ilev,lstrli(MXLEVLL),numali(MXLEVLL),
     $              iptli(30,MXLEVLL) 
      character*200 cline
      character*30 swnam
      character*80 deflt
      common/argcm/
     1  nswt(MXLEVLL),lswnam(30,MXLEVLL),ldeflt(30,MXLEVLL),
     1   ifreq(30,MXLEVLL)
     1  ,nmin(30,MXLEVLL),nmax(30,MXLEVLL),icnt(30,MXLEVLL),
     1   iopn(30,MXLEVLL)
     1  ,iptr(30,MXLEVLL),itable(100,3,MXLEVLL),lcoms(MXLEVLL)
     1  ,cline(MXLEVLL),swnam(30,MXLEVLL),deflt(30,MXLEVLL)
