
c-------------------------------------------------------------------
      subroutine chekmli(string,ierr)
      character*(*) string

      include 'margli.h'

      external miargli,mgetali

          call chekgl(string,miargli,mgetali,ierr
     1  ,swnam(1,ilev),cline(ilev),deflt(1,ilev)
     1  ,nswt(ilev),lswnam(1,ilev),ldeflt(1,ilev),ifreq(1,ilev)
     1  ,nmin(1,ilev),nmax(1,ilev),icnt(1,ilev),iopn(1,ilev),
     1  iptr(1,ilev),itable(1,1,ilev),lcoms(ilev))

      end
