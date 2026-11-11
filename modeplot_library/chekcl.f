c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine chekcl(string)
      character*(*) string

      include 'getgnl.h'
      include 'getunx.h'
      external iargc,getarg
      call chekgl(string,iargc,getarg,ierr
     1  ,swnam,cline,deflt
     1  ,nswt,lswnam,ldeflt,ifreq,nmin,nmax,icnt,iopn,iptr,itable,lcom)
      if(ierr.ne.0) call exit(2)
      return
      end
