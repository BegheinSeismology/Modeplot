      subroutine oasrelt()
      include 'oasstat.h'
cc     parameter (JKUND=-1)
cc     parameter (JKPAS= 0)
cc     parameter (JKOFL= 1)
cc     parameter (JKONL= 2)
cc     parameter (MXJUKE=2)
cc     common/oasi/njuke,jukeno,ichan(MXJUKE),jukest(MXJUKE),itchan,interc
      if(itchan.gt.0) call cclose(itchan,ires,ierrno)
      return
      end
