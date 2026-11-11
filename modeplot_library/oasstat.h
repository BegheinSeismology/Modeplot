      parameter (JKUND=-1)
      parameter (JKPAS= 0)
      parameter (JKOFL= 1)
      parameter (JKONL= 2)
      parameter (MXJTAP=2)
      character*1 tapeid
      character*1 ttyid
      character*20 oasvols,oashost
      common/oasi/ichan,itchan(MXJTAP),interc
     1   ,oasvols(MXJTAP),oashost,loashost,tapeid(MXJTAP),ttyid
