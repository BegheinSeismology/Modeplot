c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine bffout(lufl,ifbin,ibuf,nwords,istat)
      dimension ibuf(*)
      nbytes=4*nwords
      call bffo(lufl,ifbin,ibuf,nbytes,istat,0)
      return
      end
