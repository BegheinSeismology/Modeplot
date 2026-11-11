c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine bffis(lufl,ifbin,ibuf,nbytes,istat,nread,irec)
      include 'openfile.h'
      dimension ibuf(*)
      krec=irec-1
      if(irec.eq.0) krec=jrec(lufl)
      if(jrecl(lufl).eq.0) then
        if(krec.lt.jrec(lufl)) then
          knt=jrec(lufl)-krec
          do while(knt.ne.0)
            ido=min0(32000,knt)
            call cmtio(jchn(lufl),4,ido,ires,ierrno)
            knt=knt-ido
          enddo
        else if(krec.gt.jrec(lufl)) then
          knt=krec-jrec(lufl)
          do while(knt.ne.0)
            ido=min0(32000,knt)
            call cmtio(jchn(lufl),3,ido,ires,ierrno)
            knt=knt-ido
          enddo
        endif
        call cread(jchn(lufl),ibuf,nbytes,nread,ierrno)
        if(nread.eq.255) nread=0
        istat=2
        if(ierrno.eq.0) then
          if(nread.eq.0) istat=3
        else
          istat=5
          call cperror('cread in bffis')
c          call check('cread in bffis')
        endif
        if(istat.eq.3) goto 30
        jrec(lufl)=1+krec
        return
   30   jfile(lufl)=1+jfile(lufl)
        jrec(lufl)=0
      else
      nb=nbytes
      if(jfile(lufl).ne.200) nb=min0(nb,jrecl(lufl))
        call clseek(jchn(lufl),jrecl(lufl)*krec,0,ires,ierrno)
        if(ierrno.ne.0) call check('clseek in bffis')
        call cread(jchn(lufl),ibuf,nb,nread,ierrno)
        if(ierrno.ne.0) call check('cread in bffis')
        istat=2
        if(nread.eq.0) istat=3
        if(istat.ne.3) then
          jrec(lufl)=krec+(nread+jrecl(lufl)-1)/jrecl(lufl)
        else
          jrec(lufl)=krec
        endif
      endif
      return
      end
