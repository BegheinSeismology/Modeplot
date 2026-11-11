      subroutine tposn(lufl,ipfile)
      include 'openfile.h'
      if(jrecl(lufl).ne.0) pause 
     $'tposn: Attempt to position non-tape device'
      if(jfile(lufl).eq.ipfile) then
      else if(ipfile.gt.jfile(lufl)) then
        call cmtio(jchn(lufl),1,ipfile-jfile(lufl),ires,ierrno)
        if(ierrno.ne.0) call check('cmtio in tposn A')
        jfile(lufl)=ipfile
        jrec(lufl)=0
      else if(ipfile.lt.jfile(lufl)) then
        if(ipfile.eq.0) then
          call rewtp(lufl)
        else
          call cmtio(jchn(lufl),2,jfile(lufl)-ipfile+1,ires,ierrno)
          if(ierrno.ne.0) call check('cmtio in tposn B')
          call cmtio(jchn(lufl),1,1,ires,ierrno)
          if(ierrno.ne.0) call check('cmtio in tposn C')
          jfile(lufl)=ipfile
        endif
        jrec(lufl)=0
      endif
      return
      end
