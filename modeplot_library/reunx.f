c---------------------------------------------------------------
      function reunx(id,iseq,nbyts)
      character*(*) id
      character*80 string,getunx
      string=getunx(id,iseq,nbyts)
      if(nbyts.gt.0) then
        read(string,*) temp
      else
        temp=0.0
      endif

      reunx=temp
      return
      end
