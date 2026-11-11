
      subroutine juke(ijuk)
      include 'oasstat.h'
      if(ijuk.lt.1.or.ijuk.gt.njuke) then
        write(0,'(''juke: invalid argument:'',i10)') ijuk
        call exit(2)
      endif
      jukeno=ijuk
      return
      end

