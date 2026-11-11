      character*6 function fpt6(x,form)
      character*(*) form
      character*6 str6
      if(x.eq.0.) then
        fpt6=' '
      else
        write(str6,form) x
        fpt6=str6
      endif
      return
      end
