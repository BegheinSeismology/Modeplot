      character*5 function fpt5(x,form)
      character*(*) form
      character*5 str5
      if(x.eq.0.) then
        fpt5=' '
      else
        write(str5,form) x
        fpt5=str5
      endif
      return
      end
