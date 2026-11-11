      subroutine complnam(namei,nameo,lnameo)
      character*(*) namei,nameo
      character*80 getenv,temp
      lnamei=istlen(namei)
      if(namei(1:2).ne.'~/') then
        nameo=namei
        lnameo=lnamei
      else
        temp=getenv('HOME',nbyts)
        nameo=temp(1:nbyts)//namei(2:lnamei)
        lnameo=nbyts+lnamei-1
      endif
      return
      end
