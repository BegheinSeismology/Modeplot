      subroutine glinkd(linki,value,lvalue,istat)
      include 'olinkdir.h'
CC      character*24 olinkdir
CC      data olinkdir/'/home/eeyore/john/olinks/'/

      character*80 link
      character*(*) linki,value
      link=linki
      llink=istlen(link)
      ldir=istlen(olinkdir)
      value=link(1:llink)
      lvalue=llink
      istat=3
   10 continue
      call creadlink(olinkdir(1:ldir)//link(1:llink)//char(0)
     1   ,value,len(value),ires,ierrno)
      if(ierrno.eq.0) then
        link=value
        llink=ires
        lvalue=ires
        goto 10

      else if(ierrno.eq.2) then
        istat=1
      else if(ierrno.eq.22) then
        istat=0
      else




        call check('glink')
      endif

      return
      end
