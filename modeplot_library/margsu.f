c----------------------------------------------------------
      subroutine margsu(string)
      character*(*) string
      include 'margli.h'
      logical quoted
      character*1 c
      lstring=istlen(string)
      lstrli(ilev)=lstring
      k=0
      numali(ilev)=0
      ip=0
      quoted=.FALSE.
      mode=1
      do i=1,lstring
        c=string(i:i)
        if(c.eq.'''') then
          quoted=.not.quoted
          goto 100
        endif
        goto (1,2),mode
c skipping spaces
    1   if(c.ne.' '.or.quoted) then
          numali(ilev)=numali(ilev)+1
          ip=ip+1
          lineli(ilev)(ip:ip)=c
          iptli(numali(ilev),ilev)=ip
          iptli(numali(ilev)+1,ilev)=ip+1
          mode=2
        endif
        goto 100
c accumulating argument
    2   if(c.ne.' '.or.quoted) then
          ip=ip+1
          lineli(ilev)(ip:ip)=c
          iptli(numali(ilev)+1,ilev)=ip+1
        else
          mode=1
        endif
        goto 100
  100   continue
      enddo
      return
      end
