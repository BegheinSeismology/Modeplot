c-----------------------------------------------------------------
      subroutine lparse(file,prompt,delim,suffix,commds,ncommds,doit)
      character*(*) file,suffix
      character*20 commds(*)
      character*1 prompt,delim
      logical quoted

      include 'margli.h'
      external doit
      data ilev/0/

      character*200 stringi,strj(MXLEVLL),string1
      dimension ipj(MXLEVLL),lstrj(MXLEVLL)
      character*200 string
      character*1 c
      character*80 swt,temp,getmli
      dimension num(2)
      data lubase/32/
      save 
      integer*4 lus(MXLEVLL),ierrlv(MXLEVLL)
      parameter (NCOMDS=2)
      character*20 comds(NCOMDS)
      data comds(1)/'check_syntax'/
      data comds(2)/'echo'/
#if ( defined(MachineT) )
      stop 'lparse does not work on Titan'
#else
      ilev=ilev+1
      ierrlv(ilev)=0
      if(file.eq.'/dev/tty') then
        lus(ilev)=5
      else
        lus(ilev)=lubase+ilev
        open(lus(ilev),file=file,status='old',iostat=ierr)
        if(ierr.ne.0) then
          write(0,*) 'Unable to open:'//file
          lus(ilev)=0
          ierrlv(ilev)=1
          goto 200
        endif
      endif
      lstrj(ilev)=0
      ipj(ilev)=1
 1000 if(ipj(ilev).gt.lstrj(ilev)) then
        if(lus(ilev).eq.5) write(6,"(a1,$)") prompt
        lstrj(ilev)=0
 1020   read(lus(ilev),"(a200)",end=100) stringi
        if(stringi(1:1).eq.'#'.and.lstrj(ilev).eq.0) goto 1000
        lstringi=istlen(stringi)
        if(lus(ilev).ne.5) write(6,'(a)') prompt//stringi(1:lstringi)
         strj(ilev)(lstrj(ilev)+1:lstrj(ilev)+lstringi)=
     $     stringi(1:lstringi)
        lstrj(ilev)=lstrj(ilev)+lstringi
        if(strj(ilev)(lstrj(ilev):lstrj(ilev)).eq.'\\') then
          lstrj(ilev)=lstrj(ilev)-1
          goto 1020
        endif
        ipj(ilev)=1
      endif
      lstring=0
      do while(ipj(ilev).le.lstrj(ilev).and.
     $         strj(ilev)(ipj(ilev):ipj(ilev)).ne.delim)
        lstring=1+lstring
        string(lstring:lstring)=strj(ilev)(ipj(ilev):ipj(ilev))
        ipj(ilev)=1+ipj(ilev)
      enddo
      ipj(ilev)=ipj(ilev)+1


      if(lstring.eq.0) goto 1000
      lstring=lstring+1
      string(lstring:lstring)=' '

      mode=1
      is1=0
      quoted=.FALSE.
      do is=1,lstring
        c=string(is:is)
        if(c.eq.'''') quoted=.not.quoted
        goto (1001,1002,1003,1004),mode
c skipping spaces
 1001   if((c.ne.' '.and.c.ne.'$').or.quoted) then
          is1=1+is1
          string1(is1:is1)=c
          mode=2
        else if(c.eq.'$') then
          inum=1
          num(1)=-1
          num(2)=-1
          lswt=-1
          mode=3
        endif
        goto 1099
c accumulating argument
 1002   if(c.eq.' '.and..not.quoted) then
          is1=is1+1
          string1(is1:is1)=c
          mode=1
        else
          is1=is1+1
          string1(is1:is1)=c
        endif
        goto 1099
c accumulating num
 1003   if(c.eq.' ') then
          if(num(1).lt.0.and.num(2).lt.0) then
            num(1)=0
            num(2)=99999
          else if(num(1).eq.99999.and.num(2).lt.0) then
            num(1)=1
            num(2)=99999
          else if(num(1).lt.0) then
            num(1)=0
          else if(num(2).lt.0) then
            num(2)=num(1)
          endif
          num(2)=max0(num(2),num(1))
          if(lswt.lt.0) then
            ia=num(1)
            do while(ia.le.numali(ilev-1).and.ia.le.num(2))
              do i=iptli(ia+1,ilev-1),iptli(ia+2,ilev-1)-1
                is1=is1+1
                string1(is1:is1)=lineli(ilev-1)(i:i)
              enddo
              is1=1+is1
              string1(is1:is1)=' '
              ia=ia+1
            enddo
          else
            ilev=ilev-1
            do ia=num(1),num(2)
              temp=getmli(swt(1:lswt),ia,ltemp)
              if(ltemp.le.0) goto 1103
              do i=1,ltemp
                is1=1+is1
                string1(is1:is1)=temp(i:i)
              enddo
              is1=1+is1
              string1(is1:is1)=' '
            enddo
 1103       continue
            ilev=ilev+1
          endif
          mode=1
        else if(c.eq."'".and.lswt.lt.0) then
          lswt=0
          mode=4
        else if(c.eq.'-') then
          inum=2
          num(inum)=-1
        else
          ic=ichar(c)
          if(ic.ge.z'30'.and.ic.le.z'39') then
            if(num(inum).lt.0) num(inum)=0
            num(inum)=num(inum)*10+ic-z'30'
          else if(c.eq.'*') then
            num(inum)=99999
          else
            write(0,*) 'Error decoding count variable'
            ierrlv(ilev)=3
            goto 200
          endif
        endif
        goto 1099
c accumulating swt
 1004   if(c.eq."'") then
          mode=3
        else
          lswt=lswt+1
          swt(lswt:lswt)=c
        endif
1099    continue
      enddo

      string=string1(1:is1)
      lstring=istlen(string)

      lcom=1
      do while(lcom.lt.lstring.and.string(lcom:lcom).ne.' ')
        lcom=lcom+1
      enddo
      if(string(lcom:lcom).eq.' ') lcom=lcom-1

      knt=0
      do i=1,NCOMDS
        if(string(1:lcom).eq.comds(i)(1:lcom)) then
          knt=knt+1
          icom=i
          modec=1
        endif
      enddo
      do i=1,ncommds
        if(string(1:lcom).eq.commds(i)(1:lcom)) then
          knt=knt+1
          icom=i
          modec=2
        endif
      enddo
      if(knt.gt.1) then
        write(0,*) 'Ambiguous command:',string(1:lstring)
        ierrlv(ilev)=1
        goto 200
      else if(knt.eq.0) then
        call margsu(string(1:lstring))
        call lparse(string(1:lcom)//suffix,prompt,delim,suffix,
     1              commds,ncommds,doit)
      else
        call margsu(string(1:lstring))
c do command
        if(modec.eq.1) then
          goto (1,2),icom
    1     continue
c                                      check_syntax
            ilev=ilev-1
            call chekmli(string(lcom+2:lstring),ierr)
            ilev=ilev+1
            if(ierr.ne.0) then
              ierrlv(ilev)=7
              goto 200
            endif
 
          goto 1100
    2     continue
          write(6,*) string(lcom+2:lstring)
          goto 1100
 1100     continue
        else if(modec.eq.2) then
          call doit(icom,ierr)
          if(ierr.ne.0) then
            ierrlv(ilev)=8
            goto 200
          endif
        endif
        goto 1000
      endif

  200 continue
      if(lus(ilev).eq.5) then
        ierrlv(ilev)=0
        goto 1000
      else
        ierr=ierrlv(ilev)
        if(lus(ilev).ne.0) close(lus(ilev))
        ilev=ilev-1
        if(ilev.gt.0.and.lus(ilev).ne.5)  ierrlv(ilev)=ierr
        return
      endif
  100 continue
      if(lus(ilev).ne.0) close(lus(ilev))
      ilev=ilev-1
#endif
      return
      end
