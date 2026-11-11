c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine oasopen()
      save
      include 'oasstat.h'


      integer o341,z4000
      parameter (o341=1+8*(4+8*3))
      parameter (z4000=4*16**3)

      character*1 null,str1
      character*200 mess
      character*30 dev
      character*80 string,getenv,gethost
      null=char(0)

      mess=getenv('JUKE_HOST',lmess)
      if(lmess.le.0) then
        mess='owl.earth'
        lmess=istlen(mess)
      endif

      string=gethost(lstring)

      if(string(1:lstring).eq.mess(1:lmess)) then
        loashost=0
      else
        oashost=mess(1:lmess)//':'
        loashost=istlen(oashost)
      endif

      string=getenv('JUKE_CONFIG',lstring)
      if(lstring.le.0) then
        string='4 a'
        lstring=3
      endif
      read(string,'(i1)') itape
      do i=1,MXJTAP
        write(tapeid(i),'(i1)') itape
        itape=itape+1
      enddo
      dev='/dev/tty'//string(3:lstring)
      ldev=istlen(dev)
      if(loashost.gt.0) then
        string=oashost(1:loashost)//dev(1:ldev)
        ldev=ldev+loashost
        dev=string(1:ldev)
      endif

      ittry=0
      itry=0
   11 continue
      call copen(dev(1:ldev)//null,ichan,2,ierrno,0,0)
      if(ierrno.ne.0) then
        if(ierrno.eq.16) then
          if(itry.eq.0) then
            itry=1
            write(0,'(''oasopen: serial device busy: waiting ...'')')
          endif
          call wait(4000)
          goto 11
        else
          call cperror('oasopen')
          call exit(2)
        endif
      endif

c      call cgtflag(ichan,iflag,ires,ierrno)
c      iflag=o341
c      call cstflag(ichan,iflag,13,ires,ierrno)
c      call cgtlmw(ichan,iword,ires,ierrno)
c      call cstlmw(ichan,z4000,ires,ierrno)
c      call cstdtr(ichan,ires,ierrno)
c      call cstexc(ichan,ires,ierrno)
      call csetoas(ichan,ierrno)
      itried=0
   20 continue
      call oassend(' ',mess,lmess,350)
      if(lmess.lt.0) then
        if(itried.eq.0) then
          write(0,'(''oasopen: oas not responding: waiting ...'')')
        endif
        itried=1+itried
        call wait(4000)
        goto 20
      endif

      if(loashost.gt.0) then
        string=oashost(1:loashost)//'/dev/nrst'
      else
        string='/dev/nrst'
      endif
      lstring=istlen(string)
      do idev=1,MXJTAP
        call copen(string(1:lstring)//tapeid(idev)//null,itch,0,
     $              ierrno,0,0)
        if(ierrno.eq.0) then
          itchan(idev)=itch
        else if(ierrno.ne.6.and.ierrno.ne.16) then
          itchan(idev)=-1
        else
          itchan(idev)=-2
        endif
      enddo

      do idev=1,MXJTAP
        write(str1,'(i1)') idev-1
   43   call oassend('user/def='//str1,mess,lmess,-1)
        if(lmess.ne.0) goto 43
   44   call wait(300)
        call oassend('status/vol/type=0',mess,lmess,-1)
        if(lmess.ge.2.and.mess(1:2).eq.'E-'
     1    .and.mess(1:20).ne.'E-OMSG-DISK NOT MOUN') goto 44
        oasvols(idev)=mess(1:lmess)
      enddo
      call oassend('user/def=0',mess,lmess,-1)
c      do i=1,2
c         write(0,*) itchan(i),oasvols(i)
c      enddo

      return
      end
