      subroutine oposn(lufl,nrecs,ierr)
      include 'openfile.h'
cc     parameter (nlu=40)
cc     parameter (nluuse=20)
cc     common/openfile/jchn(nlu),jrec(nlu),jfile(nlu),jrecl(nlu)
cc    1  ,lenglu(nlu)
cc     character*32 opnnam
cc     common/openname/opnnam(nlu)
      common/optdr/ifopt(nlu)
      character*200 mess
      character*14 command
      ierr=0
      if(ifopt(lufl).eq.0) then
        pause 'oposn: attempt to position a non-optical device'
        ierr=2
      endif
      write(command,'(''pos/rec='',i6)') nrecs
      do i=9,14
        if(command(i:i).eq.' ') command(i:i)='0'
      enddo

      call oassend('offline',mess,lmess,-1)
      call wait(10)
      call oassend(command,mess,lmess,-1)
      call wait(30)
      if(lmess.ne.0) then
        write(6,'(4x,200a1)') (mess(i:i),i=1,lmess)
        ierr=1
      endif

      call oassend('online',mess,lmess,-1)
      return
      end
