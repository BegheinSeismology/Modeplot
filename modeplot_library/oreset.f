      subroutine oreset()
      include 'oasstat.h'
      character*80 mess
      character*1 str1
      do i=1,MXJTAP
        if(itchan(i).ge.-1) then
          write(str1,'(i1)') i-1
          call oassend('user/def='//str1,mess,lmess,-1)
          call oassend('offline',mess,lmess,-1)
          call oassend('dismount',mess,lmess,-1)
          call oassend('dismount/vol',mess,lmess,-1)
        endif
      enddo

      call oassend('user/def=0',mess,lmess,-1)


      return
      end
