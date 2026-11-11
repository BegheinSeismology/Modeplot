        subroutine oasrewt()
        character*80 mess
        call oassend('rewind t:',mess,lmess,-1)
        call wait(100)
 1223   call oassend('rewind t:',mess,lmess,-1)
        if(lmess.gt.0) then
          if(mess(1:16).eq.'E-TAPE-REWINDING'.or.
     1       (lmess.gt.5.and.mess(lmess-5:lmess).eq.'INDING')) then
            call wait(10000)
            goto 1223
          else 
            write(0,*) '*** Warning *** OAS error:',mess(1:lmess)
            goto 1223
          endif
        endif
        call wait(500)
        call oassend('rewind t:',mess,lmess,-1)
        if(lmess.ne.0) then
          write(0,*) '*** Warning *** OAS error:',mess(1:lmess)
          goto 1223
        endif
        call wait(2000)
        return
        end
