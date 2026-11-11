c-------------------------------------------------------------------
c-------- Programmed by J. H. Woodhouse ----------------------------
c-------------------------------------------------------------------
      subroutine wait(millis)
      isec=millis/1000
      msec=mod(millis,1000)
      nrep=msec/32
      mrem=mod(msec,32)
      if(isec.ne.0) call csleep(isec)
      return
      end
