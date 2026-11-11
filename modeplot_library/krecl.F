      integer*4 function krecl(nbytes)
      k=nbytes/4
      if(k*4.ne.nbytes) stop 'krecl: not divisible by 4'
#if ( defined(MachineA) )
      krecl=k
#else
      krecl=nbytes
#endif
      return
      end


      
