      parameter (ncommds=1)
      character*30 commds(ncommds)
      data commds(1)/'write'/
      external dbdoit
      call lparse('/dev/tty','>','.dbi',commds,ncommds,dbdoit)
      end
