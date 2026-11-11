      function flieee(x)
      equivalence (f,j)
      parameter (tw22=2.**-22)
#if ( defined(MachineT) )
      stop 'flieee does not work on Titan'
#else
      f=x
      if(btest(j,30)) then
        iexp=and(z'0000007f',ishft(j,-23))
      else
        iexp= or(z'ffffff80',ishft(j,-23))
      endif
      if(iexp.ne.z'ffffff80') then
        mant=or(z'00800000',and(j,z'007fffff'))
      else
        mant=ishft(and(j,z'007fffff'),1)
      endif
      if(btest(j,31)) mant=-mant
      if(iexp.ge.0) then
        flieee=mant*((2.**iexp)*tw22)
      else
        flieee=(mant*(2.**iexp))*tw22
      endif
#endif
      return
      end
