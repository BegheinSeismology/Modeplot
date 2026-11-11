      subroutine igdate(iadate)
      dimension iadate(3)
#if ( defined(MachineT) )
      CALL IDATE(IADATE(2),IADATE(1),IADATE(3))
#else
      CALL IDATE(IADATE)
#endif
      iadate(3)=1900+mod(iadate(3),1900)
      return
      end
