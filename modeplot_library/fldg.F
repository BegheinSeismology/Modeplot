      FUNCTION FLDG(X)
      INTEGER*4 IMANT,IEXP
#if ( defined(MachineS) )
      DATA IMANT/'00FFFFFF'X/,IEXP/'7F000000'X/
#else
      DATA IMANT/Z'00FFFFFF'/,IEXP/Z'7F000000'/
#endif
      LOGICAL BTEST
      EQUIVALENCE (Y,I)
      Y=X
#if ( defined(MachineS) || defined(MachineT) )
      FLDG=FLOAT(IAND(I,IMANT))
#else
      FLDG=FLOAT(AND(I,IMANT))
#endif
      IF(FLDG.NE.0.) THEN
#if ( defined(MachineS) || defined(MachineT) )
        IPOW=ISHFT(IAND(IEXP,I),-24)-70
#else
        IPOW=ISHFT(AND(IEXP,I),-24)-70
#endif
        FLDG=(16.**IPOW)*FLDG
        IF(BTEST(I,31)) FLDG=-FLDG
      ENDIF
      RETURN
      END
