             Vectorized Results From File oasopen.f
                    Origin -- Line 40

 Line   Stmt   Time        Program

   40      *      4        DO i1=1, 2
   41     24     28           WRITE( )ITAPE + (i1 - 1)
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File oasopen.f
                    Origin -- Line 97

 Line   Stmt   Time        Program

   97     68      4        DO i1=1, 2
   98     69     63           CALL COPEN(STRING(1:INT(LSTRING)) // TAPEID(i1) // CONVERT(0), ITCH, 0,                            IERRNO, 0, 0)
   99     70      8           IF (IERRNO .EQ. 0) THEN
  100     71     15              ITCHAN(i1) = ITCH
                              ELSE
  101     72     13              IF (IERRNO .NE. 6 .AND. IERRNO .NE. 16) THEN
  102     73     14                 ITCHAN(i1) = -1
                                 ELSE
  104     74     14                 ITCHAN(i1) = -2
                                 END IF
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

--------------------------------------
             Vectorized Results From File oasopen.f
                    Origin -- Line 108

 Line   Stmt   Time        Program

  108     80      4        DO i1=1, 2
  109     81     14           WRITE( )i1 - 1
  110     83     23  43       CALL OASSEND('user/def=' // STR1, MESS, LMESS, -1)
  111     84      8           IF (LMESS .NE. 0) THEN
  111     85      3              GO TO 43
                              END IF
  112     87      6  44       CALL WAIT(300)
  113     88     17           CALL OASSEND('status/vol/type=0', MESS, LMESS, -1)
  114     89     54           IF ( CH_LEQ(MESS(INT(1):INT(2)), 'E-') .AND. LMESS .GE. 2 .AND.  CH_LNE                           (MESS(INT(1):INT(20)), 'E-OMSG-DISK NOT MOUN')) THEN
  114     90      3              GO TO 44
                              END IF
  116     91     28           OASVOLS(i1) = MESS(1:LMESS)
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 5 function calls.

