             Vectorized Results From File omount.f
                    Origin -- Line 15

 Line   Stmt   Time        Program

   15      9     30        DO WHILE NBYS .LE. LNAME .AND.  CH_LNE(NAME(INT(NBYS):INT(NBYS)), '/')
   15     10      6           b1 = b1 + 1
   16     14      6           NBYS = 1 + NBYS
                           END DO
   15      *      4        r1 = b1


No directives were found.

--------------------------------------
             Vectorized Results From File omount.f
                    Origin -- Line 74

 Line   Stmt   Time        Program

   74     63      4        DO cs40=1, 2
   75     64     40           IF ( CH_LEQ(OASVOLS(cs40), BACK(INT(1):INT(LBACK)))) THEN
   76     65     19              IF (ITCHAN(cs40) .GE. -1) THEN
   77     66     19                 IF (ITCHAN(cs40) .GE. 0) THEN
   77     67     24                    CALL CCLOSE(ITCHAN(cs40), IRES, IERRNO)
                                    END IF
   78     68     14                 ITCHAN(cs40) = -1
   79     69     14                 WRITE( )cs40 - 1
   80     70     23                 CALL OASSEND('user/def=' // STR1, MESS, LMESS, -1)
   81     71     17                 CALL OASSEND('offline', MESS, LMESS, -1)
   82     72     17                 CALL OASSEND('dismount', MESS, LMESS, -1)
   83     73     17                 CALL OASSEND('dismount/v', MESS, LMESS, -1)
   84     74     28                 OASVOLS(cs40) = CONVERT(0)
                                 ELSE
   86     75      6                 ISTAT = -2
   87     76      4                 WRITE( )'omount: Back of volume in use; waiting ...'
   88     77      3                 GO TO 88
                                 END IF
                              ELSE
   90     78     40              IF ( CH_LEQ(OASVOLS(cs40), VOLNAM(INT(1):INT(LVOLNAM)))) THEN
   91     79     19                 IF (ITCHAN(cs40) .GE. -1) THEN
                                    ELSE
   93     80      4                    WRITE( )'omount: Volume in use; waiting ...'
   94     81      6                    ISTAT = -1
   95     82      3                    GO TO 88
                                    END IF
                                 END IF
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 7 function calls.
   2) This loop contains 2 branches that exit the loop.

--------------------------------------
             Vectorized Results From File omount.f
                    Origin -- Line 104

 Line   Stmt   Time        Program

  104     93      4        DO i1=1, 2
  105     94     19           IF (ITCHAN(i1) .GE. -1) THEN
  106     95     14              IF (IUSER1 .LT. 0 .OR. IUSER1 .NE. IUP) THEN
  106     96      6                 IUSER1 = i1 - 1
                                 END IF
  107     97     40              IF ( CH_LEQ(OASVOLS(i1), VOLNAM(INT(1):INT(LVOLNAM)))) THEN
  108     98      6                 IUSER = i1 - 1
  109     99      3                 ISMOUNTED = 1
                                 END IF
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

--------------------------------------
             Vectorized Results From File omount.f
                    Origin -- Line 189

 Line   Stmt   Time        Program

  189    162      4        DO i1=1, 2
  190    163     19           IF (ITCHAN(i1) .GE. 0) THEN
  191    164     24              CALL CCLOSE(ITCHAN(i1), IRES, IERRNO)
                              END IF
  193    165      4           CALL OASCLOSE()
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 2 function calls.

