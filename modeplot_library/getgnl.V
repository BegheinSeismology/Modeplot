             Vectorized Results From File getgnl.f
                    Origin -- Line 17

 Line   Stmt   Time        Program

   17      9     66        DO WHILE LL .GT. 0 .AND. ( CH_LEQ(IDENT(INT(LL):INT(LL)), ' ') .OR.  CH_LEQ                           (IDENT(INT(LL):INT(LL)), CONVERT(0)))
   17     10      6           b1 = b1 + 1
   19     14      6           LL = LL - 1
                           END DO
   17      *      4        r1 = b1


No directives were found.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 24

 Line   Stmt   Time        Program

   24     21      5        DO cs32=1, NSWT
   25     22     79           IF (( CH_LEQ(IDENT(INT(1):INT(LL)), SWNAM(cs32)(INT(1):INT(LL))) .AND.                            LL .GT. 0) .OR. (LL .EQ. 0 .AND. LSWNAM(cs32) .EQ. 0)) THEN
   27     23      6              IMATCH = 1 + IMATCH
   28     24      8              IF (ISWT .EQ. 0) THEN
   28     25      4                 ISWT = cs32
                                 END IF
   29     26     20              IF (LSWNAM(cs32) .EQ. LL) THEN
   30     27      3                 IMATCH = 1
   31     28      4                 ISWT = cs32
   32     29      3                 GO TO 22
                                 END IF
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 70

 Line   Stmt   Time        Program

   70     60      5        DO i1=1, LDEF
   71     61     39           IF ( CH_LEQ(DEFLT(ISWT)(INT(i1):INT(i1)), '$')) THEN
   71     62      4              IPDOL = i1
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 86

 Line   Stmt   Time        Program

   86     82      6        DO i1=l1, LDEF
   87     83     30           IF ( CH_LEQ(STR80(INT(i1):INT(i1)), KET)) THEN
   87     84      6              IQ2 = i1 - 1
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 116

 Line   Stmt   Time        Program

  116    104      0        DO WHILE II .LE. LDEFLT(ISWT) .AND. IK .LT. NN
  116    105      0           b1 = b1 + 1
    *    106      0           b2 = 0
  117    107      0           DO WHILE II .LE. LDEFLT(ISWT) .AND.  CH_LEQ(DEFLT(ISWT)(INT(II):INT(II)                           ), ' ')
                                 (...)
                              END DO
  120    113      0           IK = IK + 1
  121    114      0           IJ = 0
    *    115      0           b2 = 0
  122    116      0           DO WHILE II .LE. LDEFLT(ISWT) .AND.  CH_LNE(DEFLT(ISWT)(INT(II):INT(II)                           ), ' ')
                                 (...)
                              END DO
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 2 function calls.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 117

 Line   Stmt   Time        Program

  117    107     46        DO WHILE II .LE. LDEFLT(ISWT) .AND.  CH_LEQ(DEFLT(ISWT)(INT(II):INT(II)),                            ' ')
  117    108      6           b2 = b2 + 1
  118    112      6           II = II + 1
                           END DO
  117      *      4        r2 = b2


No directives were found.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 122

 Line   Stmt   Time        Program

  122    116     46        DO WHILE II .LE. LDEFLT(ISWT) .AND.  CH_LNE(DEFLT(ISWT)(INT(II):INT(II)),                            ' ')
  122    117      6           b2 = b2 + 1
    *    120      7           br2 = IK .EQ. NN
  125    121     16           IF (br2) NBYTS(0) = IJ + 1
  126    122     45           IF (br2) return_GETGNL(INT(IJ + 1):INT(IJ + 1)) = DEFLT(ISWT)(II:II)
  123    126      6           IJ = IJ + 1
  128    128      6           II = II + 1
                           END DO
  122      *      4        r2 = b2


No directives were found.

--------------------------------------
             Vectorized Results From File getgnl.f
                    Origin -- Line 147

 Line   Stmt   Time        Program

  147    141      7        DO cs33=1, NN - 1
  148    142     25           IPT = ITABLE(IPT, 3)
  149    143      9           IF (IPT .EQ. IPT0) THEN
  150    144      3              IERR = 3
  151    145      3              GO TO 99
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 branches that exit the loop.

