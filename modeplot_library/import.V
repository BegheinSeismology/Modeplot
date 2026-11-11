             Vectorized Results From File import.f
                    Origin -- Line 43

 Line   Stmt   Time        Program

   43      6      0        DO WHILE IERR .EQ. 0
   43      7      0           b1 = b1 + 1
   44      8      0           READ()LINE
   45      9      0           IF (IERR .EQ. 0) THEN
   46     10      0              J = 1
    *     11      0              b2 = 0
   50     12      0              DO WHILE  CH_LNE(LINE1(J), ' ') .AND.  CH_LNE(LINE1(J), CONVERT(0))
                                    (...)
                                 END DO
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 2 function calls.
   2) This loop contains IO statements and vectorizable inner loops.
      Use PBEST/VBEST to override.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 50

 Line   Stmt   Time        Program

   50     12     52        DO WHILE  CH_LNE(LINE1(J), ' ') .AND.  CH_LNE(LINE1(J), CONVERT(0))
   50     13      6           b2 = b2 + 1
   53     15     32           SPEC(1 + I) = LINE1(J)
   51     19      6           I = I + 1
   54     20      6           J = J + 1
                           END DO
   50      *      4        r2 = b2


No directives were found.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 130

 Line   Stmt   Time        Program

  130     73      4        DO cs30=1, 5
  131     74     29           IF ( CH_LEQ(C, TYPES(cs30))) THEN
  132     75      4              KTYP = cs30
  133     76     15              LL = LENGS(cs30)
  134     77     15              L = LL/2
  135     78      3              GO TO 80
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 143

 Line   Stmt   Time        Program

  143     86      0        DO cs31=1, NUMC
  146     88      0  50       IF ((LL + IBYT) .GT. IGOT) THEN
  147     89      0              IF (IBYT .NE. 0) THEN
  147     90      0                 CALL BFFO(LUOUT, 1, IBUF, IBYT, J, 0)
                                 END IF
  148     91      0              l2 = 1 + IWRD
  148     93      0              l2 = l2
  148     95      0              b2 = MAX(1 + IWGOT - l2, 0)
  148     96      0              DO i2=l2, IWGOT
                                    (...)
                                 END DO
  152    102      0              IREM = IWGOT - IWRD
  153    103      0              NWORD = 1 + (IREM + 1)/2
  154    104      0              CALL BFFI(LUIN, 1, IBUF(NWORD), 8192, J, MREAD, 0)
  158    105      0              IF (J .EQ. 3 .AND. MREAD .NE. 0) THEN
  158    106      0                 PAUSE 'unexpected mread'
                                 END IF
  159    107      0              IF (J .NE. 2 .AND. J .NE. 3) THEN
  159    108      0                 PAUSE 'unexpected status from bffi'
                                 END IF
  160    109      0              ISHF = (NWORD - 1)*2 - IREM
  162    110      0              IF (ISHF .NE. 1 .AND. ISHF .NE. 0) THEN
  162    111      0                 PAUSE 'unexpected ishf'
                                 END IF
  163    112      0              IF (ISHF .NE. 0) THEN
  164    113      0                 KOF = IREM + ISHF
  165    116      0                 b2 = MAX(MREAD/2, 0)
  165    117      0                 DO i2=1, MREAD/2
                                       (...)
                                    END DO
                                 END IF
  170    124      0              IWGOT = MREAD/2 + IREM
  171    125      0              IGOT = 2*IWGOT
  172    126      0              IBYT = 0
  173    127      0              IWRD = 0
  174    128      0              IF (MREAD .EQ. 0) THEN
  174    129      0                 GO TO 99
                                 END IF
  175    130      0              GO TO 50
                              END IF
  178    133      0           b2 = MAX(L, 0)
  178    134      0           DO i2=1, L
                                 (...)
                              END DO
  182    140      0           cgoto_temp_28 = INT(KTYP)
  182    141      0           GOTO (1, 2, 3, 4, 5) cgoto_temp_28
  183    142      0           PAUSE 'ktyp out of range'
  184    144      0  1        GO TO 200
  185    146      0  2        GO TO 200
  186    148      0  3        GO TO 200
  187    150      0  4        FTEMP(1) = FLDG(ITEMP(1))
  188    151      0           GO TO 200
  189    153      0  5        DTEMP = DBLE(DFLDG(ITEMP(1)))
  192    157      0  200      b2 = MAX(L, 0)
  192    158      0           DO i2=1, L
                                 (...)
                              END DO
  196    164      0           IBYT = IBYT + LL
  197    165      0           IWRD = IWRD + L
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 4 function calls.
   2) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 148

 Line   Stmt   Time        Program

  148      *      6        DO i2=l2, IWGOT
  149     97     29           JBUF(i2 - IWRD) = JBUF(i2)
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 165

 Line   Stmt   Time        Program

  165      *     16        DO i2=1, MREAD/2
  166    118     32           JBUF(IREM + i2) = JBUF(KOF + i2)
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 178

 Line   Stmt   Time        Program

  178      *      5        DO i2=1, L
  179    135     29           JTEMP(i2) = JBUF(IWRD + i2)
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File import.f
                    Origin -- Line 192

 Line   Stmt   Time        Program

  192      *      5        DO i2=1, L
  193    159     29           JBUF(IWRD + i2) = JTEMP(i2)
                           END DO


No directives were found.

