             Vectorized Results From File chekgl.f
                    Origin -- Line 30

 Line   Stmt   Time        Program

   30     14      0        DO cs64=1, LL
   31     15      0           C = STRING(cs64:cs64)
   32     16      0           IF ( CH_LEQ(C, '|')) THEN
   33     19      0              b2 = MAX(NSWT, 0)
   33     20      0              DO i2=1, NSWT
                                    (...)
                                 END DO
   39     26      0              NSWT = 1 + NSWT
   39     27      0              NSWT = NSWT
   40     28      0              IF (NSWT .GT. 0) THEN
   41     29      0                 SWNAM(NSWT) = SWT
   42     30      0                 LSWNAM(NSWT) = LSWT
   43     31      0                 IFREQ(NSWT) = IFRQ
   44     32      0                 NMIN(NSWT) = NMN
   45     33      0                 NMAX(NSWT) = NMX
   46     34      0                 MESS(NSWT) = MSS
   47     35      0                 MLEN(NSWT) = MLN
   49     36      0                 LDEF = -1
   50     37      0                 IF (IFRQ .EQ. 0) THEN
   51     38      0                    MEE = MSS + MLN
    *     39      0                    b2 = 0
   52     40      0                    DO WHILE MSS .LT. MEE .AND.  CH_LNE(STRING(INT(MSS):INT(MSS)),                            '[')
                                          (...)
                                       END DO
   56     46      0                    IF (MSS .LE. MEE) THEN
   56     47      0                       LDEF = 0
                                       END IF
   57     48      0                    MSS = MSS + 1
    *     49      0                    b2 = 0
   58     50      0                    DO WHILE 1
                                          (...)
                                       END DO
   67     58      0  77                CONTINUE 
                                    END IF
   70     59      0                 LDEFLT(NSWT) = LDEF
                                 END IF
   72     60      0              SWT = CONVERT(0)
   73     61      0              LSWT = 0
   74     62      0              IFRQ = -1
   75     63      0              NMN = 0
   76     64      0              NMX = 0
   77     65      0              MODE = 0
   78     66      0              MODE1 = 0
   79     67      0              NUM = 0
   81     68      0              MSS = -1
   82     69      0              MLN = 0
                              ELSE
   83     70      0              IF ( CH_LEQ(C, ':')) THEN
   84     71      0                 MODE = MODE + 1
                                 ELSE
   85     72      0                 IF ( CH_LEQ(C, ',') .AND. MODE .NE. 3) THEN
   86     73      0                    IF (MODE .NE. 2) THEN
   87     74      0                       WRITE( )'chekcl: format error 1'
   88     75      0                       CALL EXIT(2)
                                       END IF
   90     76      0                    NUM = 0
   91     77      0                    MODE1 = 1
                                    ELSE
   92     78      0                    IF ( CH_LEQ(C, ' ') .AND. MODE .NE. 3) THEN
                                       ELSE
   94     79      0                       IF (MODE .EQ. 0) THEN
   95     80      0                          LSWT = 1 + LSWT
   96     81      0                          SWT(INT(LSWT):INT(LSWT)) = C
                                          ELSE
   97     82      0                          IF (MODE .EQ. 1) THEN
   98     83      0                             IF ( CH_LEQ(C, 'r')) THEN
   99     84      0                                IFRQ = 1
                                                ELSE
  100     85      0                                IF ( CH_LEQ(C, 'o')) THEN
  101     86      0                                   IFRQ = 0
                                                   ELSE
  103     87      0                                   WRITE( )'chekcl: format error 2'
  104     88      0                                   CALL EXIT(2)
                                                   END IF
                                                END IF
                                             ELSE
  106     89      0                             IF (MODE .EQ. 2) THEN
  107     90      0                                IC = INT(CONVERT(C))
  108     91      0                                IF (IC .LT. 48 .OR. IC .GT. 57) THEN
  109     92      0                                   IF ( CH_LEQ(C, '*')) THEN
  110     93      0                                      NUM = NBIG
                                                      ELSE
  112     94      0                                      IERR = 1
  112     95      0                                      IERR = IERR
  113     96      0                                      GO TO 52
                                                      END IF
                                                   ELSE
  116     97      0                                   NUM = NUM*10 + (IC - 48)
                                                   END IF
  118     98      0                                IF (MODE1 .EQ. 0) THEN
  119     99      0                                   NMN = NUM
  120    100      0                                   NMX = NUM
  121    101      0                                   IF (NUM .EQ. NBIG) THEN
  122    102      0                                      IF (IFRQ .EQ. 1) THEN
  123    103      0                                         NMN = 1
                                                         ELSE
  125    104      0                                         NMN = 0
                                                         END IF
                                                      END IF
                                                   ELSE
  129    105      0                                   NMX = NUM
                                                   END IF
                                                ELSE
  131    106      0                                IF (MODE .EQ. 3) THEN
  132    107      0                                   MLN = 1 + MLN
  133    108      0                                   IF (MSS .LT. 0) THEN
  133    109      0                                      MSS = cs64
                                                      END IF
                                                   ELSE
  135    110      0                                   IERR = 1
  135    111      0                                   IERR = IERR
  136    112      0                                   GO TO 52
                                                   END IF
                                                END IF
                                             END IF
                                          END IF
                                       END IF
                                    END IF
                                 END IF
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 12 function calls.
   2) This loop contains 3 branches that exit the loop.
   3) This loop contains IO statements and vectorizable inner loops.
      Use PBEST/VBEST to override.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 33

 Line   Stmt   Time        Program

   33     20      5        DO i2=1, NSWT
   34     21     29           IF ( CH_LEQ(SWT, SWNAM(i2))) THEN
   35     22      6              IERR = 5
   35     23      7              IERR = IERR
   36     24      3              GO TO 52
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 52

 Line   Stmt   Time        Program

   52      *     30        DO WHILE MSS .LT. MEE .AND.  CH_LNE(STRING(INT(MSS):INT(MSS)), '[')
   52     41      6           b2 = b2 + 1
   53     45      6           MSS = MSS + 1
                           END DO
   52      *      4        r2 = b2


No directives were found.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 58

 Line   Stmt   Time        Program

   58     50      1        DO WHILE 1
   58     51      6           b2 = b2 + 1
   59     52     34           IF ( CH_LNE(STRING(INT(MSS):INT(MSS)), ']') .AND. MSS .LT. MEE) THEN
   60     53      6              LDEF = LDEF + 1
   61     54     41              DEFLT(NSWT)(INT(LDEF):INT(LDEF)) = STRING(MSS:MSS)
   62     55      6              MSS = MSS + 1
                              ELSE
   64     56      3              GO TO 77
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 143

 Line   Stmt   Time        Program

  143      *      5        DO iv=1, NSWT, 32
  143      *      9           rv = MIN(NSWT, 31 + iv)
    *      *      9           vl = rv - iv + 1
  143      *      6           DO VECTOR i1=iv, rv
  144    121     15              ICNT(i1) = -1
                              END DO
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 161

 Line   Stmt   Time        Program

  161      *     30        DO WHILE  CH_LEQ(CLINE(INT(K):INT(K)), ' ') .AND. K .GE. KLEN
  161    139      6           b1 = b1 + 1
  162    143      6           K = K - 1
                           END DO
  161      *      4        r1 = b1


No directives were found.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 167

 Line   Stmt   Time        Program

  167    151      0        DO cs65=1, NARG
  168    152      0           KLEN = KLEN + 1
  169    153      0           IP1 = KLEN
  170    154      0           CALL GTARGF(cs65, CLINE(INT(KLEN):INT(LLEN)))
  171    155      0           K = LLEN
    *    156      0           b2 = 0
  172    157      0           DO WHILE  CH_LEQ(CLINE(INT(K):INT(K)), ' ') .AND. K .GE. KLEN
                                 (...)
                              END DO
  175    163      0           IF ( CH_LEQ(CLINE(INT(KLEN):INT(K)), '!') .AND. cs65 .EQ. 1) THEN
  176    164      0              IERR = 6
  176    165      0              IERR = IERR
  177    166      0              GO TO 52
                              END IF
  179    167      0           IP2 = K
  180    168      0           KLEN = K + 1
  181    169      0           IF (KLEN .GT. LLEN) THEN
  181    170      0              PAUSE 'chekcl: command line too long'
                              END IF
  182    171      0           LN = IP2 - IP1 + 1
  183    172      0           SWTEST = CLINE(IP1:IP2)
  184    173      0           IBREAK = 0
  186    175      0  67       IF (IBREAK .NE. 0) THEN
  187    176      0              SWTEST = '-' // CLINE(IP1 + IBREAK:IP1 + IBREAK)
  188    177      0              LN = 2
  191    178      0              IBREAK = 1 + IBREAK
                              END IF
  193    179      0           IMATCH = 0
  194    180      0           ISWT = 0
  195    183      0           b2 = MAX(NSWT, 0)
  195    184      0           DO i2=1, NSWT
                                 (...)
                              END DO
  207    196      0  22       IF (IMATCH .GT. 1) THEN
  208    197      0              IERR = 2
  208    198      0              IERR = IERR
  209    199      0              GO TO 52
                              ELSE
  210    200      0              IF (IMATCH .EQ. 1) THEN
  211    201      0                 IF (NMAX(ISWT) .GT. 0) THEN
  212    202      0                    IOPLV = 1 + IOPLV
  213    203      0                    IOPN(IOPLV) = ISWT
                                    END IF
  216    204      0                 ICNT(ISWT) = 0
                                 ELSE
  222    205      0                 IF ( CH_LEQ(CLINE(INT(IP1):INT(IP1)), '-') .AND. IP2 .GT. (1 + IP1                           ) .AND. IBREAK .EQ. 0) THEN
  224    206      0                    IBREAK = 1
  225    207      0                    GO TO 67
                                    END IF
  229    208      0                 IF (IOPLV .LE. 0) THEN
  230    209      0                    IERR = 1
  230    210      0                    IERR = IERR
  231    211      0                    GO TO 52
                                    END IF
  234    212      0                 KENT = KENT + 1
  235    213      0                 ISWT = IOPN(IOPLV)
  236    214      0                 IF (ICNT(ISWT) .EQ. 0) THEN
  237    215      0                    IPTR(ISWT) = KENT
                                    END IF
  240    216      0                 IPT = IPTR(ISWT)
  242    217      0                 IPTL = KENT
  243    220      0                 b2 = MAX(ICNT(ISWT), 0)
  243    221      0                 DO i2=1, ICNT(ISWT)
                                       (...)
                                    END DO
  248    230      0                 IF (IPTR(ISWT) .NE. IPT) THEN
  248    231      0                    PAUSE 'circle does not close'
                                    END IF
  249    232      0                 ITABLE(KENT, 1) = IP1
  250    233      0                 ITABLE(KENT, 2) = IP2
  251    234      0                 ITABLE(KENT, 3) = IPTR(ISWT)
  252    235      0                 ITABLE(IPTL, 3) = KENT
  253    236      0                 ICNT(ISWT) = 1 + ICNT(ISWT)
  254    237      0                 IF (ICNT(ISWT) .EQ. NMAX(ISWT)) THEN
  255    238      0                    IOPLV = IOPLV - 1
                                    END IF
                                 END IF
                              END IF
  258    239      0           IF (IBREAK .LE. (IP2 - IP1) .AND. IBREAK .NE. 0) THEN
  258    240      0              GO TO 67
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 5 function calls.
   2) This loop contains 3 branches that exit the loop.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 172

 Line   Stmt   Time        Program

  172      *     30        DO WHILE  CH_LEQ(CLINE(INT(K):INT(K)), ' ') .AND. K .GE. KLEN
  172    158      6           b2 = b2 + 1
  173    162      6           K = K - 1
                           END DO
  172      *      4        r2 = b2


No directives were found.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 195

 Line   Stmt   Time        Program

  195    184      5        DO i2=1, NSWT
  196    185     51           IF ( CH_LEQ(SWTEST(INT(1):INT(LN)), SWNAM(i2)(INT(1):INT(LN)))) THEN
  197    186      8              IF (ISWT .EQ. 0) THEN
  197    187      4                 ISWT = i2
                                 END IF
  198    188      6              IMATCH = 1 + IMATCH
  199    189     20              IF (LSWNAM(i2) .EQ. LN) THEN
  200    190      4                 ISWT = i2
  201    191      3                 IMATCH = 1
  202    192      3                 GO TO 22
                                 END IF
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 243

 Line   Stmt   Time        Program

  243      *     13        DO i2=1, ICNT(ISWT)
  244    227      4           IPTL = IPT
  245    228     29           IPT = ITABLE(IPT, 3)
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 267

 Line   Stmt   Time        Program

  267      *      5        DO i1=1, NSWT
    *    251     33           br3 = IFREQ(i1) .NE. 0 .AND. ICNT(i1) .LT. 0
  268    252      3           IF (br3) IERR = 3
    *    255     74           br4 = (ICNT(i1) .LT. NMIN(i1) .OR. ICNT(i1) .GT. NMAX(i1)
                          $        ) .AND. ICNT(i1) .GE. 0
  271    256      3           IF (br4) IERR = 4
  272    258     45           IF (br4) PRINT( )i1, ICNT(i1), NMIN(i1), NMAX(i1)
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 294

 Line   Stmt   Time        Program

  294    293      0        DO i1=1, NSWT
  295    294      0           IF (IFREQ(i1) .NE. 0) THEN
  296    295      0              STR1 = 'required'
                              ELSE
  298    296      0              STR1 = 'optional'
                              END IF
  300    297      0           IF (NMAX(i1) .EQ. NBIG) THEN
  301    298      0              WRITE( )NMIN(i1)
                              ELSE
  302    299      0              IF (NMAX(i1) .EQ. NMIN(i1) .AND. NMAX(i1) .EQ. 1) THEN
  303    300      0                 WRITE( )NMAX(i1)
                                 ELSE
  304    301      0                 IF (NMAX(i1) .EQ. NMIN(i1)) THEN
  305    302      0                    WRITE( )NMAX(i1)
                                    ELSE
  306    303      0                    IF (NMAX(i1) .EQ. 1) THEN
  307    304      0                       WRITE( )NMIN(i1), NMAX(i1)
                                       ELSE
  309    305      0                       WRITE( )NMIN(i1), NMAX(i1)
                                       END IF
                                    END IF
                                 END IF
                              END IF
  311    306      0           IF (LSWNAM(i1) .EQ. 0) THEN
  312    307      0              STR3 = ' '
                              ELSE
  314    308      0              STR3 = SWNAM(i1)(1:LSWNAM(i1))
                              END IF
  317    309      0           M10 = MESS(i1)
  318    310      0           M1 = M10
  320    311      0           MM = MLEN(i1) + M1 - 1
    *    312      0           b2 = 0
  321    313      0           DO WHILE M1 .LT. MM
                                 (...)
                              END DO
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 2 function calls.
   2) This loop contains IO statements and vectorizable inner loops.
      Use PBEST/VBEST to override.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 321

 Line   Stmt   Time        Program

  321    313      0        DO WHILE M1 .LT. MM
  321    314      0           b2 = b2 + 1
  322    315      0           M20 = MIN(MM, M1 + 35)
  323    316      0           M2 = M20
    *    317      0           b3 = 0
  324    318      0           DO WHILE M2 .LT. MM .AND. M2 .GT. (M1 + 20) .AND. 
                          $         CH_LNE(STRING(INT(M2):INT(M2)), ' ')
                                 (...)
                              END DO
  328    324      0           IF ( CH_LNE(STRING(INT(M2):INT(M2)), ' ')) THEN
  328    325      0              M2 = M20
                              END IF
  329    326      0           IF (M1 .EQ. M10) THEN
  330    327      0              WRITE( )STR3, STR2, STR1, (STRING(INT(J):INT(J)), J=M1, M2
                              ELSE
  337    328      0              WRITE( )(STRING(INT(J):INT(J)), J=M1, M2
                              END IF
  339    329      0           M1 = M2 + 1
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 2 function calls.
   2) This loop contains IO statements and vectorizable inner loops.
      Use PBEST/VBEST to override.

--------------------------------------
             Vectorized Results From File chekgl.f
                    Origin -- Line 324

 Line   Stmt   Time        Program

  324      *     38        DO WHILE M2 .LT. MM .AND. M2 .GT. (M1 + 20) .AND.  CH_LNE(STRING(INT(M2)
                          $     :INT(M2)), ' ')
  324    319      6           b3 = b3 + 1
  325    323      6           M2 = M2 - 1
                           END DO
  324      *      4        r3 = b3


No directives were found.

