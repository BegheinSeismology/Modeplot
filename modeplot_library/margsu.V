             Vectorized Results From File margsu.f
                    Origin -- Line 14

 Line   Stmt   Time        Program

   14     11      5        DO i1=1, LSTRING
   15     12     18           C = STRING(i1:i1)
   16     13     16           IF ( CH_LEQ(C, ''')) THEN
   17     14      5              QUOTED = .NOT. QUOTED
   18     15      3              GO TO 100
                              END IF
   20     16      5           cgoto_temp_6 = INT(MODE)
   20     17      8           GOTO (1, 2) cgoto_temp_6
   22     19     19  1        IF ( CH_LNE(C, ' ') .OR. QUOTED) THEN
   23     20     28              NUMALI(ILEV) = NUMALI(ILEV) + 1
   24     21      6              IP = IP + 1
   25     22     31              LINELI(ILEV)(INT(IP):INT(IP)) = C
   26     23     37              IPTLI(NUMALI(ILEV), ILEV) = IP
   27     24     41              IPTLI(1 + NUMALI(ILEV), ILEV) = IP + 1
   28     25      3              MODE = 2
                              END IF
   30     26      3           GO TO 100
   32     28     19  2        IF ( CH_LNE(C, ' ') .OR. QUOTED) THEN
   33     29      6              IP = IP + 1
   34     30     31              LINELI(ILEV)(INT(IP):INT(IP)) = C
   35     31     41              IPTLI(1 + NUMALI(ILEV), ILEV) = IP + 1
                              ELSE
   37     32      3              MODE = 1
                              END IF
   40     33      0  100      CONTINUE 
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 3 function calls.

