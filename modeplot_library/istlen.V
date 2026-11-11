             Vectorized Results From File istlen.f
                    Origin -- Line 9

 Line   Stmt   Time        Program

    9      6      5        DO cs6=1, K
   10      7      9           J = K + 1 - cs6
   11      8     64           
                          $        IF ( CH_LEQ(STRING(INT(J):INT(J)), ' ') .OR.  CH_LEQ(STRING(INT(J):                           INT(J)), CONVERT(0))) THEN
   11      9      3              GO TO 10
                              END IF
   12     10      4           Return_Type_5 = J
   13     11      3           GO TO 99
   14     12      0  10       CONTINUE 
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 2 function calls.
   2) This loop contains 1 branches that exit the loop.

