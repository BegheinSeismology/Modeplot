             Vectorized Results From File strip.f
                    Origin -- Line 9

 Line   Stmt   Time        Program

    9      4      4        DO WHILE LMESS .GT. 0
    9      5      6           b1 = b1 + 1
   10      6    114           
                          $        IF ( CH_LNE(MESS(INT(LMESS):INT(LMESS)), '>') .AND.  CH_LNE(MESS(INT(                           LMESS):INT(LMESS)), '
') .AND.  CH_LNE(MESS(INT(LMESS):INT(LMESS))
                          $        , CONVERT(0)) .AND.  CH_LNE(MESS(INT(LMESS):INT(LMESS)), ' ')) THEN                           
   10      7      3              GO TO 10
                              END IF
   13      8      9           LMESS = LMESS - 1
   13      9      7           LMESS = LMESS
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 4 function calls.
   2) This loop contains 1 branches that exit the loop.

