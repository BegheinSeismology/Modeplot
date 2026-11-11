             Vectorized Results From File iststa.f
                    Origin -- Line 8

 Line   Stmt   Time        Program

    8      7      5        DO I=1, K
    9      9     28           IF ( CH_LNE(STRING(INT(I):INT(I)), ' ')) THEN
    9     10      3              GO TO 10
                              END IF
   10     11      6           Return_Type_5 = 1 + Return_Type_5
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

