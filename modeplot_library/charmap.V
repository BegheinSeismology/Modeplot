             Vectorized Results From File charmap.f
                    Origin -- Line 8

 Line   Stmt   Time        Program

    8     12      5        DO I=1, NCHAR
    9     14     28           IF ( CH_LEQ(return_CHARMAP(INT(I):INT(I)), ' ')) THEN
    9     15     18              return_CHARMAP(INT(I):INT(I)) = '0'
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

--------------------------------------
             Vectorized Results From File charmap.f
                    Origin -- Line 15

 Line   Stmt   Time        Program

   15     26      5        DO I=1, NCHAR
   16     28     28           IF ( CH_LEQ(return_CHARMAP(INT(I):INT(I)), ' ')) THEN
   16     29     18              return_CHARMAP(INT(I):INT(I)) = '0'
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

