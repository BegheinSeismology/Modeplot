             Vectorized Results From File oposn.f
                    Origin -- Line 18

 Line   Stmt   Time        Program

   18     16      4        DO I=9, 14
   19     18     28           IF ( CH_LEQ(COMMAND(INT(I):INT(I)), ' ')) THEN
   19     19     18              COMMAND(INT(I):INT(I)) = '0'
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

