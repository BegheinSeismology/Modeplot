             Vectorized Results From File lufil.f
                    Origin -- Line 5

 Line   Stmt   Time        Program

    5      6      4        DO I=1, 40
    6      8     44           IF ( CH_LEQ(NAME, OPNNAM(I)) .AND. JCHN(I) .GT. 0) THEN
    7      9      4              Return_Type_5 = I
    8     10      3              GO TO 10
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.
   2) This loop contains 1 branches that exit the loop.

