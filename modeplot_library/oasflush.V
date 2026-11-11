             Vectorized Results From File oasflush.f
                    Origin -- Line 14

 Line   Stmt   Time        Program

   14      5      4        DO WHILE IERRNO .NE. 35
   14      6      6           b1 = b1 + 1
   15      7     20           CALL CREAD(ICHAN, CC, 1, IRES, IERRNO)
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

