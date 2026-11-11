             Vectorized Results From File bffis.f
                    Origin -- Line 12

 Line   Stmt   Time        Program

   12     12      4        DO WHILE KNT .NE. 0
   12     13      6           b1 = b1 + 1
   13     14      6           IDO = MIN(KNT, 32000)
   14     15     29           CALL CMTIO(JCHN(LUFL), 4, IDO, IRES, IERRNO)
   15     16      7           KNT = KNT - IDO
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

--------------------------------------
             Vectorized Results From File bffis.f
                    Origin -- Line 19

 Line   Stmt   Time        Program

   19     21      4        DO WHILE KNT .NE. 0
   19     22      6           b1 = b1 + 1
   20     23      6           IDO = MIN(KNT, 32000)
   21     24     29           CALL CMTIO(JCHN(LUFL), 3, IDO, IRES, IERRNO)
   22     25      7           KNT = KNT - IDO
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

