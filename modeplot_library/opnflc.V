             Vectorized Results From File opnflc.f
                    Origin -- Line 36

 Line   Stmt   Time        Program

   36      *      4        DO iv=1, 40, 32
   36      *      8           rv = MIN(40, 31 + iv)
    *      *      9           vl = rv - iv + 1
   36      *      6           DO VECTOR i1=iv, rv
   37     26     15              JCHN(i1) = 0
                              END DO
                           END DO


No directives were found.

--------------------------------------
             Vectorized Results From File opnflc.f
                    Origin -- Line 41

 Line   Stmt   Time        Program

   41     36      4        DO cs38=21, 40
   42     37     19           IF (JCHN(cs38) .EQ. 0) THEN
   43     38      7              LUFL = cs38
   43     39      7              LUFL = LUFL
   44     40      3              GO TO 71
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 branches that exit the loop.

--------------------------------------
             Vectorized Results From File opnflc.f
                    Origin -- Line 102

 Line   Stmt   Time        Program

  102     93      4        DO i1=1, 2
  103     94     25           IF (i1 .NE. IDEV .AND. ITCHAN(i1) .GE. 0) THEN
  104     95     24              CALL CCLOSE(ITCHAN(i1), IRES, IERRNO)
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 1 function calls.

