             Vectorized Results From File oreset.f
                    Origin -- Line 5

 Line   Stmt   Time        Program

    5      6      4        DO i1=1, 2
    6      7     19           IF (ITCHAN(i1) .GE. -1) THEN
    7      8     14              WRITE( )i1 - 1
    8      9     23              CALL OASSEND('user/def=' // STR1, MESS, LMESS, -1)
    9     10     17              CALL OASSEND('offline', MESS, LMESS, -1)
   10     11     17              CALL OASSEND('dismount', MESS, LMESS, -1)
   11     12     17              CALL OASSEND('dismount/vol', MESS, LMESS, -1)
                              END IF
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 4 function calls.

