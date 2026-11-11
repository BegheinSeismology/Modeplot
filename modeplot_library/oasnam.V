             Vectorized Results From File oasnam.f
                    Origin -- Line 12

 Line   Stmt   Time        Program

   12      8      8        DO I=1, LEN(STRING)
   13      9     18           CC = STRING(I:I)
   14     11     16           IF ( CH_LEQ(CC, '"')) THEN
   15     12      4              WRITE( )
   17     13      6              CALL EXIT(2)
                              END IF
   20     15     94           IF ( CH_LEQ(CC, ' ') .OR.  CH_LEQ(CC, ':') .OR.  CH_LEQ(CC, '/') .OR.  CH_LEQ                           (CC, '[') .OR.  CH_LEQ(CC, '[') .OR.  CH_LEQ(CC, '>') .OR. 
                          $         CH_LEQ(CC, '<')) THEN
   23     17      7              IF (.NOT. QUOTE) THEN
   24     18      6                 J = J + 1
   27     19     18                 return_OASNAM(INT(J):INT(J)) = '"'
   28     20      3                 QUOTE = 1
                                 END IF
                              ELSE
   35     22      6              IF (QUOTE) THEN
   36     23      6                 J = J + 1
   38     24     18                 return_OASNAM(INT(J):INT(J)) = '"'
   39     25      3                 QUOTE = 0
                                 END IF
                              END IF
   45     26      6           J = J + 1
   49     27     20           return_OASNAM(INT(J):INT(J)) = CC
                           END DO


Loop was not analyzed for the following reasons:

   1) This loop contains 9 function calls.

