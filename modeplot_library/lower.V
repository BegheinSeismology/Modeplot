             Vectorized Results From File lower.f
                    Origin -- Line 7

 Line   Stmt   Time        Program

    7      6      4        r1 = LL
   10     11      0  10    CONTINUE 
    7      *      5        DO I=1, r1
    8      7     18           IC = INT(CONVERT(STRING(I:I)))
    *      9     11           br1 = IC .GE. 65 .AND. IC .LE. 90
    9     10      6           IF (br1) IC = IC + 32
   10     12     30           STRING(INT(I):INT(I)) = CONVERT(IC)
                           END DO


No directives were found.

