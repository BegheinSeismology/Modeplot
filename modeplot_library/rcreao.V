             Vectorized Results From File rcreao.f
                    Origin -- Line 35

 Line   Stmt   Time        Program

   35     25     29        DO WHILE IP2 .GT. 0 .AND.  CH_LNE(PATH(INT(IP2):INT(IP2)), '/')
   35     26      6           b1 = b1 + 1
   36     30      6           IP2 = IP2 - 1
                           END DO
   35      *      4        r1 = b1


No directives were found.

--------------------------------------
             Vectorized Results From File rcreao.f
                    Origin -- Line 48

 Line   Stmt   Time        Program

   48     43     24        DO WHILE  CH_LNE(PATH(INT(IP2):INT(IP2)), '/')
   48     44      6           b1 = b1 + 1
   49     48      6           IP2 = 1 + IP2
                           END DO
   48      *      4        r1 = b1


No directives were found.

