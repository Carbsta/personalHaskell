module MyTAMCode where


import TAMCode
import TAMInterpreter

-- reads a number n from the terminal and then prints the numbers
-- from 1 to n (in order) using a loop. Doesn't print if n < 1
myTAMCode3a = [LOADL 1, GETINT, LOAD (ST (-1)), LOADL 0, GTR, JUMPIFZ "end",
 Label "loop",
 LOAD (SB 0), PUTINT, LOAD (SB 0), LOADL 1, ADD, STORE (SB 0),
 LOAD (SB 0), LOAD (SB 1), LSS, JUMPIFNZ "loop",
 LOADL 1,
 EQL,
 JUMPIFNZ "end",
 PUTINT,
 Label "end"]

-- reads a number n from the terminal, returns 1 for any argument n <= 0,
-- otherwise calculates the factorial, needs to use a recusrive function
-- and function is pure: no side effects on global variables.
myTAMCode3b = [GETINT, LOAD (SB 0), CALL "fac", PUTINT, HALT,
              Label "fac",
              LOAD (LB (-1)),
              LOADL 2,
              LSS,
              JUMPIFZ "recursiveCase",
              Label "baseCase",
              LOADL 1,
              RETURN 1 1,
              Label "recursiveCase",
              LOAD (LB (-1)),
              LOAD (LB (-1)),
              LOADL 1,
              SUB,
              CALL "fac",
              MUL,
              RETURN 1 1]

-- getchr returns to a variable passed by reference
getchartest = [LOADL 0, LOADL 0, CALL "getchr", PUTCHR, HALT,
               Label "getchr",
               GETCHR,
               LOAD (LB (-1)),
               STOREI 0,
               RETURN 0 1]

-- putchr prints a var passed by value.
putchartest = [LOADL 97, CALL "putchr", HALT,
               Label "putchr",
               LOAD (LB (-1)),
               PUTCHR,
               RETURN 0 1]
