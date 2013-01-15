//    MASTER

GET "LIBHDR"

GLOBAL {
    chbuf:100; prsource:110
    pptrace:127; option:128
    formtree:150; plist:152
    treep:167; treevec:168
    reportcount:191; reportmax:192
    sourcestream:193; sysprint:194; ocode:195; sysin:196
    compileae:245
    savespacesize:282
}

COMP(V, TREEMAX)
{
    int B = VEC 63;
    int A;
    chbuf := B

    for (;;) {
        treep, treevec := V+TREEMAX, V

        A = formtree()
        IF A=0 BREAK

        writef("*NTREE SIZE %N*N", TREEMAX+treevec-treep)

        IF option!2 DO {
            writes('AE TREE*N')
            plist(A, 0, 20)
            newline()
        }

        UNLESS reportcount=0 DO
            stop(8)

        UNLESS option!3 DO {
            selectoutput(ocode)
            compileae(A)
            selectoutput(sysprint)
        }
    }
}

start(parm)
{
    LET OPT = VEC 20
    AND TREESIZE = 5500

    sysin := input()
    sysprint := output()
    selectoutput(sysprint)

    writef("*NBCPL %N*N", @start)

    option := OPT
    savespacesize := 2
    pptrace := FALSE
    prsource := FALSE
    FOR I = 0 TO 20 DO
        OPT!I := FALSE

    sourcestream := findinput("OPTIONS")

    UNLESS sourcestream=0 DO {
        LET CH = 0
        AND N = 0
        selectinput(sourcestream)
        writes("OPTIONS  ")

        for (;;) {
            CH := rdch()
L:          IF CH='*N' \/ CH=ENDSTREAMCH
                BREAK
            wrch(CH)
            IF CH='P' DO N := 1
            IF CH='T' DO N := 2
            IF CH='C' DO N := 3
            IF CH='M' DO N := 4
            IF CH='N' DO N := 5
            IF CH='S' DO prsource := TRUE
            IF CH='E' DO pptrace := TRUE
            IF CH='L' DO {
                TREESIZE := readn()
                writen(TREESIZE)
                CH := terminator
                GOTO L
            }
            IF CH='3' DO savespacesize := 3
            option!N := TRUE
        }

        newline()
        endread()
    }

    reportmax := 20
    reportcount := 0

    sourcestream := sysin
    selectinput(sourcestream)

    ocode := findoutput("OCODE")
    IF ocode=0 DO
        ocode := sysprint

    {2

       aptovec(COMP, TREESIZE)

       endread()
       //IF option!4 DO mapstore()
       writes('*NPHASE 1 COMPLETE*N')
       UNLESS reportcount=0 DO stop(8)
       FINISH
    }2
}
.

//    LEX1


GET "SYNHDR"

LET NEXTSYMB() BE
{1   NLPENDING := FALSE

NEXT: IF pptrace DO wrch(CH)

    SWITCHON CH INTO

    { CASE '*P':
       CASE '*N': LINECOUNT := LINECOUNT + 1
                  NLPENDING := TRUE  // IGNORABLE CHARACTERS
       CASE '*T':
       CASE '*S': RCH() REPEATWHILE CH='*S'
                  GOTO NEXT

       CASE '0':CASE '1':CASE '2':CASE '3':CASE '4':
       CASE '5':CASE '6':CASE '7':CASE '8':CASE '9':
            SYMB := S.NUMBER
            readnumber(10)
            RETURN

       CASE 'A':CASE 'B':CASE 'C':CASE 'D':CASE 'E':
       CASE 'F':CASE 'G':CASE 'H':CASE 'I':CASE 'J':
       CASE 'K':CASE 'L':CASE 'M':CASE 'N':CASE 'O':
       CASE 'P':CASE 'Q':CASE 'R':CASE 'S':CASE 'T':
       CASE 'U':CASE 'V':CASE 'W':CASE 'X':CASE 'Y':
       CASE 'Z':
       CASE 'a':CASE 'b':CASE 'c':CASE 'd':CASE 'e':
       CASE 'f':CASE 'g':CASE 'h':CASE 'i':CASE 'j':
       CASE 'k':CASE 'l':CASE 'm':CASE 'n':CASE 'o':
       CASE 'p':CASE 'q':CASE 'r':CASE 's':CASE 't':
       CASE 'u':CASE 'v':CASE 'w':CASE 'x':CASE 'y':
       CASE 'z':
              RDTAG(CH)
              SYMB := LOOKUPWORD()
              IF SYMB=S.GET DO { PERFORMGET(); GOTO NEXT  }
              RETURN

       CASE '$': RCH()
                 UNLESS CH='(' \/ CH=')' DO CAEREPORT(91)
                 SYMB := CH='(' -> S.LSECT, S.RSECT
                 RDTAG('$')
                 LOOKUPWORD()
                 RETURN

       CASE '[':
       CASE '(': SYMB := S.LPAREN; GOTO L
       CASE ']':
       CASE ')': SYMB := S.RPAREN; GOTO L

       CASE '#': SYMB := S.NUMBER
                 RCH()
                 IF '0'<=CH<='7' DO { readnumber(8); RETURN  }
                 IF CH='B' DO { RCH(); readnumber(2); RETURN  }
                 IF CH='O' DO { RCH(); readnumber(8); RETURN  }
                 IF CH='X' DO { RCH(); readnumber(16); RETURN  }
                 CAEREPORT(33)

       CASE '?': SYMB := S.QUERY; GOTO L
       CASE '+': SYMB := S.PLUS; GOTO L
       CASE ',': SYMB := S.COMMA; GOTO L
       CASE ';': SYMB := S.SEMICOLON; GOTO L
       CASE '@': SYMB := S.LV; GOTO L
       CASE '&': SYMB := S.LOGAND; GOTO L
       CASE '=': SYMB := S.EQ; GOTO L
       CASE '!': SYMB := S.VECAP; GOTO L
       CASE '_': SYMB := S.ASS; GOTO L
       CASE '**': SYMB := S.MULT; GOTO L

       CASE '/': RCH()
                 IF CH='\' DO { SYMB := S.LOGAND; GOTO L }
                 IF CH='/' GOTO COMMENT
                 UNLESS CH='**' DO { SYMB := S.DIV; RETURN  }

                 RCH()

                 UNTIL CH=ENDSTREAMCH DO TEST CH='**'

                       THEN { RCH()
                               UNLESS CH='/' LOOP
                               RCH()
                               GOTO NEXT  }

                       OR { IF CH='*N' DO LINECOUNT := LINECOUNT+1
                             RCH()  }

                 CAEREPORT(63)


       COMMENT: RCH() REPEATUNTIL CH='*N' \/ CH=ENDSTREAMCH
                GOTO NEXT

       CASE '|': RCH()
                 IF CH='|' GOTO COMMENT
                 SYMB := S.LOGOR
                 RETURN

       CASE '\': RCH()
                 IF CH='/' DO { SYMB := S.LOGOR; GOTO L  }
                 IF CH='=' DO { SYMB := S.NE; GOTO L  }
                 SYMB := S.NOT
                 RETURN

       CASE '<': RCH()
                 IF CH='=' DO { SYMB := S.LE; GOTO L  }
                 IF CH='<' DO { SYMB := S.LSHIFT; GOTO L }
                 SYMB := S.LS
                 RETURN

       CASE '>': RCH()
                 IF CH='=' DO { SYMB := S.GE; GOTO L  }
                 IF CH='>' DO { SYMB := S.RSHIFT; GOTO L  }
                 SYMB := S.GR
                 RETURN

       CASE '-': RCH()
                 IF CH='>' DO { SYMB := S.COND; GOTO L  }
                 SYMB := S.MINUS
                 RETURN

       CASE ':': RCH()
                 IF CH='=' DO { SYMB := S.ASS; GOTO L  }
                 SYMB := S.COLON
                 RETURN

        CASE '*'':CASE '*"':
             {1 LET QUOTE = CH
                 CHARP := 0

              { RCH()
                 IF CH=QUOTE \/ CHARP=255 DO
                        { UNLESS CH=QUOTE DO CAEREPORT(95)
                           IF CHARP=1 & CH='*'' DO
                                   { SYMB := S.NUMBER
                                      GOTO L  }
                           CHARV!0 := CHARP
                           WORDSIZE := packstring(CHARV, WORDV)
                           SYMB := S.STRING
                           GOTO L   }


                 IF CH='*N' DO LINECOUNT := LINECOUNT + 1

                 IF CH='**' DO
                        { RCH()
                           IF CH='*N' DO
                               { LINECOUNT := LINECOUNT+1
                                  RCH() REPEATWHILE CH='*S' \/ CH='*T'
                                  UNLESS CH='**' DO CAEREPORT(34)
                                  LOOP  }
                           IF CH='T' DO CH := '*T'
                           IF CH='S' DO CH := '*S'
                           IF CH='N' DO CH := '*N'
                           IF CH='B' DO CH := '*B'
                           IF CH='P' DO CH := '*P'  }

                 DECVAL, CHARP := CH, CHARP+1
                 CHARV!CHARP := CH  } REPEAT  }1



       DEFAULT: IF CH=ENDSTREAMCH DO
       CASE '.':    { IF GETP=0 DO
                             { SYMB := S.END
                                RETURN   }

                       endread()
                       GETP := GETP - 3
                       sourcestream := GETV!GETP
                       selectinput(sourcestream)
                       LINECOUNT := GETV!(GETP+1)
                       CH := GETV!(GETP+2)
                       GOTO NEXT  }

                   CH := '*S'
                   CAEREPORT(94)
                   RCH()
                   GOTO NEXT

       L: RCH()   }1

AND readnumber(RADIX) BE
    { LET D = VALUE(CH)
       DECVAL := D
       IF D>=RADIX DO CAEREPORT(33)

       { RCH()
          D := VALUE(CH)
          IF D>=RADIX RETURN
          DECVAL := RADIX*DECVAL + D  } REPEAT
    }


AND VALUE(CH) = '0'<=CH<='9' -> CH-'0',
                'A'<=CH<='F' -> CH-'A'+10,
                100

.

//    LEX2


GET "SYNHDR"

LET D(S, ITEM) BE { unpackstring(S, CHARV)
                     WORDSIZE := packstring(CHARV, WORDV)
                     LOOKUPWORD()
                     WORDNODE!0 := ITEM  }

AND DECLSYSWORDS() BE
     { D("AND", S.AND)

        D("BE", S.BE)
        D("BREAK", S.BREAK)
        D("BY", S.BY)

        D("CASE", S.CASE)

        D("DO", S.DO)
        D("DEFAULT", S.DEFAULT)

        D("EQ", S.EQ)
        D("EQV", S.EQV)
        D("ELSE", S.OR)
        D("ENDCASE", S.ENDCASE)

        D("FALSE", S.FALSE)
        D("FOR", S.FOR)
        D("FINISH", S.FINISH)

        D("GOTO", S.GOTO)
        D("GE", S.GE)
        D("GR", S.GR)
        D("GLOBAL", S.GLOBAL)
        D("GET", S.GET)

        D("IF", S.IF)
        D("INTO", S.INTO)

        D("LET", S.LET)
        D("LV", S.LV)
        D("LE", S.LE)
        D("LS", S.LS)
        D("LOGOR", S.LOGOR)
        D("LOGAND", S.LOGAND)
        D("LOOP", S.LOOP)
        D("LSHIFT", S.LSHIFT)

        D("MANIFEST", S.MANIFEST)

        D("NE", S.NE)
        D("NOT", S.NOT)
        D("NEQV", S.NEQV)

        D("OR", S.OR)

        D("RESULTIS", S.RESULTIS)
        D("RETURN", S.RETURN)
        D("REM", S.REM)
        D("RSHIFT", S.RSHIFT)
        D("RV", S.RV)
        D("REPEAT", S.REPEAT)
        D("REPEATWHILE", S.REPEATWHILE)
        D("REPEATUNTIL", S.REPEATUNTIL)

        D("SWITCHON", S.SWITCHON)
        D("STATIC", S.STATIC)

        D("TO", S.TO)
        D("TEST", S.TEST)
        D("TRUE", S.TRUE)
        D("THEN", S.DO)
        D("TABLE", S.TABLE)

        D("UNTIL", S.UNTIL)
        D("UNLESS", S.UNLESS)

        D("VEC", S.VEC)
        D("VALOF", S.VALOF)

        D("WHILE", S.WHILE)

        D("$", 0); NULLTAG := WORDNODE  }

AND LOOKUPWORD() = VALOF

{1     LET HASHVAL = (WORDV!0+WORDV!WORDSIZE >> 1) REM NAMETABLESIZE
        LET M = @NAMETABLE!HASHVAL

  NEXT: WORDNODE := !M
        UNLESS WORDNODE=0 DO
             {2 FOR I = 0 TO WORDSIZE DO
                   IF WORDNODE!(I+2) NE WORDV!I DO
                   { M := WORDNODE+1
                      GOTO NEXT  }
                 RESULTIS WORDNODE!0  }2

        WORDNODE := NEWVEC(WORDSIZE+2)
        WORDNODE!0, WORDNODE!1 := S.NAME, NAMETABLE!HASHVAL
        FOR I = 0 TO WORDSIZE DO WORDNODE!(I+2) := WORDV!I
        NAMETABLE!HASHVAL := WORDNODE
        RESULTIS S.NAME
}1

.

//    LEX3


GET "SYNHDR"

LET RCH() BE
    { CH := rdch()

       IF prsource DO IF GETP=0 /\ CH NE ENDSTREAMCH DO
          { UNLESS LINECOUNT=PRLINE DO { writef("%I4  ", LINECOUNT)
                                           PRLINE := LINECOUNT  }
             wrch(CH)  }

       CHCOUNT := CHCOUNT + 1
       chbuf!(CHCOUNT&63) := CH  }

AND wrchbuf() BE
    { writes("*N...")
       FOR P = CHCOUNT-63 TO CHCOUNT DO
                { LET K = chbuf!(P&63)
                   UNLESS K=0 DO wrch(K)  }
       newline()  }


AND RDTAG(X) BE
    { CHARP, CHARV!1 := 1, X

        {  RCH()
            UNLESS 'A'<=CH<='Z' \/
                   'a'<=CH<='z' \/
                   '0'<=CH<='9' \/
                    CH='.' BREAK
            CHARP := CHARP+1
            CHARV!CHARP := CH  } REPEAT

       CHARV!0 := CHARP
       WORDSIZE := packstring(CHARV, WORDV)  }


AND PERFORMGET() BE
    { NEXTSYMB()
       UNLESS SYMB=S.STRING THEN CAEREPORT(97)

       IF option!5 RETURN

       GETV!GETP := sourcestream
       GETV!(GETP+1) := LINECOUNT
       GETV!(GETP+2) := CH
       GETP := GETP + 3
       LINECOUNT := 1
       sourcestream := findinput(WORDV)
       IF sourcestream=0 THEN
           sourcestream := findlibinput(WORDV)
       IF sourcestream=0 THEN CAEREPORT(96,WORDV)
       selectinput(sourcestream)
       RCH()   }

AND APPEND(D, S) BE
    { LET ND = getbyte(D, 0)
       AND NS = getbyte(S, 0)
       FOR I = 1 TO NS DO {
           ND := ND + 1
           putbyte(D, ND, getbyte(S, I)) }
       putbyte(D, 0, ND) }

AND findlibinput(NAME) = VALOF
    { LET PATH = VEC 64
       AND DIR = "/usr/lib/bcpl/"
       TEST getbyte(DIR, 0) + getbyte(NAME, 0) > 255
       THEN RESULTIS 0
         OR { putbyte(PATH, 0, 0)
               APPEND(PATH, DIR)
               APPEND(PATH, NAME)
               RESULTIS findinput(PATH) }
    }


.

//    CAE0


GET "SYNHDR"

LET NEWVEC(N) = VALOF
    { treep := treep - N - 1
       IF treep<=treevec DO
                { reportmax := 0
                   CAEREPORT(98)  }
        RESULTIS treep  }

AND LIST1(X) = VALOF
    { LET P = NEWVEC(0)
       P!0 := X
       RESULTIS P  }

AND LIST2(X, Y) = VALOF
     { LET P = NEWVEC(1)
        P!0, P!1 := X, Y
        RESULTIS P   }

AND LIST3(X, Y, Z) = VALOF
     { LET P = NEWVEC(2)
        P!0, P!1, P!2 := X, Y, Z
        RESULTIS P     }

AND LIST4(X, Y, Z, T) = VALOF
     { LET P = NEWVEC(3)
        P!0, P!1, P!2, P!3 := X, Y, Z, T
        RESULTIS P   }

AND LIST5(X, Y, Z, T, U) = VALOF
     { LET P = NEWVEC(4)
        P!0, P!1, P!2, P!3, P!4 := X, Y, Z, T, U
        RESULTIS P   }

AND LIST6(X, Y, Z, T, U, V) = VALOF
     { LET P = NEWVEC(5)
        P!0, P!1, P!2, P!3, P!4, P!5 := X, Y, Z, T, U, V
        RESULTIS P  }

AND CAEREPORT(N, A) BE
     { reportcount := reportcount + 1
        writef("*NSYNTAX ERROR NEAR LINE %N:  ", LINECOUNT)
        CAEMESSAGE(N, A)
        wrchbuf()
        IF reportcount GR reportmax DO
                    { writes('*NCOMPILATION ABORTED*N')
                       stop(8)   }
        NLPENDING := FALSE

        UNTIL SYMB=S.LSECT \/ SYMB=S.RSECT \/
              SYMB=S.LET \/ SYMB=S.AND \/
              SYMB=S.END \/ NLPENDING DO NEXTSYMB()
        longjump(REC.P, REC.L)   }

AND formtree() =  VALOF
    {1 CHCOUNT := 0
        FOR I = 0 TO 63 DO chbuf!I := 0

     { LET V = VEC 10   // FOR 'GET' STREAMS
        GETV, GETP, GETT := V, 0, 10

     { LET V = VEC 100
        WORDV := V

     { LET V = VEC 256
        CHARV, CHARP := V, 0

     { LET V = VEC 100
        NAMETABLE, NAMETABLESIZE := V, 100
        FOR I = 0 TO 100 DO NAMETABLE!I := 0

        REC.P, REC.L := level(), L

        LINECOUNT, PRLINE := 1, 0
        RCH()

        IF CH=ENDSTREAMCH RESULTIS 0
        DECLSYSWORDS()

     L: NEXTSYMB()

        IF option!1 DO   //   PP DEBUGGING OPTION
             { writef("%N %S*N", SYMB, WORDV)
                IF SYMB=S.END RESULTIS 0
                GOTO L  }

     { LET A = RDBLOCKBODY()
        UNLESS SYMB=S.END DO { CAEREPORT(99); GOTO L  }

        RESULTIS A        }1



AND CAEMESSAGE(N, A) BE
    { LET S = VALOF

         SWITCHON N INTO

         { DEFAULT:  writen(N); RETURN

            CASE 91: RESULTIS "'8'  '(' OR ')' EXPECTED"
            CASE 94: RESULTIS "ILLEGAL CHARACTER"
            CASE 95: RESULTIS "STRING TOO LONG"
            CASE 96: RESULTIS "NO INPUT %S"
            CASE 97: RESULTIS "STRING OR NUMBER EXPECTED"
            CASE 98: RESULTIS "PROGRAM TOO LARGE"
            CASE 99: RESULTIS "INCORRECT TERMINATION"

            CASE 8:CASE 40:CASE 43:
                     RESULTIS "NAME EXPECTED"
            CASE 6: RESULTIS "'{' EXPECTED"
            CASE 7: RESULTIS "'}' EXPECTED"
            CASE 9: RESULTIS "UNTAGGED '}' MISMATCH"
            CASE 32: RESULTIS "ERROR IN EXPRESSION"
            CASE 33: RESULTIS "ERROR IN NUMBER"
            CASE 34: RESULTIS "BAD STRING"
            CASE 15:CASE 19:CASE 41: RESULTIS "')' MISSING"
            CASE 30: RESULTIS "',' MISSING"
            CASE 42: RESULTIS "'=' OR 'BE' EXPECTED"
            CASE 44: RESULTIS "'=' OR '(' EXPECTED"
            CASE 50: RESULTIS "ERROR IN LABEL"
            CASE 51: RESULTIS "ERROR IN COMMAND"
            CASE 54: RESULTIS "'OR' EXPECTED"
            CASE 57: RESULTIS "'=' EXPECTED"
            CASE 58: RESULTIS "'TO' EXPECTED"
            CASE 60: RESULTIS "'INTO' EXPECTED"
            CASE 61:CASE 62: RESULTIS "':' EXPECTED"
            CASE 63: RESULTIS "'**/' MISSING"
                       }

         writef(S, A)  }


.

//    CAE1


GET "SYNHDR"

LET RDBLOCKBODY() = VALOF
    {1 LET P, L = REC.P, REC.L
        LET A = 0

        REC.P, REC.L := level(), RECOVER

        IGNORE(S.SEMICOLON)

        SWITCHON SYMB INTO
     { CASE S.MANIFEST:
        CASE S.STATIC:
        CASE S.GLOBAL:
            {  LET OP = SYMB
                NEXTSYMB()
                A := RDSECT(RDCDEFS)
                A := LIST3(OP, A, RDBLOCKBODY())
                GOTO RET  }


        CASE S.LET: NEXTSYMB()
                    A := RDEF()
           RECOVER: WHILE SYMB=S.AND DO
                          { NEXTSYMB()
                             A := LIST3(S.AND, A, RDEF())  }
                    A := LIST3(S.LET, A, RDBLOCKBODY())
                    GOTO RET

        DEFAULT: A := RDSEQ()

                 UNLESS SYMB=S.RSECT \/ SYMB=S.END DO
                          CAEREPORT(51)

        CASE S.RSECT: CASE S.END:
        RET:   REC.P, REC.L := P, L
               RESULTIS A   }1

AND RDSEQ() = VALOF
    { LET A = 0
       IGNORE(S.SEMICOLON)
       A := RCOM()
       IF SYMB=S.RSECT \/ SYMB=S.END RESULTIS A
       RESULTIS LIST3(S.SEQ, A, RDSEQ())   }


AND RDCDEFS() = VALOF
    {1 LET A, B = 0, 0
        LET PTR = @A
        LET P, L = REC.P, REC.L
        REC.P, REC.L := level(), RECOVER

        { B := RNAME()
           TEST SYMB=S.EQ \/ SYMB=S.COLON THEN NEXTSYMB()
                                            OR CAEREPORT(45)
           !PTR := LIST4(S.CONSTDEF, 0, B, REXP(0))
           PTR := @H2!(!PTR)
  RECOVER: IGNORE(S.SEMICOLON) } REPEATWHILE SYMB=S.NAME

        REC.P, REC.L := P, L
        RESULTIS A  }1

AND RDSECT(R) = VALOF
    {  LET TAG, A = WORDNODE, 0
        CHECKFOR(S.LSECT, 6)
        A := R()
        UNLESS SYMB=S.RSECT DO CAEREPORT(7)
        TEST TAG=WORDNODE
             THEN NEXTSYMB()
               OR IF WORDNODE=NULLTAG DO
                      { SYMB := 0
                         CAEREPORT(9)  }
        RESULTIS A   }


AND RNAMELIST() = VALOF
    {  LET A = RNAME()
        UNLESS SYMB=S.COMMA RESULTIS A
        NEXTSYMB()
        RESULTIS LIST3(S.COMMA, A, RNAMELIST())   }


AND RNAME() = VALOF
    { LET A = WORDNODE
       CHECKFOR(S.NAME, 8)
       RESULTIS A  }

AND IGNORE(ITEM) BE IF SYMB=ITEM DO NEXTSYMB()

AND CHECKFOR(ITEM, N) BE
      { UNLESS SYMB=ITEM DO CAEREPORT(N)
         NEXTSYMB()  }

.

//    CAE2


GET "SYNHDR"
LET RBEXP() = VALOF
  {1   LET A, OP = 0, SYMB

        SWITCHON SYMB INTO

    {  DEFAULT:
            CAEREPORT(32)

        CASE S.QUERY:
            NEXTSYMB(); RESULTIS LIST1(S.QUERY)

        CASE S.TRUE:
        CASE S.FALSE:
        CASE S.NAME:
            A := WORDNODE
            NEXTSYMB()
            RESULTIS A

        CASE S.STRING:
            A := NEWVEC(WORDSIZE+1)
            A!0 := S.STRING
            FOR I = 0 TO WORDSIZE DO A!(I+1) := WORDV!I
            NEXTSYMB()
            RESULTIS A

        CASE S.NUMBER:
            A := LIST2(S.NUMBER, DECVAL)
            NEXTSYMB()
            RESULTIS A

        CASE S.LPAREN:
            NEXTSYMB()
            A := REXP(0)
            CHECKFOR(S.RPAREN, 15)
            RESULTIS A

        CASE S.VALOF:
            NEXTSYMB()
            RESULTIS LIST2(S.VALOF, RCOM())

        CASE S.VECAP: OP := S.RV
        CASE S.LV:
        CASE S.RV: NEXTSYMB(); RESULTIS LIST2(OP, REXP(35))

        CASE S.PLUS: NEXTSYMB(); RESULTIS REXP(34)

        CASE S.MINUS: NEXTSYMB()
                      A := REXP(34)
                      TEST H1!A=S.NUMBER
                          THEN H2!A := - H2!A
                            OR A := LIST2(S.NEG, A)
                      RESULTIS A

        CASE S.NOT: NEXTSYMB(); RESULTIS LIST2(S.NOT, REXP(24))

        CASE S.TABLE: NEXTSYMB()
                      RESULTIS LIST2(S.TABLE, REXPLIST())   }1



AND REXP(N) = VALOF
    {1 LET A = RBEXP()

        LET B, C, P, Q = 0, 0, 0, 0

  L: { LET OP = SYMB

        IF NLPENDING RESULTIS A

        SWITCHON OP INTO
    {B DEFAULT: RESULTIS A

        CASE S.LPAREN: NEXTSYMB()
                       B := 0
                       UNLESS SYMB=S.RPAREN DO B := REXPLIST()
                       CHECKFOR(S.RPAREN, 19)
                       A := LIST3(S.FNAP, A, B)
                       GOTO L

        CASE S.VECAP: P := 40; GOTO LASSOC

        CASE S.REM:CASE S.MULT:CASE S.DIV: P := 35; GOTO LASSOC

        CASE S.PLUS:CASE S.MINUS: P := 34; GOTO LASSOC

        CASE S.EQ:CASE S.NE:
        CASE S.LE:CASE S.GE:
        CASE S.LS:CASE S.GR:
                IF N>=30 RESULTIS A

            {R NEXTSYMB()
                B := REXP(30)
                A := LIST3(OP, A, B)
                TEST C=0 THEN C :=  A
                           OR C := LIST3(S.LOGAND, C, A)
                A, OP := B, SYMB  }R REPEATWHILE S.EQ<=OP<=S.GE

                A := C
                GOTO L

        CASE S.LSHIFT:CASE S.RSHIFT: P, Q := 25, 30; GOTO DIADIC

        CASE S.LOGAND: P := 23; GOTO LASSOC

        CASE S.LOGOR: P := 22; GOTO LASSOC

        CASE S.EQV:CASE S.NEQV: P := 21; GOTO LASSOC

        CASE S.COND:
                IF N>=13 RESULTIS A
                NEXTSYMB()
                B := REXP(0)
                CHECKFOR(S.COMMA, 30)
                A := LIST4(S.COND, A, B, REXP(0))
                GOTO L

        LASSOC: Q := P

        DIADIC: IF N>=P RESULTIS A
                NEXTSYMB()
                A := LIST3(OP, A, REXP(Q))
                GOTO L                     }B     }1

LET REXPLIST() = VALOF
    {1 LET A = 0
        LET PTR = @A

     { LET B = REXP(0)
        UNLESS SYMB=S.COMMA DO { !PTR := B
                                  RESULTIS A  }
        NEXTSYMB()
        !PTR := LIST3(S.COMMA, B, 0)
        PTR := @H3!(!PTR)  } REPEAT
    }1

LET RDEF() = VALOF
    {1 LET N = RNAMELIST()

        SWITCHON SYMB INTO

     { CASE S.LPAREN:
             { LET A = 0
                NEXTSYMB()
                UNLESS H1!N=S.NAME DO CAEREPORT(40)
                IF SYMB=S.NAME DO A := RNAMELIST()
                CHECKFOR(S.RPAREN, 41)

                IF SYMB=S.BE DO
                     { NEXTSYMB()
                        RESULTIS LIST5(S.RTDEF, N, A, RCOM(), 0)  }

                IF SYMB=S.EQ DO
                     { NEXTSYMB()
                        RESULTIS LIST5(S.FNDEF, N, A, REXP(0), 0)  }

                CAEREPORT(42)  }

        DEFAULT: CAEREPORT(44)

        CASE S.EQ:
                NEXTSYMB()
                IF SYMB=S.VEC DO
                     { NEXTSYMB()
                        UNLESS H1!N=S.NAME DO CAEREPORT(43)
                        RESULTIS LIST3(S.VECDEF, N, REXP(0))  }
                RESULTIS LIST3(S.VALDEF, N, REXPLIST())  }1

.


//    CAE4



GET "SYNHDR"

LET RBCOM() = VALOF
   {1 LET A, B, OP = 0, 0, SYMB

        SWITCHON SYMB INTO
     { DEFAULT: RESULTIS 0

        CASE S.NAME:CASE S.NUMBER:CASE S.STRING:
        CASE S.TRUE:CASE S.FALSE:CASE S.LV:CASE S.RV:CASE S.VECAP:
        CASE S.LPAREN:
                A := REXPLIST()

                IF SYMB=S.ASS  THEN
                    {  OP := SYMB
                        NEXTSYMB()
                        RESULTIS LIST3(OP, A, REXPLIST())  }

                IF SYMB=S.COLON DO
                     { UNLESS H1!A=S.NAME DO CAEREPORT(50)
                        NEXTSYMB()
                        RESULTIS LIST4(S.COLON, A, RBCOM(), 0)  }

                IF H1!A=S.FNAP DO
                     { H1!A := S.RTAP
                        RESULTIS A  }

                CAEREPORT(51)
                RESULTIS A

        CASE S.GOTO:CASE S.RESULTIS:
                NEXTSYMB()
                RESULTIS LIST2(OP, REXP(0))

        CASE S.IF:CASE S.UNLESS:
        CASE S.WHILE:CASE S.UNTIL:
                NEXTSYMB()
                A := REXP(0)
                IGNORE(S.DO)
                RESULTIS LIST3(OP, A, RCOM())

        CASE S.TEST:
                NEXTSYMB()
                A := REXP(0)
                IGNORE(S.DO)
                B := RCOM()
                CHECKFOR(S.OR, 54)
                RESULTIS LIST4(S.TEST, A, B, RCOM())

        CASE S.FOR:
            {  LET I, J, K = 0, 0, 0
                NEXTSYMB()
                A := RNAME()
                CHECKFOR(S.EQ, 57)
                I := REXP(0)
                CHECKFOR(S.TO, 58)
                J := REXP(0)
                IF SYMB=S.BY DO { NEXTSYMB()
                                   K := REXP(0)  }
                IGNORE(S.DO)
                RESULTIS LIST6(S.FOR, A, I, J, K, RCOM())  }

        CASE S.LOOP:
        CASE S.BREAK:CASE S.RETURN:CASE S.FINISH:CASE S.ENDCASE:
                A := WORDNODE
                NEXTSYMB()
                RESULTIS A

        CASE S.SWITCHON:
                NEXTSYMB()
                A := REXP(0)
                CHECKFOR(S.INTO, 60)
                RESULTIS LIST3(S.SWITCHON, A, RDSECT(RDSEQ))

        CASE S.CASE:
                NEXTSYMB()
                A := REXP(0)
                CHECKFOR(S.COLON, 61)
                RESULTIS LIST3(S.CASE, A, RBCOM())

        CASE S.DEFAULT:
                NEXTSYMB()
                CHECKFOR(S.COLON, 62)
                RESULTIS LIST2(S.DEFAULT, RBCOM())

        CASE S.LSECT:
                RESULTIS RDSECT(RDBLOCKBODY)   }1


AND RCOM() = VALOF
    {1 LET A = RBCOM()

        IF A=0 DO CAEREPORT(51)

        WHILE SYMB=S.REPEAT \/ SYMB=S.REPEATWHILE \/
                    SYMB=S.REPEATUNTIL DO
                  { LET OP = SYMB
                     NEXTSYMB()
                     TEST OP=S.REPEAT
                         THEN A := LIST2(OP, A)
                           OR A := LIST3(OP, A, REXP(0))   }

        RESULTIS A  }1

.

//    PLIST


GET "SYNHDR"

LET plist(X, N, D) BE
    {1 LET SIZE = 0
        LET V = TABLE 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

        IF X=0 DO { writes("NIL"); RETURN  }

        SWITCHON H1!X INTO
    {  CASE S.NUMBER: writen(H2!X); RETURN

        CASE S.NAME: writes(X+2); RETURN

        CASE S.STRING: writef("*"%S*"", X+1); RETURN

        CASE S.FOR:
                SIZE := SIZE + 2

        CASE S.COND:CASE S.FNDEF:CASE S.RTDEF:
        CASE S.TEST:CASE S.CONSTDEF:
                SIZE := SIZE + 1

        CASE S.VECAP:CASE S.FNAP:
        CASE S.MULT:CASE S.DIV:CASE S.REM:CASE S.PLUS:CASE S.MINUS:
        CASE S.EQ:CASE S.NE:CASE S.LS:CASE S.GR:CASE S.LE:CASE S.GE:
        CASE S.LSHIFT:CASE S.RSHIFT:CASE S.LOGAND:CASE S.LOGOR:
        CASE S.EQV:CASE S.NEQV:CASE S.COMMA:
        CASE S.AND:CASE S.VALDEF:CASE S.VECDEF:
        CASE S.ASS:CASE S.RTAP:CASE S.COLON:CASE S.IF:CASE S.UNLESS:
        CASE S.WHILE:CASE S.UNTIL:CASE S.REPEATWHILE:
        CASE S.REPEATUNTIL:
        CASE S.SWITCHON:CASE S.CASE:CASE S.SEQ:CASE S.LET:
        CASE S.MANIFEST:CASE S.STATIC:CASE S.GLOBAL:
                SIZE := SIZE + 1

        CASE S.VALOF:CASE S.LV:CASE S.RV:CASE S.NEG:CASE S.NOT:
        CASE S.TABLE:CASE S.GOTO:CASE S.RESULTIS:CASE S.REPEAT:
        CASE S.DEFAULT:
                SIZE := SIZE + 1

        CASE S.LOOP:
        CASE S.BREAK:CASE S.RETURN:CASE S.FINISH:CASE S.ENDCASE:
        CASE S.TRUE:CASE S.FALSE:CASE S.QUERY:
        DEFAULT:
                SIZE := SIZE + 1

                IF N=D DO { writes("ETC")
                             RETURN  }

                writes ("OP")
                writen(H1!X)
                FOR I = 2 TO SIZE DO
                     { newline()
                        FOR J=0 TO N-1 DO writes( V!J )
                        writes("**-")
                        V!N := I=SIZE->"  ","! "
                        plist(H1!(X+I-1), N+1, D)  }
                RETURN  }1
//    TRN0

GET "TRNHDR"

LET NEXTPARAM() = VALOF
    { PARAMNUMBER := PARAMNUMBER + 1
       RESULTIS PARAMNUMBER  }

AND TRANSREPORT(N, X) BE
    { selectoutput(sysprint)
       reportcount := reportcount + 1
       IF reportcount GE reportmax DO
                { writes("*NCOMPILATION ABORTED*N")
                   stop(8)  }
       writes("*NREPORT:   "); TRNMESSAGE(N)
       writef("*NCOMMANDS COMPILED %N*N", COMCOUNT)
       plist(X, 0, 4); newline()
       selectoutput(ocode)  }

AND TRNMESSAGE(N) BE
{ LET S = VALOF
    SWITCHON N INTO

    { DEFAULT: writef("COMPILER ERROR  %N", N); RETURN

       CASE 141: RESULTIS "TOO MANY CASES"
       CASE 104: RESULTIS "ILLEGAL USE OF BREAK, LOOP OR RESULTIS"
       CASE 101:
       CASE 105: RESULTIS "ILLEGAL USE OF CASE OR DEFAULT"
       CASE 106: RESULTIS "TWO CASES WITH SAME CONSTANT"
       CASE 144: RESULTIS "TOO MANY GLOBALS"
       CASE 142: RESULTIS "NAME DECLARED TWICE"
       CASE 143: RESULTIS "TOO MANY NAMES DECLARED"
       CASE 115: RESULTIS "NAME NOT DECLARED"
       CASE 116: RESULTIS "DYNAMIC FREE VARIABLE USED"
       CASE 117:CASE 118:CASE 119:
                 RESULTIS "ERROR IN CONSTANT EXPRESSION"
       CASE 110:CASE 112:
                 RESULTIS "LHS AND RHS DO NOT MATCH"
       CASE 109:CASE 113:
                 RESULTIS "LTYPE EXPRESSION EXPECTED"
                   }

   writes(S)   }


LET compileae(X) BE
   {1 LET A = VEC 1200
       LET D = VEC 100
       LET K = VEC 150
       LET L = VEC 150

       DVEC, DVECS, DVECE, DVECP, DVECT := A, 3, 3, 3, 1200
       DVEC!0, DVEC!1, DVEC!2 := 0, 0, 0

       GLOBDECL, GLOBDECLS, GLOBDECLT := D, 0, 100

       CASEK, CASEL, CASEP, CASET, CASEB := K, L, 0, 150, -1
       ENDCASELABEL, DEFAULTLABEL := 0, 0

       RESULTLABEL, BREAKLABEL, LOOPLABEL := -1, -1, -1

       COMCOUNT, CURRENTBRANCH := 0, X

       OCOUNT := 0

       PARAMNUMBER := 0
       SSP := savespacesize
       OUT2(S.STACK, SSP)
       DECLLABELS(X)
       TRANS(X)
       OUT2(S.GLOBAL, GLOBDECLS/2)

    { LET I = 0
       UNTIL I=GLOBDECLS DO
          { OUTN(GLOBDECL!I)
             OUTL(GLOBDECL!(I+1))
             I := I + 2  }

       ENDOCODE()  }1

.

//    TRN1


GET "TRNHDR"

LET TRANS(X) BE
  {TR
NEXT:
 { LET SW = FALSE
    IF X=0 RETURN
    CURRENTBRANCH := X

    SWITCHON H1!X INTO
{  DEFAULT: TRANSREPORT(100, X); RETURN

    CASE S.LET:
      { LET A, B, S, S1 = DVECE, DVECS, SSP, 0
         LET V = VECSSP
         DECLNAMES(H2!X)
         CHECKDISTINCT(B, DVECS)
         DVECE := DVECS
         VECSSP, S1 := SSP, SSP
         SSP := S
         TRANSDEF(H2!X)
         UNLESS SSP=S1 DO TRANSREPORT(110, X)
         UNLESS SSP=VECSSP DO { SSP := VECSSP
                                 OUT2(S.STACK, SSP)  }
         OUT1(S.STORE)
         DECLLABELS(H3!X)
         TRANS(H3!X)
         VECSSP := V
         UNLESS SSP=S DO OUT2(S.STACK, S)
         DVECE, DVECS, SSP := A, B, S
         RETURN   }

    CASE S.STATIC:
    CASE S.GLOBAL:
    CASE S.MANIFEST:
     {1 LET A, B, S = DVECE, DVECS, SSP
         AND OP = H1!X
         AND Y = H2!X

         IF OP=S.MANIFEST DO OP := S.NUMBER

         UNTIL Y=0 DO
           { TEST OP=S.STATIC THEN
                { LET M = NEXTPARAM()
                   ADDNAME(H3!Y, S.LABEL, M)
                   COMPDATALAB(M)
                   OUT2(S.ITEMN, EVALCONST(H4!Y))  }

                OR ADDNAME(H3!Y, OP, EVALCONST(H4!Y))

              Y := H2!Y
              DVECE := DVECS  }

         DECLLABELS(H3!X)
         TRANS(H3!X)
         DVECE, DVECS, SSP := A, B, S
         RETURN   }1


    CASE S.ASS:
       ASSIGN(H2!X, H3!X)
       RETURN

    CASE S.RTAP:
     { LET S = SSP
        SSP := SSP+savespacesize
        OUT2(S.STACK, SSP)
        LOADLIST(H3!X)
        LOAD(H2!X)
        OUT2(S.RTAP, S)
        SSP := S
        RETURN  }

    CASE S.GOTO:
        LOAD(H2!X)
        OUT1(S.GOTO)
        SSP := SSP-1
        RETURN

    CASE S.COLON:
        COMPLAB(H4!X)
        TRANS(H3!X)
        RETURN

    CASE S.UNLESS: SW := TRUE
    CASE S.IF:
     { LET L = NEXTPARAM()
        JUMPCOND(H2!X, SW, L)
        TRANS(H3!X)
        COMPLAB(L)
        RETURN   }

    CASE S.TEST:
     { LET L, M = NEXTPARAM(), NEXTPARAM()
        JUMPCOND(H2!X, FALSE, L)
        TRANS(H3!X)
        COMPJUMP(M)
        COMPLAB(L)
        TRANS(H4!X)
        COMPLAB(M)
        RETURN   }

    CASE S.LOOP:
        IF LOOPLABEL<0 DO TRANSREPORT(104, X)
        IF LOOPLABEL=0 DO LOOPLABEL := NEXTPARAM()
        COMPJUMP(LOOPLABEL)
        RETURN

    CASE S.BREAK:
        IF BREAKLABEL<0 DO TRANSREPORT(104, X)
        IF BREAKLABEL=0 DO BREAKLABEL := NEXTPARAM()
        COMPJUMP(BREAKLABEL)
        RETURN

    CASE S.RETURN: OUT1(S.RTRN)
                   RETURN

    CASE S.FINISH: OUT1(S.FINISH)
                   RETURN

    CASE S.RESULTIS:
        IF RESULTLABEL<0 DO TRANSREPORT(104, X)
        LOAD(H2!X)
        OUT2P(S.RES, RESULTLABEL)
        SSP := SSP - 1
        RETURN

    CASE S.WHILE: SW := TRUE
    CASE S.UNTIL:
     { LET L, M = NEXTPARAM(), NEXTPARAM()
        LET BL, LL = BREAKLABEL, LOOPLABEL
        BREAKLABEL, LOOPLABEL := 0, M

        COMPJUMP(M)
        COMPLAB(L)
        TRANS(H3!X)
        COMPLAB(M)
        JUMPCOND(H2!X, SW, L)
        UNLESS BREAKLABEL=0 DO COMPLAB(BREAKLABEL)
        BREAKLABEL, LOOPLABEL := BL, LL
        RETURN   }

    CASE S.REPEATWHILE: SW := TRUE
    CASE S.REPEATUNTIL:
    CASE S.REPEAT:
     { LET L, BL, LL = NEXTPARAM(), BREAKLABEL, LOOPLABEL
        BREAKLABEL, LOOPLABEL := 0, 0
        COMPLAB(L)
        TEST H1!X=S.REPEAT
            THEN { LOOPLABEL := L
                    TRANS(H2!X)
                    COMPJUMP(L)  }
              OR { TRANS(H2!X)
                    UNLESS LOOPLABEL=0 DO COMPLAB(LOOPLABEL)
                    JUMPCOND(H3!X, SW, L)  }
        UNLESS BREAKLABEL=0 DO COMPLAB(BREAKLABEL)
        BREAKLABEL, LOOPLABEL := BL, LL
        RETURN   }

    CASE S.CASE:
     { LET L, K = NEXTPARAM(), EVALCONST(H2!X)
        IF CASEP>=CASET DO TRANSREPORT(141, X)
        IF CASEB<0 DO TRANSREPORT(105, X)
        FOR I = CASEB TO CASEP-1 DO
                    IF CASEK!I=K DO TRANSREPORT(106, X)
        CASEK!CASEP := K
        CASEL!CASEP := L
        CASEP := CASEP + 1
        COMPLAB(L)
        TRANS(H3!X)
        RETURN   }

    CASE S.DEFAULT:
        IF CASEB<0 DO TRANSREPORT(105, X)
        UNLESS DEFAULTLABEL=0 DO TRANSREPORT(101, X)
        DEFAULTLABEL := NEXTPARAM()
        COMPLAB(DEFAULTLABEL)
        TRANS(H2!X)
        RETURN

    CASE S.ENDCASE: IF CASEB<0 DO TRANSREPORT(105, X)
                    COMPJUMP(ENDCASELABEL)
                    RETURN

    CASE S.SWITCHON:
        TRANSSWITCH(X)
        RETURN

    CASE S.FOR: TRANSFOR(X)
                RETURN

    CASE S.SEQ:
        TRANS(H2!X)
        COMCOUNT :=  COMCOUNT + 1
        X := H3!X
        GOTO NEXT        }TR
.

//    TRN2


GET "TRNHDR"

LET DECLNAMES(X) BE UNLESS X=0 SWITCHON H1!X INTO

     {  DEFAULT: TRANSREPORT(102, CURRENTBRANCH)
                  RETURN

         CASE S.VECDEF: CASE S.VALDEF:
               DECLDYN(H2!X)
               RETURN

         CASE S.RTDEF: CASE S.FNDEF:
               H5!X := NEXTPARAM()
               DECLSTAT(H2!X, H5!X)
               RETURN

         CASE S.AND:
               DECLNAMES(H2!X)
               DECLNAMES(H3!X)
               RETURN    }


AND DECLDYN(X) BE UNLESS X=0 DO

    { IF H1!X=S.NAME DO
          { ADDNAME(X, S.LOCAL, SSP)
             SSP := SSP + 1
             RETURN   }

       IF H1!X=S.COMMA DO
          { ADDNAME(H2!X, S.LOCAL, SSP)
             SSP := SSP + 1
             DECLDYN(H3!X)
             RETURN  }

       TRANSREPORT(103, X)   }

AND DECLSTAT(X, L) BE
    {1 LET T = CELLWITHNAME(X)

       IF DVEC!(T+1)=S.GLOBAL DO
          { LET N = DVEC!(T+2)
             ADDNAME(X, S.GLOBAL, N)
             IF GLOBDECLS>=GLOBDECLT DO TRANSREPORT(144, X)
             GLOBDECL!GLOBDECLS := N
             GLOBDECL!(GLOBDECLS+1) := L
             GLOBDECLS := GLOBDECLS + 2
             RETURN  }


    { LET M = NEXTPARAM()
       ADDNAME(X, S.LABEL, M)
       COMPDATALAB(M)
       OUT2P(S.ITEML, L)    }1


AND DECLLABELS(X) BE
    { LET B = DVECS
       SCANLABELS(X)
       CHECKDISTINCT(B, DVECS)
       DVECE := DVECS   }


AND CHECKDISTINCT(E, S) BE
       UNTIL E=S DO
          { LET P = E + 3
             AND N = DVEC!E
             WHILE P<S DO
                { IF DVEC!P=N DO TRANSREPORT(142, N)
                   P := P + 3  }
             E := E + 3  }


AND ADDNAME(N, P, A) BE
    { IF DVECS>=DVECT DO TRANSREPORT(143, CURRENTBRANCH)
       DVEC!DVECS, DVEC!(DVECS+1), DVEC!(DVECS+2) := N, P, A
       DVECS := DVECS + 3  }


AND CELLWITHNAME(N) = VALOF
    { LET X = DVECE

       X := X - 3 REPEATUNTIL X=0 \/ DVEC!X=N

       RESULTIS X  }


AND SCANLABELS(X) BE UNLESS X=0 SWITCHON H1!X INTO

    { DEFAULT: RETURN

       CASE S.COLON:
            H4!X := NEXTPARAM()
            DECLSTAT(H2!X, H4!X)

       CASE S.IF: CASE S.UNLESS: CASE S.WHILE: CASE S.UNTIL:
       CASE S.SWITCHON: CASE S.CASE:
            SCANLABELS(H3!X)
            RETURN

       CASE S.SEQ:
            SCANLABELS(H3!X)

       CASE S.REPEAT:
       CASE S.REPEATWHILE: CASE S.REPEATUNTIL: CASE S.DEFAULT:
            SCANLABELS(H2!X)
            RETURN

       CASE S.TEST:
            SCANLABELS(H3!X)
            SCANLABELS(H4!X)
            RETURN    }


AND TRANSDEF(X) BE
    {1 TRANSDYNDEFS(X)
        IF STATDEFS(X) DO
           { LET L, S= NEXTPARAM(), SSP
              COMPJUMP(L)
              TRANSSTATDEFS(X)
              SSP := S
              OUT2(S.STACK, SSP)
              COMPLAB(L)  }1


AND TRANSDYNDEFS(X) BE
        SWITCHON H1!X INTO
     { CASE S.AND:
            TRANSDYNDEFS(H2!X)
            TRANSDYNDEFS(H3!X)
            RETURN

        CASE S.VECDEF:
            OUT2(S.LLP, VECSSP)
            SSP := SSP + 1
            VECSSP := VECSSP + 1 + EVALCONST(H3!X)
            RETURN

        CASE S.VALDEF: LOADLIST(H3!X)
                       RETURN

        DEFAULT: RETURN  }

AND TRANSSTATDEFS(X) BE
        SWITCHON H1!X INTO
     { CASE S.AND:
             TRANSSTATDEFS(H2!X)
             TRANSSTATDEFS(H3!X)
             RETURN

        CASE S.FNDEF: CASE S.RTDEF:
         {2 LET A, B, C = DVECE, DVECS, DVECP
             AND BL, LL = BREAKLABEL, LOOPLABEL
             AND RL, CB = RESULTLABEL, CASEB
             BREAKLABEL, LOOPLABEL := -1, -1
             RESULTLABEL, CASEB := -1, -1

             COMPENTRY(H2!X, H5!X)
             SSP := savespacesize

             DVECP := DVECS
             DECLDYN(H3!X)
             CHECKDISTINCT(B, DVECS)
             DVECE := DVECS
             DECLLABELS(H4!X)

             OUT2(S.SAVE, SSP)

             TEST H1!X=S.FNDEF
                THEN { LOAD(H4!X); OUT1(S.FNRN)  }
                  OR { TRANS(H4!X); OUT1(S.RTRN)  }

             OUT2(S.ENDPROC, 0)

             BREAKLABEL, LOOPLABEL := BL, LL
             RESULTLABEL, CASEB := RL, CB
             DVECE, DVECS, DVECP := A, B, C   }2

        DEFAULT: RETURN   }

AND STATDEFS(X) = H1!X=S.FNDEF \/ H1!X=S.RTDEF -> TRUE,
                  H1!X NE S.AND -> FALSE,
                  STATDEFS(H2!X) -> TRUE,
                  STATDEFS(H3!X)


.

//    TRN3


GET "TRNHDR"

LET JUMPCOND(X, B, L) BE
{JC LET SW = B
     SWITCHON H1!X INTO
     { CASE S.FALSE: B := NOT B
        CASE S.TRUE: IF B DO COMPJUMP(L)
                     RETURN

        CASE S.NOT: JUMPCOND(H2!X, NOT B, L)
                    RETURN

        CASE S.LOGAND: SW := NOT SW
        CASE S.LOGOR:
         TEST SW THEN { JUMPCOND(H2!X, B, L)
                         JUMPCOND(H3!X, B, L)  }

                   OR { LET M = NEXTPARAM()
                         JUMPCOND(H2!X, NOT B, M)
                         JUMPCOND(H3!X, B, L)
                         COMPLAB(M)  }

         RETURN

        DEFAULT: LOAD(X)
                 OUT2P(B -> S.JT, S.JF, L)
                 SSP := SSP - 1
                 RETURN     }JC

AND TRANSSWITCH(X) BE
    {1 LET P, B, DL = CASEP, CASEB, DEFAULTLABEL
        AND ECL = ENDCASELABEL
        LET L = NEXTPARAM()
        ENDCASELABEL := NEXTPARAM()
        CASEB := CASEP

        COMPJUMP(L)
        DEFAULTLABEL := 0
        TRANS(H3!X)
        COMPJUMP(ENDCASELABEL)

        COMPLAB(L)
        LOAD(H2!X)
        IF DEFAULTLABEL=0 DO DEFAULTLABEL := ENDCASELABEL
        OUT3P(S.SWITCHON, CASEP-P, DEFAULTLABEL)

        FOR I = CASEB TO CASEP-1 DO { OUTN(CASEK!I)
                                       OUTL(CASEL!I)  }

        SSP := SSP - 1
        COMPLAB(ENDCASELABEL)
        ENDCASELABEL := ECL
        CASEP, CASEB, DEFAULTLABEL := P, B, DL   }1

AND TRANSFOR(X) BE
     { LET A, B = DVECE, DVECS
        LET L, M = NEXTPARAM(), NEXTPARAM()
        LET BL, LL = BREAKLABEL, LOOPLABEL
        LET K, N = 0, 0
        LET STEP = 1
        LET S = SSP
        BREAKLABEL, LOOPLABEL := 0, 0

        ADDNAME(H2!X, S.LOCAL, S)
        DVECE := DVECS
        LOAD(H3!X)

        TEST H1!(H4!X)=S.NUMBER
            THEN K, N := S.LN, H2!(H4!X)
              OR { K, N := S.LP, SSP
                    LOAD(H4!X)  }

        UNLESS H5!X=0 DO STEP := EVALCONST(H5!X)

        OUT1(S.STORE)
        COMPJUMP(L)
        DECLLABELS(H6!X)
        COMPLAB(M)
        TRANS(H6!X)
        UNLESS LOOPLABEL=0 DO COMPLAB(LOOPLABEL)
        OUT2(S.LP, S); OUT2(S.LN, STEP); OUT1(S.PLUS); OUT2(S.SP, S)
        COMPLAB(L)
        OUT2(S.LP, S); OUT2(K, N); OUT1(STEP<0 -> S.GE, S.LE)
        OUT2P(S.JT, M)

        UNLESS BREAKLABEL=0 DO COMPLAB(BREAKLABEL)
        BREAKLABEL, LOOPLABEL, SSP := BL, LL, S
        OUT2(S.STACK, SSP)
        DVECE, DVECS := A, B  }

.

//    TRN4


GET "TRNHDR"

LET LOAD(X) BE
    {1 IF X=0 DO { TRANSREPORT(148, CURRENTBRANCH)
                     LOADZERO()
                     RETURN  }

     { LET OP = H1!X

        SWITCHON OP INTO
     { DEFAULT: TRANSREPORT(147, CURRENTBRANCH)
                 LOADZERO()
                 RETURN

        CASE S.DIV: CASE S.REM: CASE S.MINUS:
        CASE S.LS: CASE S.GR: CASE S.LE: CASE S.GE:
        CASE S.LSHIFT: CASE S.RSHIFT:
            LOAD(H2!X)
            LOAD(H3!X)
            OUT1(OP)
            SSP := SSP - 1
            RETURN

        CASE S.VECAP: CASE S.MULT: CASE S.PLUS: CASE S.EQ: CASE S.NE:
        CASE S.LOGAND: CASE S.LOGOR: CASE S.EQV: CASE S.NEQV:
         { LET A, B = H2!X, H3!X
            IF H1!A=S.NAME \/ H1!A=S.NUMBER DO
                               A, B := H3!X, H2!X
            LOAD(A)
            LOAD(B)
            IF OP=S.VECAP DO { OUT1(S.PLUS); OP := S.RV  }
            OUT1(OP)
            SSP := SSP - 1
            RETURN   }

        CASE S.NEG: CASE S.NOT: CASE S.RV:
            LOAD(H2!X)
            OUT1(OP)
            RETURN

        CASE S.TRUE: CASE S.FALSE: CASE S.QUERY:
            OUT1(OP)
            SSP := SSP + 1
            RETURN

        CASE S.LV: LOADLV(H2!X)
                   RETURN

        CASE S.NUMBER:
            OUT2(S.LN, H2!X)
            SSP := SSP + 1
            RETURN

        CASE S.STRING:
         { LET S = @H2!X
            OUT2(S.LSTR, getbyte(S, 0))
            FOR I = 1 TO getbyte(S, 0) DO OUTC(getbyte(S, I))
            WRC('*S')
            SSP := SSP + 1
            RETURN   }

        CASE S.NAME:
             TRANSNAME(X, S.LP, S.LG, S.LL, S.LN)
             SSP := SSP + 1
             RETURN

        CASE S.VALOF:
         { LET RL = RESULTLABEL
            LET A, B = DVECS, DVECE
            DECLLABELS(H2!X)
            RESULTLABEL := NEXTPARAM()
            TRANS(H2!X)
            COMPLAB(RESULTLABEL)
            OUT2(S.RSTACK, SSP)
            SSP := SSP + 1
            DVECS, DVECE := A, B
            RESULTLABEL := RL
            RETURN   }


        CASE S.FNAP:
         { LET S = SSP
            SSP := SSP + savespacesize
            OUT2(S.STACK, SSP)
            LOADLIST(H3!X)
            LOAD(H2!X)
            OUT2(S.FNAP, S)
            SSP := S + 1
            RETURN   }

        CASE S.COND:
         { LET L, M = NEXTPARAM(), NEXTPARAM()
            LET S = SSP
            JUMPCOND(H2!X, FALSE, M)
            LOAD(H3!X)
            COMPJUMP(L)
            SSP := S; OUT2(S.STACK, SSP)
            COMPLAB(M)
            LOAD(H4!X)
            COMPLAB(L)
            RETURN   }

        CASE S.TABLE:
         { LET M = NEXTPARAM()
            COMPDATALAB(M)
            X := H2!X
            WHILE H1!X=S.COMMA DO
                  { OUT2(S.ITEMN, EVALCONST(H2!X))
                     X := H3!X   }
            OUT2(S.ITEMN, EVALCONST(X))
            OUT2P(S.LLL, M)
            SSP := SSP + 1
            RETURN  }                         }1


AND LOADLV(X) BE
    {1 IF X=0 GOTO ERR

        SWITCHON H1!X INTO
     { DEFAULT:
        ERR:     TRANSREPORT(113, CURRENTBRANCH)
                 LOADZERO()
                 RETURN

        CASE S.NAME:
              TRANSNAME(X, S.LLP, S.LLG, S.LLL, 0)
              SSP := SSP + 1
              RETURN

        CASE S.RV:
            LOAD(H2!X)
            RETURN

        CASE S.VECAP:
         { LET A, B = H2!X, H3!X
            IF H1!A=S.NAME DO A, B := H3!X, H2!X
            LOAD(A)
            LOAD(B)
            OUT1(S.PLUS)
            SSP := SSP - 1
            RETURN   }  }1

AND LOADZERO() BE { OUT2(S.LN, 0)
                     SSP := SSP + 1  }

AND LOADLIST(X) BE UNLESS X=0 DO
    { UNLESS H1!X=S.COMMA DO { LOAD(X); RETURN  }

       LOADLIST(H2!X)
       LOADLIST(H3!X)  }
.

//    TRN5


GET "TRNHDR"

LET EVALCONST(X) = VALOF
    {1 IF X=0 DO { TRANSREPORT(117, CURRENTBRANCH)
                     RESULTIS 0  }

        SWITCHON H1!X INTO
     { DEFAULT: TRANSREPORT(118, X)
                 RESULTIS 0

        CASE S.NAME:
         { LET T = CELLWITHNAME(X)
            IF DVEC!(T+1)=S.NUMBER RESULTIS DVEC!(T+2)
            TRANSREPORT(119, X)
            RESULTIS 0  }

        CASE S.NUMBER: RESULTIS H2!X
        CASE S.TRUE: RESULTIS TRUE
        CASE S.FALSE: RESULTIS FALSE

        CASE S.NEG: RESULTIS - EVALCONST(H2!X)

        CASE S.MULT: RESULTIS EVALCONST(H2!X) * EVALCONST(H3!X)
        CASE S.DIV:  RESULTIS EVALCONST(H2!X) / EVALCONST(H3!X)
        CASE S.PLUS: RESULTIS EVALCONST(H2!X) + EVALCONST(H3!X)
        CASE S.MINUS:RESULTIS EVALCONST(H2!X) - EVALCONST(H3!X)
                    }1


AND ASSIGN(X, Y) BE
    {1 IF X=0 \/ Y=0 DO
            { TRANSREPORT(110, CURRENTBRANCH)
               RETURN  }

        SWITCHON H1!X INTO
     { CASE S.COMMA:
            UNLESS H1!Y=S.COMMA DO
                       { TRANSREPORT(112, CURRENTBRANCH)
                          RETURN   }
            ASSIGN(H2!X, H2!Y)
            ASSIGN(H3!X, H3!Y)
            RETURN

        CASE S.NAME:
            LOAD(Y)
            TRANSNAME(X, S.SP, S.SG, S.SL, 0)
            SSP := SSP - 1
            RETURN

        CASE S.RV: CASE S.VECAP: CASE S.COND:
            LOAD(Y)
            LOADLV(X)
            OUT1(S.STIND)
            SSP := SSP - 2
            RETURN

        DEFAULT: TRANSREPORT(109, CURRENTBRANCH)   }1


AND TRANSNAME(X, P, G, L, N) BE
    {1 LET T = CELLWITHNAME(X)
        LET K, A = DVEC!(T+1), DVEC!(T+2)

        IF T=0 DO { TRANSREPORT(115, X)
                     OUT2(G, 2)
                     RETURN  }

        SWITCHON K INTO
        { CASE S.LOCAL: IF T<DVECP DO TRANSREPORT(116, X)
                         OUT2(P, A); RETURN

           CASE S.GLOBAL: OUT2(G, A); RETURN

           CASE S.LABEL: OUT2P(L, A); RETURN

           CASE S.NUMBER: IF N=0 DO { TRANSREPORT(113, X)
                                       N := P  }
                          OUT2(N, A)  }1

.

//    TRN6


GET "TRNHDR"

LET COMPLAB(L) BE OUT2P(S.LAB, L)

AND COMPENTRY(N, L) BE
    {  LET S = @N!2
        OUT3P(S.ENTRY, getbyte(S, 0), L)
        FOR I = 1 TO getbyte(S, 0) DO OUTC(getbyte(S, I))
        WRC('*S')  }

AND COMPDATALAB(L) BE OUT2P(S.DATALAB, L)

AND COMPJUMP(L) BE OUT2P(S.JUMP, L)

AND OUT1(X) BE
    { WRITEOP(X); WRC('*S')  }

AND OUT2(X, Y) BE
    { WRITEOP(X); WRC('*S')
       WRN(Y); WRC('*S')   }

AND OUT2P(X, Y) BE
    { WRITEOP(X); WRC('*S'); WRC('L')
       WRN(Y); WRC('*S')   }

AND OUT3P(X, Y, Z) BE
    { WRITEOP(X); WRC('*S')
       WRN(Y); WRC('*S'); WRC('L')
       WRN(Z); WRC('*S')   }


AND OUTN(N) BE WRN(N)

AND OUTL(X) BE
    { WRC('*S'); WRC('L'); WRN(X); WRC('*S')  }

AND OUTC(X) BE
    { WRN(CHARCODE(X)); WRC('*S')   }

AND WRITEOP(X) BE
    {1 LET S = VALOF SWITCHON X INTO
        { DEFAULT: TRANSREPORT(199, CURRENTBRANCH)
                    RESULTIS 'ERROR'

           CASE S.MULT:    RESULTIS "MULT"
           CASE S.DIV:     RESULTIS "DIV"
           CASE S.REM:     RESULTIS "REM"
           CASE S.PLUS:    RESULTIS "PLUS"
           CASE S.MINUS:   RESULTIS "MINUS"
           CASE S.EQ:      RESULTIS "EQ"
           CASE S.NE:      RESULTIS "NE"
           CASE S.LS:      RESULTIS "LS"
           CASE S.GR:      RESULTIS "GR"
           CASE S.LE:      RESULTIS "LE"
           CASE S.GE:      RESULTIS "GE"
           CASE S.LSHIFT:  RESULTIS "LSHIFT"
           CASE S.RSHIFT:  RESULTIS "RSHIFT"
           CASE S.LOGAND:  RESULTIS "LOGAND"
           CASE S.LOGOR:   RESULTIS "LOGOR"
           CASE S.EQV:     RESULTIS "EQV"
           CASE S.NEQV:    RESULTIS "NEQV"

           CASE S.NEG:     RESULTIS "NEG"
           CASE S.NOT:     RESULTIS "NOT"
           CASE S.RV:      RESULTIS "RV"

           CASE S.TRUE:    RESULTIS "TRUE"
           CASE S.FALSE:   RESULTIS "FALSE"
           CASE S.QUERY:   RESULTIS "QUERY"

           CASE S.LP:      RESULTIS "LP"
           CASE S.LG:      RESULTIS "LG"
           CASE S.LN:      RESULTIS "LN"
           CASE S.LSTR:    RESULTIS "LSTR"
           CASE S.LL:      RESULTIS "LL"

           CASE S.LLP:     RESULTIS "LLP"
           CASE S.LLG:     RESULTIS "LLG"
           CASE S.LLL:     RESULTIS "LLL"

           CASE S.SP:      RESULTIS "SP"
           CASE S.SG:      RESULTIS "SG"
           CASE S.SL:      RESULTIS "SL"
           CASE S.STIND:   RESULTIS "STIND"

           CASE S.JUMP:    RESULTIS "JUMP"
           CASE S.JT:      RESULTIS "JT"
           CASE S.JF:      RESULTIS "JF"
           CASE S.GOTO:    RESULTIS "GOTO"
           CASE S.LAB:     RESULTIS "LAB"
           CASE S.STACK:   RESULTIS "STACK"
           CASE S.STORE:   RESULTIS "STORE"

           CASE S.ENTRY:   RESULTIS "ENTRY"
           CASE S.SAVE:    RESULTIS "SAVE"
           CASE S.FNAP:    RESULTIS "FNAP"
           CASE S.FNRN:    RESULTIS "FNRN"
           CASE S.RTAP:    RESULTIS "RTAP"
           CASE S.RTRN:    RESULTIS "RTRN"
           CASE S.ENDPROC: RESULTIS "ENDPROC"
           CASE S.RES:     RESULTIS "RES"
           CASE S.RSTACK:  RESULTIS "RSTACK"
           CASE S.FINISH:  RESULTIS "FINISH"

           CASE S.SWITCHON:RESULTIS "SWITCHON"
           CASE S.GLOBAL:  RESULTIS "GLOBAL"
           CASE S.DATALAB: RESULTIS "DATALAB"
           CASE S.ITEML:   RESULTIS "ITEML"
           CASE S.ITEMN:   RESULTIS "ITEMN"   }

        FOR I = 1 TO getbyte(S, 0) DO WRC(getbyte(S, I))   }1


AND WRN(N) BE { IF N<0 DO { WRC('-'); N := - N  }
                 WRPN(N)  }

AND WRPN(N) BE { IF N>9 DO WRPN(N/10)
                  WRC(N REM 10 + '0')  }

AND ENDOCODE() BE { wrch('*N'); OCOUNT := 0  }


AND WRC(CH) BE { OCOUNT := OCOUNT + 1
                  IF OCOUNT>62 /\ CH='*S' DO
                            { wrch('*N'); OCOUNT := 0; RETURN  }
                  wrch(CH)  }
