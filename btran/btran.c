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

comp(V, treemax)
{
    int B = VEC 63;
    int A;
    chbuf := B

    for (;;) {
        treep, treevec := V+treemax, V

        A = formtree()
        IF A=0 break

        writef("*NTREE SIZE %N*N", treemax+treevec-treep)

        IF option[2] DO {
            writes('AE TREE*N')
            plist(A, 0, 20)
            newline()
        }

        UNLESS reportcount=0 DO
            stop(8)

        UNLESS option[3] DO {
            selectoutput(ocode)
            compileae(A)
            selectoutput(sysprint)
        }
    }
}

int main()
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
    FOR i = 0 TO 20 DO
        OPT[i] := FALSE

    sourcestream := findinput("OPTIONS")

    UNLESS sourcestream=0 DO {
        LET ch = 0
        AND N = 0
        selectinput(sourcestream)
        writes("OPTIONS  ")

        for (;;) {
            ch := getchar()
L:          IF ch='*N' \/ ch=ENDSTREAMCH
                break
            putchar(ch)
            IF ch='P' DO N := 1
            IF ch='T' DO N := 2
            IF ch='C' DO N := 3
            IF ch='M' DO N := 4
            IF ch='N' DO N := 5
            IF ch='S' DO prsource := TRUE
            IF ch='E' DO pptrace := TRUE
            IF ch='L' DO {
                TREESIZE := readn()
                writen(TREESIZE)
                ch := terminator
                goto L
            }
            IF ch='3' DO savespacesize := 3
            option[N] := TRUE
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

    aptovec(comp, TREESIZE)

    endread()
    //IF option[4] DO mapstore()
    writes('*NPHASE 1 COMPLETE*N')
    UNLESS reportcount=0 DO stop(8)
    return 0;
}

//    LEX1

GET "SYNHDR"

void nextsymb()
{1
    nlpending := FALSE
next:
    if (pptrace) {
        putchar(ch);
    }

    switch (ch) {
    case '\n':
        linecount := linecount + 1;
        nlpending := TRUE;          // ignorable characters
    case '\v':
    case '\t':
    case ' ':
        do {
            rch();
        } while (ch == ' ');
        goto next;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
         SYMB := S_NUMBER
         readnumber(10)
         RETURN

    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y':
    case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e':
    case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y':
    case 'z':
           RDTAG(ch)
           SYMB := LOOKUPWORD()
           IF SYMB=S_GET DO { PERFORMGET(); goto next  }
           RETURN

    case '$': rch()
              UNLESS ch='(' \/ ch=')' DO CAEREPORT(91)
              SYMB := ch='(' -> S_LSECT, S_RSECT
              RDTAG('$')
              LOOKUPWORD()
              RETURN

    case '[':
    case '(': SYMB := S_LPAREN; goto L
    case ']':
    case ')': SYMB := S_RPAREN; goto L

    case '#': SYMB := S_NUMBER
              rch()
              IF '0'<=ch<='7' DO { readnumber(8); RETURN  }
              IF ch='B' DO { rch(); readnumber(2); RETURN  }
              IF ch='O' DO { rch(); readnumber(8); RETURN  }
              IF ch='X' DO { rch(); readnumber(16); RETURN  }
              CAEREPORT(33)

    case '?': SYMB := S_QUERY; goto L
    case '+': SYMB := S_PLUS; goto L
    case ',': SYMB := S_COMMA; goto L
    case ';': SYMB := S_SEMICOLON; goto L
    case '@': SYMB := S_LV; goto L
    case '&': SYMB := S_LOGAND; goto L
    case '=': SYMB := S_EQ; goto L
    case '!': SYMB := S_VECAP; goto L
    case '_': SYMB := S_ASS; goto L
    case '**': SYMB := S_MULT; goto L

    case '/': rch()
              IF ch='\' DO { SYMB := S_LOGAND; goto L }
              IF ch='/' goto COMMENT
              UNLESS ch='**' DO { SYMB := S_DIV; RETURN  }

              rch()

              UNTIL ch=ENDSTREAMCH DO TEST ch='**'

                    THEN { rch()
                            UNLESS ch='/' LOOP
                            rch()
                            goto next  }

                    OR { IF ch='*N' DO linecount := linecount+1
                          rch()  }

              CAEREPORT(63)


    COMMENT: rch() REPEATUNTIL ch='*N' \/ ch=ENDSTREAMCH
             goto next

    case '|': rch()
              IF ch='|' goto COMMENT
              SYMB := S_LOGOR
              RETURN

    case '\': rch()
              IF ch='/' DO { SYMB := S_LOGOR; goto L  }
              IF ch='=' DO { SYMB := S_NE; goto L  }
              SYMB := S_NOT
              RETURN

    case '<': rch()
              IF ch='=' DO { SYMB := S_LE; goto L  }
              IF ch='<' DO { SYMB := S_LSHIFT; goto L }
              SYMB := S_LS
              RETURN

    case '>': rch()
              IF ch='=' DO { SYMB := S_GE; goto L  }
              IF ch='>' DO { SYMB := S_RSHIFT; goto L  }
              SYMB := S_GR
              RETURN

    case '-': rch()
              IF ch='>' DO { SYMB := S_COND; goto L  }
              SYMB := S_MINUS
              RETURN

    case ':': rch()
              IF ch='=' DO { SYMB := S_ASS; goto L  }
              SYMB := S_COLON
              RETURN

     case '\'': case '\"':
          {1 LET QUOTE = ch
              CHARP := 0

           { rch()
              IF ch=QUOTE \/ CHARP=255 DO
                     { UNLESS ch=QUOTE DO CAEREPORT(95)
                        IF CHARP=1 & ch='*'' DO
                                { SYMB := S_NUMBER
                                   goto L  }
                        CHARV[0] := CHARP
                        WORDSIZE := packstring(CHARV, WORDV)
                        SYMB := S_STRING
                        goto L   }


              IF ch='*N' DO linecount := linecount + 1

              IF ch='**' DO
                     { rch()
                        IF ch='*N' DO
                            { linecount := linecount+1
                               rch() REPEATWHILE ch='*S' \/ ch='*T'
                               UNLESS ch='**' DO CAEREPORT(34)
                               LOOP  }
                        IF ch='T' DO ch := '*T'
                        IF ch='S' DO ch := '*S'
                        IF ch='N' DO ch := '*N'
                        IF ch='B' DO ch := '*B'
                        IF ch='P' DO ch := '*P'  }

              DECVAL, CHARP := ch, CHARP+1
              CHARV[CHARP] := ch  } REPEAT  }1



    DEFAULT: IF ch=ENDSTREAMCH DO
    case '.':    { IF GETP=0 DO
                          { SYMB := S_END
                             RETURN   }

                    endread()
                    GETP := GETP - 3
                    sourcestream := GETV[GETP]
                    selectinput(sourcestream)
                    linecount := GETV[GETP+1]
                    ch := GETV[GETP+2]
                    goto next  }

                ch := '*S'
                CAEREPORT(94)
                rch()
                goto next

L:      rch()
    }
}1

AND readnumber(RADIX) BE
    { LET D = value(ch)
       DECVAL := D
       IF D>=RADIX DO CAEREPORT(33)

       { rch()
          D := value(ch)
          IF D>=RADIX RETURN
          DECVAL := RADIX*DECVAL + D  } REPEAT
    }


int value(ch)
{
    return ('0' <= ch && ch <= '9') ? (ch - '0') :
           ('A' <= ch && ch <= 'F') ? (ch - 'A' + 10) :
           ('a' <= ch && ch <= 'f') ? (ch - 'a' + 10) : 100;
}

//    LEX2

GET "SYNHDR"

void D(S, ITEM)
{
    unpackstring(S, CHARV)
    WORDSIZE := packstring(CHARV, WORDV)
    LOOKUPWORD()
    WORDNODE[0] := ITEM
}

void DECLSYSWORDS()
{
    D("AND", S_AND)

    D("BE", S_BE)
    D("BREAK", S_BREAK)
    D("BY", S_BY)

    D("CASE", S_case)

    D("DO", S_DO)
    D("DEFAULT", S_DEFAULT)

    D("EQ", S_EQ)
    D("EQV", S_EQV)
    D("ELSE", S_OR)
    D("ENDcase", S_ENDcase)

    D("FALSE", S_FALSE)
    D("FOR", S_FOR)
    D("FINISH", S_FINISH)

    D("GOTO", S_goto)
    D("GE", S_GE)
    D("GR", S_GR)
    D("GLOBAL", S_GLOBAL)
    D("GET", S_GET)

    D("IF", S_IF)
    D("INTO", S_INTO)

    D("LET", S_LET)
    D("LV", S_LV)
    D("LE", S_LE)
    D("LS", S_LS)
    D("LOGOR", S_LOGOR)
    D("LOGAND", S_LOGAND)
    D("LOOP", S_LOOP)
    D("LSHIFT", S_LSHIFT)

    D("MANIFEST", S_MANIFEST)

    D("NE", S_NE)
    D("NOT", S_NOT)
    D("NEQV", S_NEQV)

    D("OR", S_OR)

    D("RESULTIS", S_RESULTIS)
    D("RETURN", S_RETURN)
    D("REM", S_REM)
    D("RSHIFT", S_RSHIFT)
    D("RV", S_RV)
    D("REPEAT", S_REPEAT)
    D("REPEATWHILE", S_REPEATWHILE)
    D("REPEATUNTIL", S_REPEATUNTIL)

    D("SWITCHON", S_SWITCHON)
    D("STATIC", S_STATIC)

    D("TO", S_TO)
    D("TEST", S_TEST)
    D("TRUE", S_TRUE)
    D("THEN", S_DO)
    D("TABLE", S_TABLE)

    D("UNTIL", S_UNTIL)
    D("UNLESS", S_UNLESS)

    D("VEC", S_VEC)
    D("VALOF", S_VALOF)

    D("WHILE", S_WHILE)

    D("$", 0);
    NULLTAG := WORDNODE;
}

AND LOOKUPWORD() = VALOF

{1     LET HASHVAL = (WORDV[0]+WORDV[WORDSIZE] >> 1) REM NAMETABLESIZE
        LET M = @NAMETABLE[HASHVAL]

  next: WORDNODE := *M
        UNLESS WORDNODE=0 DO
             {2 FOR I = 0 TO WORDSIZE DO
                   IF WORDNODE[I+2] NE WORDV[I] DO
                   { M := WORDNODE+1
                      goto next  }
                 RESULTIS WORDNODE[0]  }2

        WORDNODE := NEWVEC(WORDSIZE+2)
        WORDNODE[0], WORDNODE[1] := S_NAME, NAMETABLE[HASHVAL]
        FOR I = 0 TO WORDSIZE DO WORDNODE[I+2] := WORDV[I]
        NAMETABLE[HASHVAL] := WORDNODE
        RESULTIS S_NAME
}1

.

//    LEX3


GET "SYNHDR"

LET rch() BE
    { ch := getchar()

       IF prsource DO IF GETP=0 /\ ch NE ENDSTREAMCH DO
          { UNLESS linecount=PRLINE DO { writef("%I4  ", linecount)
                                           PRLINE := linecount  }
             putchar(ch)  }

       CHCOUNT := CHCOUNT + 1
       chbuf[CHCOUNT&63] := ch  }

AND wrchbuf() BE
    { writes("\n...")
       FOR P = CHCOUNT-63 TO CHCOUNT DO
                { LET K = chbuf[P&63]
                   UNLESS K=0 DO putchar(K)  }
       newline()  }


AND RDTAG(X) BE
    { CHARP, CHARV[1] := 1, X

        {  rch()
            UNLESS 'A'<=ch<='Z' \/
                   'a'<=ch<='z' \/
                   '0'<=ch<='9' \/
                    ch='.' break
            CHARP := CHARP+1
            CHARV[CHARP] := ch  } REPEAT

       CHARV[0] := CHARP
       WORDSIZE := packstring(CHARV, WORDV)  }


AND PERFORMGET() BE
    { nextsymb()
       UNLESS SYMB=S_STRING THEN CAEREPORT(97)

       IF option[5] RETURN

       GETV[GETP] := sourcestream
       GETV[GETP+1] := linecount
       GETV[GETP+2] := ch
       GETP := GETP + 3
       linecount := 1
       sourcestream := findinput(WORDV)
       IF sourcestream=0 THEN
           sourcestream := findlibinput(WORDV)
       IF sourcestream=0 THEN CAEREPORT(96,WORDV)
       selectinput(sourcestream)
       rch()   }

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
       P[0] := X
       RESULTIS P  }

AND LIST2(X, Y) = VALOF
     { LET P = NEWVEC(1)
        P[0], P[1] := X, Y
        RESULTIS P   }

AND LIST3(X, Y, Z) = VALOF
     { LET P = NEWVEC(2)
        P[0], P[1], P[2] := X, Y, Z
        RESULTIS P     }

AND LIST4(X, Y, Z, T) = VALOF
     { LET P = NEWVEC(3)
        P[0], P[1], P[2], P[3] := X, Y, Z, T
        RESULTIS P   }

AND LIST5(X, Y, Z, T, U) = VALOF
     { LET P = NEWVEC(4)
        P[0], P[1], P[2], P[3], P[4] := X, Y, Z, T, U
        RESULTIS P   }

AND LIST6(X, Y, Z, T, U, V) = VALOF
     { LET P = NEWVEC(5)
        P[0], P[1], P[2], P[3], P[4], P[5] := X, Y, Z, T, U, V
        RESULTIS P  }

AND CAEREPORT(N, A) BE
     { reportcount := reportcount + 1
        writef("*NSYNTAX ERROR NEAR LINE %N:  ", linecount)
        CAEMESSAGE(N, A)
        wrchbuf()
        IF reportcount GR reportmax DO
                    { writes('*NCOMPILATION ABORTED*N')
                       stop(8)   }
        nlpending := FALSE

        UNTIL SYMB=S_LSECT \/ SYMB=S_RSECT \/
              SYMB=S_LET \/ SYMB=S_AND \/
              SYMB=S_END \/ nlpending DO nextsymb()
        longjump(REC_P, REC_L)   }

AND formtree() =  VALOF
    {1 CHCOUNT := 0
        FOR I = 0 TO 63 DO chbuf[I] := 0

     { LET V = VEC 10   // FOR 'GET' STREAMS
        GETV, GETP, GETT := V, 0, 10

     { LET V = VEC 100
        WORDV := V

     { LET V = VEC 256
        CHARV, CHARP := V, 0

     { LET V = VEC 100
        NAMETABLE, NAMETABLESIZE := V, 100
        FOR I = 0 TO 100 DO NAMETABLE[I] := 0

        REC_P, REC_L := level(), L

        linecount, PRLINE := 1, 0
        rch()

        IF ch=ENDSTREAMCH RESULTIS 0
        DECLSYSWORDS()

     L: nextsymb()

        IF option[1] DO   //   PP DEBUGGING OPTION
             { writef("%N %S*N", SYMB, WORDV)
                IF SYMB=S_END RESULTIS 0
                goto L  }

     { LET A = RDBLOCKBODY()
        UNLESS SYMB=S_END DO { CAEREPORT(99); goto L  }

        RESULTIS A        }1



AND CAEMESSAGE(N, A) BE
    { LET S = VALOF

         SWITCHON N INTO

         { DEFAULT:  writen(N); RETURN

            case 91: RESULTIS "'8'  '(' OR ')' EXPECTED"
            case 94: RESULTIS "ILLEGAL CHARACTER"
            case 95: RESULTIS "STRING TOO LONG"
            case 96: RESULTIS "NO INPUT %S"
            case 97: RESULTIS "STRING OR NUMBER EXPECTED"
            case 98: RESULTIS "PROGRAM TOO LARGE"
            case 99: RESULTIS "INCORRECT TERMINATION"

            case 8: case 40: case 43:
                     RESULTIS "NAME EXPECTED"
            case 6: RESULTIS "'{' EXPECTED"
            case 7: RESULTIS "'}' EXPECTED"
            case 9: RESULTIS "UNTAGGED '}' MISMATCH"
            case 32: RESULTIS "ERROR IN EXPRESSION"
            case 33: RESULTIS "ERROR IN NUMBER"
            case 34: RESULTIS "BAD STRING"
            case 15: case 19: case 41: RESULTIS "')' MISSING"
            case 30: RESULTIS "',' MISSING"
            case 42: RESULTIS "'=' OR 'BE' EXPECTED"
            case 44: RESULTIS "'=' OR '(' EXPECTED"
            case 50: RESULTIS "ERROR IN LABEL"
            case 51: RESULTIS "ERROR IN COMMAND"
            case 54: RESULTIS "'OR' EXPECTED"
            case 57: RESULTIS "'=' EXPECTED"
            case 58: RESULTIS "'TO' EXPECTED"
            case 60: RESULTIS "'INTO' EXPECTED"
            case 61: case 62: RESULTIS "':' EXPECTED"
            case 63: RESULTIS "'**/' MISSING"
                       }

         writef(S, A)  }


.

//    CAE1


GET "SYNHDR"

LET RDBLOCKBODY() = VALOF
    {1 LET P, L = REC_P, REC_L
        LET A = 0

        REC_P, REC_L := level(), RECOVER

        IGNORE(S_SEMICOLON)

        SWITCHON SYMB INTO
     { case S_MANIFEST:
        case S_STATIC:
        case S_GLOBAL:
            {  LET OP = SYMB
                nextsymb()
                A := RDSECT(RDCDEFS)
                A := LIST3(OP, A, RDBLOCKBODY())
                goto RET  }


        case S_LET: nextsymb()
                    A := RDEF()
           RECOVER: WHILE SYMB=S_AND DO
                          { nextsymb()
                             A := LIST3(S_AND, A, RDEF())  }
                    A := LIST3(S_LET, A, RDBLOCKBODY())
                    goto RET

        DEFAULT: A := RDSEQ()

                 UNLESS SYMB=S_RSECT \/ SYMB=S_END DO
                          CAEREPORT(51)

        case S_RSECT: case S_END:
        RET:   REC_P, REC_L := P, L
               RESULTIS A   }1

AND RDSEQ() = VALOF
    { LET A = 0
       IGNORE(S_SEMICOLON)
       A := RCOM()
       IF SYMB=S_RSECT \/ SYMB=S_END RESULTIS A
       RESULTIS LIST3(S_SEQ, A, RDSEQ())   }


AND RDCDEFS() = VALOF
    {1 LET A, B = 0, 0
        LET PTR = @A
        LET P, L = REC_P, REC_L
        REC_P, REC_L := level(), RECOVER

        { B := RNAME()
           TEST SYMB=S_EQ \/ SYMB=S_COLON THEN nextsymb()
                                            OR CAEREPORT(45)
           *PTR := LIST4(S_CONSTDEF, 0, B, REXP(0))
           PTR := @H2[*PTR]
  RECOVER: IGNORE(S_SEMICOLON) } REPEATWHILE SYMB=S_NAME

        REC_P, REC_L := P, L
        RESULTIS A  }1

AND RDSECT(R) = VALOF
    {  LET TAG, A = WORDNODE, 0
        CHECKFOR(S_LSECT, 6)
        A := R()
        UNLESS SYMB=S_RSECT DO CAEREPORT(7)
        TEST TAG=WORDNODE
             THEN nextsymb()
               OR IF WORDNODE=NULLTAG DO
                      { SYMB := 0
                         CAEREPORT(9)  }
        RESULTIS A   }


AND RNAMELIST() = VALOF
    {  LET A = RNAME()
        UNLESS SYMB=S_COMMA RESULTIS A
        nextsymb()
        RESULTIS LIST3(S_COMMA, A, RNAMELIST())   }


AND RNAME() = VALOF
    { LET A = WORDNODE
       CHECKFOR(S_NAME, 8)
       RESULTIS A  }

AND IGNORE(ITEM) BE IF SYMB=ITEM DO nextsymb()

AND CHECKFOR(ITEM, N) BE
      { UNLESS SYMB=ITEM DO CAEREPORT(N)
         nextsymb()  }

.

//    CAE2


GET "SYNHDR"
LET RBEXP() = VALOF
  {1   LET A, OP = 0, SYMB

        SWITCHON SYMB INTO

    {  DEFAULT:
            CAEREPORT(32)

        case S_QUERY:
            nextsymb(); RESULTIS LIST1(S_QUERY)

        case S_TRUE:
        case S_FALSE:
        case S_NAME:
            A := WORDNODE
            nextsymb()
            RESULTIS A

        case S_STRING:
            A := NEWVEC(WORDSIZE+1)
            A[0] := S_STRING
            FOR I = 0 TO WORDSIZE DO A[I+1] := WORDV[I]
            nextsymb()
            RESULTIS A

        case S_NUMBER:
            A := LIST2(S_NUMBER, DECVAL)
            nextsymb()
            RESULTIS A

        case S_LPAREN:
            nextsymb()
            A := REXP(0)
            CHECKFOR(S_RPAREN, 15)
            RESULTIS A

        case S_VALOF:
            nextsymb()
            RESULTIS LIST2(S_VALOF, RCOM())

        case S_VECAP: OP := S_RV
        case S_LV:
        case S_RV: nextsymb(); RESULTIS LIST2(OP, REXP(35))

        case S_PLUS: nextsymb(); RESULTIS REXP(34)

        case S_MINUS: nextsymb()
                      A := REXP(34)
                      TEST H1[A]=S_NUMBER
                          THEN H2[A] := - H2[A]
                            OR A := LIST2(S_NEG, A)
                      RESULTIS A

        case S_NOT: nextsymb(); RESULTIS LIST2(S_NOT, REXP(24))

        case S_TABLE: nextsymb()
                      RESULTIS LIST2(S_TABLE, REXPLIST())   }1



AND REXP(N) = VALOF
    {1 LET A = RBEXP()

        LET B, C, P, Q = 0, 0, 0, 0

  L: { LET OP = SYMB

        IF nlpending RESULTIS A

        SWITCHON OP INTO
    {B DEFAULT: RESULTIS A

        case S_LPAREN: nextsymb()
                       B := 0
                       UNLESS SYMB=S_RPAREN DO B := REXPLIST()
                       CHECKFOR(S_RPAREN, 19)
                       A := LIST3(S_FNAP, A, B)
                       goto L

        case S_VECAP: P := 40; goto LASSOC

        case S_REM: case S_MULT: case S_DIV: P := 35; goto LASSOC

        case S_PLUS: case S_MINUS: P := 34; goto LASSOC

        case S_EQ: case S_NE:
        case S_LE: case S_GE:
        case S_LS: case S_GR:
                IF N>=30 RESULTIS A

            {R nextsymb()
                B := REXP(30)
                A := LIST3(OP, A, B)
                TEST C=0 THEN C :=  A
                           OR C := LIST3(S_LOGAND, C, A)
                A, OP := B, SYMB  }R REPEATWHILE S_EQ<=OP<=S_GE

                A := C
                goto L

        case S_LSHIFT: case S_RSHIFT: P, Q := 25, 30; goto DIADIC

        case S_LOGAND: P := 23; goto LASSOC

        case S_LOGOR: P := 22; goto LASSOC

        case S_EQV: case S_NEQV: P := 21; goto LASSOC

        case S_COND:
                IF N>=13 RESULTIS A
                nextsymb()
                B := REXP(0)
                CHECKFOR(S_COMMA, 30)
                A := LIST4(S_COND, A, B, REXP(0))
                goto L

        LASSOC: Q := P

        DIADIC: IF N>=P RESULTIS A
                nextsymb()
                A := LIST3(OP, A, REXP(Q))
                goto L                     }B     }1

LET REXPLIST() = VALOF
    {1 LET A = 0
        LET PTR = @A

     { LET B = REXP(0)
        UNLESS SYMB=S_COMMA DO { *PTR := B
                                  RESULTIS A  }
        nextsymb()
        *PTR := LIST3(S_COMMA, B, 0)
        PTR := @H3[*PTR]  } REPEAT
    }1

LET RDEF() = VALOF
    {1 LET N = RNAMELIST()

        SWITCHON SYMB INTO

     { case S_LPAREN:
             { LET A = 0
                nextsymb()
                UNLESS H1[N]=S_NAME DO CAEREPORT(40)
                IF SYMB=S_NAME DO A := RNAMELIST()
                CHECKFOR(S_RPAREN, 41)

                IF SYMB=S_BE DO
                     { nextsymb()
                        RESULTIS LIST5(S_RTDEF, N, A, RCOM(), 0)  }

                IF SYMB=S_EQ DO
                     { nextsymb()
                        RESULTIS LIST5(S_FNDEF, N, A, REXP(0), 0)  }

                CAEREPORT(42)  }

        DEFAULT: CAEREPORT(44)

        case S_EQ:
                nextsymb()
                IF SYMB=S_VEC DO
                     { nextsymb()
                        UNLESS H1[N]=S_NAME DO CAEREPORT(43)
                        RESULTIS LIST3(S_VECDEF, N, REXP(0))  }
                RESULTIS LIST3(S_VALDEF, N, REXPLIST())  }1

.


//    CAE4



GET "SYNHDR"

LET RBCOM() = VALOF
   {1 LET A, B, OP = 0, 0, SYMB

        SWITCHON SYMB INTO
     { DEFAULT: RESULTIS 0

        case S_NAME: case S_NUMBER: case S_STRING:
        case S_TRUE: case S_FALSE: case S_LV: case S_RV: case S_VECAP:
        case S_LPAREN:
                A := REXPLIST()

                IF SYMB=S_ASS  THEN
                    {  OP := SYMB
                        nextsymb()
                        RESULTIS LIST3(OP, A, REXPLIST())  }

                IF SYMB=S_COLON DO
                     { UNLESS H1[A]=S_NAME DO CAEREPORT(50)
                        nextsymb()
                        RESULTIS LIST4(S_COLON, A, RBCOM(), 0)  }

                IF H1[A]=S_FNAP DO
                     { H1[A] := S_RTAP
                        RESULTIS A  }

                CAEREPORT(51)
                RESULTIS A

        case S_GOTO: case S_RESULTIS:
                nextsymb()
                RESULTIS LIST2(OP, REXP(0))

        case S_IF: case S_UNLESS:
        case S_WHILE: case S_UNTIL:
                nextsymb()
                A := REXP(0)
                IGNORE(S_DO)
                RESULTIS LIST3(OP, A, RCOM())

        case S_TEST:
                nextsymb()
                A := REXP(0)
                IGNORE(S_DO)
                B := RCOM()
                CHECKFOR(S_OR, 54)
                RESULTIS LIST4(S_TEST, A, B, RCOM())

        case S_FOR:
            {  LET I, J, K = 0, 0, 0
                nextsymb()
                A := RNAME()
                CHECKFOR(S_EQ, 57)
                I := REXP(0)
                CHECKFOR(S_TO, 58)
                J := REXP(0)
                IF SYMB=S_BY DO { nextsymb()
                                   K := REXP(0)  }
                IGNORE(S_DO)
                RESULTIS LIST6(S_FOR, A, I, J, K, RCOM())  }

        case S_LOOP:
        case S_BREAK: case S_RETURN: case S_FINISH: case S_ENDcase:
                A := WORDNODE
                nextsymb()
                RESULTIS A

        case S_SWITCHON:
                nextsymb()
                A := REXP(0)
                CHECKFOR(S_INTO, 60)
                RESULTIS LIST3(S_SWITCHON, A, RDSECT(RDSEQ))

        case S_case:
                nextsymb()
                A := REXP(0)
                CHECKFOR(S_COLON, 61)
                RESULTIS LIST3(S_case, A, RBCOM())

        case S_DEFAULT:
                nextsymb()
                CHECKFOR(S_COLON, 62)
                RESULTIS LIST2(S_DEFAULT, RBCOM())

        case S_LSECT:
                RESULTIS RDSECT(RDBLOCKBODY)   }1


AND RCOM() = VALOF
    {1 LET A = RBCOM()

        IF A=0 DO CAEREPORT(51)

        WHILE SYMB=S_REPEAT \/ SYMB=S_REPEATWHILE \/
                    SYMB=S_REPEATUNTIL DO
                  { LET OP = SYMB
                     nextsymb()
                     TEST OP=S_REPEAT
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

        SWITCHON H1[X] INTO
    {  case S_NUMBER: writen(H2[X]); RETURN

        case S_NAME: writes(X+2); RETURN

        case S_STRING: writef("*"%S*"", X+1); RETURN

        case S_FOR:
                SIZE := SIZE + 2

        case S_COND: case S_FNDEF: case S_RTDEF:
        case S_TEST: case S_CONSTDEF:
                SIZE := SIZE + 1

        case S_VECAP: case S_FNAP:
        case S_MULT: case S_DIV: case S_REM: case S_PLUS: case S_MINUS:
        case S_EQ: case S_NE: case S_LS: case S_GR: case S_LE: case S_GE:
        case S_LSHIFT: case S_RSHIFT: case S_LOGAND: case S_LOGOR:
        case S_EQV: case S_NEQV: case S_COMMA:
        case S_AND: case S_VALDEF: case S_VECDEF:
        case S_ASS: case S_RTAP: case S_COLON: case S_IF: case S_UNLESS:
        case S_WHILE: case S_UNTIL: case S_REPEATWHILE:
        case S_REPEATUNTIL:
        case S_SWITCHON: case S_CASE: case S_SEQ: case S_LET:
        case S_MANIFEST: case S_STATIC: case S_GLOBAL:
                SIZE := SIZE + 1

        case S_VALOF: case S_LV: case S_RV: case S_NEG: case S_NOT:
        case S_TABLE: case S_GOTO: case S_RESULTIS: case S_REPEAT:
        case S_DEFAULT:
                SIZE := SIZE + 1

        case S_LOOP:
        case S_BREAK: case S_RETURN: case S_FINISH: case S_ENDcase:
        case S_TRUE: case S_FALSE: case S_QUERY:
        DEFAULT:
                SIZE := SIZE + 1

                IF N=D DO { writes("ETC")
                             RETURN  }

                writes ("OP")
                writen(H1[X])
                FOR I = 2 TO SIZE DO
                     { newline()
                        FOR J=0 TO N-1 DO writes( V[J] )
                        writes("**-")
                        V[N] := (I == SIZE_ ? "  " : "! ";
                        plist(H1[X+I-1], N+1, D)  }
                RETURN  }1
//    TRN0

GET "TRNHDR"

LET nextparam() = VALOF
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

       case 141: RESULTIS "TOO MANY caseS"
       case 104: RESULTIS "ILLEGAL USE OF BREAK, LOOP OR RESULTIS"
       case 101:
       case 105: RESULTIS "ILLEGAL USE OF case OR DEFAULT"
       case 106: RESULTIS "TWO caseS WITH SAME CONSTANT"
       case 144: RESULTIS "TOO MANY GLOBALS"
       case 142: RESULTIS "NAME DECLARED TWICE"
       case 143: RESULTIS "TOO MANY NAMES DECLARED"
       case 115: RESULTIS "NAME NOT DECLARED"
       case 116: RESULTIS "DYNAMIC FREE VARIABLE USED"
       case 117: case 118: case 119:
                 RESULTIS "ERROR IN CONSTANT EXPRESSION"
       case 110: case 112:
                 RESULTIS "LHS AND RHS DO NOT MATCH"
       case 109: case 113:
                 RESULTIS "LTYPE EXPRESSION EXPECTED"
                   }

   writes(S)   }


LET compileae(X) BE
   {1 LET A = VEC 1200
       LET D = VEC 100
       LET K = VEC 150
       LET L = VEC 150

       DVEC, DVECS, DVECE, DVECP, DVECT := A, 3, 3, 3, 1200
       DVEC[0], DVEC[1], DVEC[2] := 0, 0, 0

       GLOBDECL, GLOBDECLS, GLOBDECLT := D, 0, 100

       caseK, caseL, caseP, caseT, caseB := K, L, 0, 150, -1
       ENDcaseLABEL, DEFAULTLABEL := 0, 0

       RESULTLABEL, breaklabel, LOOPLABEL := -1, -1, -1

       COMCOUNT, CURRENTBRANCH := 0, X

       OCOUNT := 0

       PARAMNUMBER := 0
       SSP := savespacesize
       OUT2(S_STACK, SSP)
       DECLLABELS(X)
       TRANS(X)
       OUT2(S_GLOBAL, GLOBDECLS/2)

    { LET I = 0
       UNTIL I=GLOBDECLS DO
          { OUTN(GLOBDECL[I])
             OUTL(GLOBDECL[I+1])
             I := I + 2  }

       ENDOCODE()  }1

.

//    TRN1


GET "TRNHDR"

LET TRANS(X) BE
  {TR
next:
 { LET SW = FALSE
    IF X=0 RETURN
    CURRENTBRANCH := X

    SWITCHON H1[X] INTO
{  DEFAULT: TRANSREPORT(100, X); RETURN

    case S_LET:
      { LET A, B, S, S1 = DVECE, DVECS, SSP, 0
         LET V = VECSSP
         DECLNAMES(H2[X])
         CHECKDISTINCT(B, DVECS)
         DVECE := DVECS
         VECSSP, S1 := SSP, SSP
         SSP := S
         TRANSDEF(H2[X])
         UNLESS SSP=S1 DO TRANSREPORT(110, X)
         UNLESS SSP=VECSSP DO { SSP := VECSSP
                                 OUT2(S_STACK, SSP)  }
         OUT1(S_STORE)
         DECLLABELS(H3[X])
         TRANS(H3[X])
         VECSSP := V
         UNLESS SSP=S DO OUT2(S_STACK, S)
         DVECE, DVECS, SSP := A, B, S
         RETURN   }

    case S_STATIC:
    case S_GLOBAL:
    case S_MANIFEST:
     {1 LET A, B, S = DVECE, DVECS, SSP
         AND OP = H1[X]
         AND Y = H2[X]

         IF OP=S_MANIFEST DO OP := S_NUMBER

         UNTIL Y=0 DO
           { TEST OP=S_STATIC THEN
                { LET M = nextparam()
                   ADDNAME(H3[Y], S_LABEL, M)
                   COMPDATALAB(M)
                   OUT2(S_ITEMN, EVALCONST(H4[Y]))  }

                OR ADDNAME(H3[Y], OP, EVALCONST(H4[Y]))

              Y := H2[Y]
              DVECE := DVECS  }

         DECLLABELS(H3[X])
         TRANS(H3[X])
         DVECE, DVECS, SSP := A, B, S
         RETURN   }1


    case S_ASS:
       ASSIGN(H2[X], H3[X])
       RETURN

    case S_RTAP:
     { LET S = SSP
        SSP := SSP+savespacesize
        OUT2(S_STACK, SSP)
        LOADLIST(H3[X])
        LOAD(H2[X])
        OUT2(S_RTAP, S)
        SSP := S
        RETURN  }

    case S_goto:
        LOAD(H2[X])
        OUT1(S_goto)
        SSP := SSP-1
        RETURN

    case S_COLON:
        COMPLAB(H4[X])
        TRANS(H3[X])
        RETURN

    case S_UNLESS: SW := TRUE
    case S_IF:
     { LET L = nextparam()
        JUMPCOND(H2[X], SW, L)
        TRANS(H3[X])
        COMPLAB(L)
        RETURN   }

    case S_TEST:
     { LET L, M = nextparam(), nextparam()
        JUMPCOND(H2[X], FALSE, L)
        TRANS(H3[X])
        COMPJUMP(M)
        COMPLAB(L)
        TRANS(H4[X])
        COMPLAB(M)
        RETURN   }

    case S_LOOP:
        IF LOOPLABEL<0 DO TRANSREPORT(104, X)
        IF LOOPLABEL=0 DO LOOPLABEL := nextparam()
        COMPJUMP(LOOPLABEL)
        RETURN

    case S_BREAK:
        IF breaklabel<0 DO TRANSREPORT(104, X)
        IF breaklabel=0 DO breaklabel := nextparam()
        COMPJUMP(breaklabel)
        RETURN

    case S_RETURN: OUT1(S_RTRN)
                   RETURN

    case S_FINISH: OUT1(S_FINISH)
                   RETURN

    case S_RESULTIS:
        IF RESULTLABEL<0 DO TRANSREPORT(104, X)
        LOAD(H2[X])
        OUT2P(S_RES, RESULTLABEL)
        SSP := SSP - 1
        RETURN

    case S_WHILE: SW := TRUE
    case S_UNTIL:
     { LET L, M = nextparam(), nextparam()
        LET BL, LL = breaklabel, LOOPLABEL
        breaklabel, LOOPLABEL := 0, M

        COMPJUMP(M)
        COMPLAB(L)
        TRANS(H3[X])
        COMPLAB(M)
        JUMPCOND(H2[X], SW, L)
        UNLESS breaklabel=0 DO COMPLAB(breaklabel)
        breaklabel, LOOPLABEL := BL, LL
        RETURN   }

    case S_REPEATWHILE: SW := TRUE
    case S_REPEATUNTIL:
    case S_REPEAT:
     { LET L, BL, LL = nextparam(), breaklabel, LOOPLABEL
        breaklabel, LOOPLABEL := 0, 0
        COMPLAB(L)
        TEST H1[X]=S_REPEAT
            THEN { LOOPLABEL := L
                    TRANS(H2[X])
                    COMPJUMP(L)  }
              OR { TRANS(H2[X])
                    UNLESS LOOPLABEL=0 DO COMPLAB(LOOPLABEL)
                    JUMPCOND(H3[X], SW, L)  }
        UNLESS breaklabel=0 DO COMPLAB(breaklabel)
        breaklabel, LOOPLABEL := BL, LL
        RETURN   }

    case S_case:
     { LET L, K = nextparam(), EVALCONST(H2[X])
        IF caseP>=caseT DO TRANSREPORT(141, X)
        IF caseB<0 DO TRANSREPORT(105, X)
        FOR I = caseB TO caseP-1 DO
                    IF caseK[I]=K DO TRANSREPORT(106, X)
        caseK[caseP] := K
        caseL[caseP] := L
        caseP := caseP + 1
        COMPLAB(L)
        TRANS(H3[X])
        RETURN   }

    case S_DEFAULT:
        IF caseB<0 DO TRANSREPORT(105, X)
        UNLESS DEFAULTLABEL=0 DO TRANSREPORT(101, X)
        DEFAULTLABEL := nextparam()
        COMPLAB(DEFAULTLABEL)
        TRANS(H2[X])
        RETURN

    case S_ENDcase: IF caseB<0 DO TRANSREPORT(105, X)
                    COMPJUMP(ENDcaseLABEL)
                    RETURN

    case S_SWITCHON:
        TRANSSWITCH(X)
        RETURN

    case S_FOR: TRANSFOR(X)
                RETURN

    case S_SEQ:
        TRANS(H2[X])
        COMCOUNT :=  COMCOUNT + 1
        X := H3[X]
        goto next        }TR
.

//    TRN2


GET "TRNHDR"

LET DECLNAMES(X) BE UNLESS X=0 SWITCHON H1[X] INTO

     {  DEFAULT: TRANSREPORT(102, CURRENTBRANCH)
                  RETURN

         case S_VECDEF: case S_VALDEF:
               DECLDYN(H2[X])
               RETURN

         case S_RTDEF: case S_FNDEF:
               H5[X] := nextparam()
               DECLSTAT(H2[X], H5[X])
               RETURN

         case S_AND:
               DECLNAMES(H2[X])
               DECLNAMES(H3[X])
               RETURN    }


AND DECLDYN(X) BE UNLESS X=0 DO

    { IF H1[X]=S_NAME DO
          { ADDNAME(X, S_LOCAL, SSP)
             SSP := SSP + 1
             RETURN   }

       IF H1[X]=S_COMMA DO
          { ADDNAME(H2[X], S_LOCAL, SSP)
             SSP := SSP + 1
             DECLDYN(H3[X])
             RETURN  }

       TRANSREPORT(103, X)   }

AND DECLSTAT(X, L) BE
    {1 LET T = CELLWITHNAME(X)

       IF DVEC[T+1]=S_GLOBAL DO
          { LET N = DVEC[T+2]
             ADDNAME(X, S_GLOBAL, N)
             IF GLOBDECLS>=GLOBDECLT DO TRANSREPORT(144, X)
             GLOBDECL[GLOBDECLS] := N
             GLOBDECL[GLOBDECLS+1] := L
             GLOBDECLS := GLOBDECLS + 2
             RETURN  }


    { LET M = nextparam()
       ADDNAME(X, S_LABEL, M)
       COMPDATALAB(M)
       OUT2P(S_ITEML, L)    }1


AND DECLLABELS(X) BE
    { LET B = DVECS
       SCANLABELS(X)
       CHECKDISTINCT(B, DVECS)
       DVECE := DVECS   }


AND CHECKDISTINCT(E, S) BE
       UNTIL E=S DO
          { LET P = E + 3
             AND N = DVEC[E]
             WHILE P<S DO
                { IF DVEC[P]=N DO TRANSREPORT(142, N)
                   P := P + 3  }
             E := E + 3  }


AND ADDNAME(N, P, A) BE
    { IF DVECS>=DVECT DO TRANSREPORT(143, CURRENTBRANCH)
       DVEC[DVECS], DVEC[DVECS+1], DVEC[DVECS+2] := N, P, A
       DVECS := DVECS + 3  }


AND CELLWITHNAME(N) = VALOF
    { LET X = DVECE

       X := X - 3 REPEATUNTIL X=0 \/ DVEC[X]=N

       RESULTIS X  }


AND SCANLABELS(X) BE UNLESS X=0 SWITCHON H1[X] INTO

    { DEFAULT: RETURN

       case S_COLON:
            H4[X] := nextparam()
            DECLSTAT(H2[X], H4[X])

       case S_IF: case S_UNLESS: case S_WHILE: case S_UNTIL:
       case S_SWITCHON: case S_case:
            SCANLABELS(H3[X])
            RETURN

       case S_SEQ:
            SCANLABELS(H3[X])

       case S_REPEAT:
       case S_REPEATWHILE: case S_REPEATUNTIL: case S_DEFAULT:
            SCANLABELS(H2[X])
            RETURN

       case S_TEST:
            SCANLABELS(H3[X])
            SCANLABELS(H4[X])
            RETURN    }


AND TRANSDEF(X) BE
    {1 TRANSDYNDEFS(X)
        IF STATDEFS(X) DO
           { LET L, S= nextparam(), SSP
              COMPJUMP(L)
              TRANSSTATDEFS(X)
              SSP := S
              OUT2(S_STACK, SSP)
              COMPLAB(L)  }1


AND TRANSDYNDEFS(X) BE
        SWITCHON H1[X] INTO
     { case S_AND:
            TRANSDYNDEFS(H2[X])
            TRANSDYNDEFS(H3[X])
            RETURN

        case S_VECDEF:
            OUT2(S_LLP, VECSSP)
            SSP := SSP + 1
            VECSSP := VECSSP + 1 + EVALCONST(H3[X])
            RETURN

        case S_VALDEF: LOADLIST(H3[X])
                       RETURN

        DEFAULT: RETURN  }

AND TRANSSTATDEFS(X) BE
        SWITCHON H1[X] INTO
     { case S_AND:
             TRANSSTATDEFS(H2[X])
             TRANSSTATDEFS(H3[X])
             RETURN

        case S_FNDEF: case S_RTDEF:
         {2 LET A, B, C = DVECE, DVECS, DVECP
             AND BL, LL = breaklabel, LOOPLABEL
             AND RL, CB = RESULTLABEL, caseB
             breaklabel, LOOPLABEL := -1, -1
             RESULTLABEL, caseB := -1, -1

             COMPENTRY(H2[X], H5[X])
             SSP := savespacesize

             DVECP := DVECS
             DECLDYN(H3[X])
             CHECKDISTINCT(B, DVECS)
             DVECE := DVECS
             DECLLABELS(H4[X])

             OUT2(S_SAVE, SSP)

             TEST H1[X]=S_FNDEF
                THEN { LOAD(H4[X]); OUT1(S_FNRN)  }
                  OR { TRANS(H4[X]); OUT1(S_RTRN)  }

             OUT2(S_ENDPROC, 0)

             breaklabel, LOOPLABEL := BL, LL
             RESULTLABEL, caseB := RL, CB
             DVECE, DVECS, DVECP := A, B, C   }2

        DEFAULT: RETURN   }

AND STATDEFS(X) = H1[X]=S_FNDEF \/ H1[X]=S_RTDEF -> TRUE,
                  H1[X] NE S_AND -> FALSE,
                  STATDEFS(H2[X]) -> TRUE,
                  STATDEFS(H3[X])


.

//    TRN3


GET "TRNHDR"

LET JUMPCOND(X, B, L) BE
{JC LET SW = B
     SWITCHON H1[X] INTO
     { case S_FALSE: B := NOT B
        case S_TRUE: IF B DO COMPJUMP(L)
                     RETURN

        case S_NOT: JUMPCOND(H2[X], NOT B, L)
                    RETURN

        case S_LOGAND: SW := NOT SW
        case S_LOGOR:
         TEST SW THEN { JUMPCOND(H2[X], B, L)
                         JUMPCOND(H3[X], B, L)  }

                   OR { LET M = nextparam()
                         JUMPCOND(H2[X], NOT B, M)
                         JUMPCOND(H3[X], B, L)
                         COMPLAB(M)  }

         RETURN

        DEFAULT: LOAD(X)
                 OUT2P(B -> S_JT, S_JF, L)
                 SSP := SSP - 1
                 RETURN     }JC

AND TRANSSWITCH(X) BE
    {1 LET P, B, DL = caseP, caseB, DEFAULTLABEL
        AND ECL = ENDcaseLABEL
        LET L = nextparam()
        ENDcaseLABEL := nextparam()
        caseB := caseP

        COMPJUMP(L)
        DEFAULTLABEL := 0
        TRANS(H3[X])
        COMPJUMP(ENDcaseLABEL)

        COMPLAB(L)
        LOAD(H2[X])
        IF DEFAULTLABEL=0 DO DEFAULTLABEL := ENDcaseLABEL
        OUT3P(S_SWITCHON, caseP-P, DEFAULTLABEL)

        FOR I = caseB TO caseP-1 DO { OUTN(caseK[I])
                                       OUTL(caseL[I])  }

        SSP := SSP - 1
        COMPLAB(ENDcaseLABEL)
        ENDcaseLABEL := ECL
        caseP, caseB, DEFAULTLABEL := P, B, DL   }1

AND TRANSFOR(X) BE
     { LET A, B = DVECE, DVECS
        LET L, M = nextparam(), nextparam()
        LET BL, LL = breaklabel, LOOPLABEL
        LET K, N = 0, 0
        LET STEP = 1
        LET S = SSP
        breaklabel, LOOPLABEL := 0, 0

        ADDNAME(H2[X], S_LOCAL, S)
        DVECE := DVECS
        LOAD(H3[X])

        TEST H1[H4[X]]=S_NUMBER
            THEN K, N := S_LN, H2[H4[X]]
              OR { K, N := S_LP, SSP
                    LOAD(H4[X])  }

        UNLESS H5[X]=0 DO STEP := EVALCONST(H5[X])

        OUT1(S_STORE)
        COMPJUMP(L)
        DECLLABELS(H6[X])
        COMPLAB(M)
        TRANS(H6[X])
        UNLESS LOOPLABEL=0 DO COMPLAB(LOOPLABEL)
        OUT2(S_LP, S); OUT2(S_LN, STEP); OUT1(S_PLUS); OUT2(S_SP, S)
        COMPLAB(L)
        OUT2(S_LP, S); OUT2(K, N); OUT1(STEP<0 -> S_GE, S_LE)
        OUT2P(S_JT, M)

        UNLESS breaklabel=0 DO COMPLAB(breaklabel)
        breaklabel, LOOPLABEL, SSP := BL, LL, S
        OUT2(S_STACK, SSP)
        DVECE, DVECS := A, B  }

.

//    TRN4


GET "TRNHDR"

LET LOAD(X) BE
    {1 IF X=0 DO { TRANSREPORT(148, CURRENTBRANCH)
                     LOADZERO()
                     RETURN  }

     { LET OP = H1[X]

        SWITCHON OP INTO
     { DEFAULT: TRANSREPORT(147, CURRENTBRANCH)
                 LOADZERO()
                 RETURN

        case S_DIV: case S_REM: case S_MINUS:
        case S_LS: case S_GR: case S_LE: case S_GE:
        case S_LSHIFT: case S_RSHIFT:
            LOAD(H2[X])
            LOAD(H3[X])
            OUT1(OP)
            SSP := SSP - 1
            RETURN

        case S_VECAP: case S_MULT: case S_PLUS: case S_EQ: case S_NE:
        case S_LOGAND: case S_LOGOR: case S_EQV: case S_NEQV:
         { LET A, B = H2[X], H3[X]
            IF H1[A]=S_NAME \/ H1[A]=S_NUMBER DO
                               A, B := H3[X], H2[X]
            LOAD(A)
            LOAD(B)
            IF OP=S_VECAP DO { OUT1(S_PLUS); OP := S_RV  }
            OUT1(OP)
            SSP := SSP - 1
            RETURN   }

        case S_NEG: case S_NOT: case S_RV:
            LOAD(H2[X])
            OUT1(OP)
            RETURN

        case S_TRUE: case S_FALSE: case S_QUERY:
            OUT1(OP)
            SSP := SSP + 1
            RETURN

        case S_LV: LOADLV(H2[X])
                   RETURN

        case S_NUMBER:
            OUT2(S_LN, H2[X])
            SSP := SSP + 1
            RETURN

        case S_STRING:
         { LET S = @H2[X]
            OUT2(S_LSTR, getbyte(S, 0))
            FOR I = 1 TO getbyte(S, 0) DO OUTC(getbyte(S, I))
            WRC('*S')
            SSP := SSP + 1
            RETURN   }

        case S_NAME:
             TRANSNAME(X, S_LP, S_LG, S_LL, S_LN)
             SSP := SSP + 1
             RETURN

        case S_VALOF:
         { LET RL = RESULTLABEL
            LET A, B = DVECS, DVECE
            DECLLABELS(H2[X])
            RESULTLABEL := nextparam()
            TRANS(H2[X])
            COMPLAB(RESULTLABEL)
            OUT2(S_RSTACK, SSP)
            SSP := SSP + 1
            DVECS, DVECE := A, B
            RESULTLABEL := RL
            RETURN   }


        case S_FNAP:
         { LET S = SSP
            SSP := SSP + savespacesize
            OUT2(S_STACK, SSP)
            LOADLIST(H3[X])
            LOAD(H2[X])
            OUT2(S_FNAP, S)
            SSP := S + 1
            RETURN   }

        case S_COND:
         { LET L, M = nextparam(), nextparam()
            LET S = SSP
            JUMPCOND(H2[X], FALSE, M)
            LOAD(H3[X])
            COMPJUMP(L)
            SSP := S; OUT2(S_STACK, SSP)
            COMPLAB(M)
            LOAD(H4[X])
            COMPLAB(L)
            RETURN   }

        case S_TABLE:
         { LET M = nextparam()
            COMPDATALAB(M)
            X := H2[X]
            WHILE H1[X]=S_COMMA DO
                  { OUT2(S_ITEMN, EVALCONST(H2[X]))
                     X := H3[X]   }
            OUT2(S_ITEMN, EVALCONST(X))
            OUT2P(S_LLL, M)
            SSP := SSP + 1
            RETURN  }                         }1


AND LOADLV(X) BE
    {1 IF X=0 goto ERR

        SWITCHON H1[X] INTO
     { DEFAULT:
        ERR:     TRANSREPORT(113, CURRENTBRANCH)
                 LOADZERO()
                 RETURN

        case S_NAME:
              TRANSNAME(X, S_LLP, S_LLG, S_LLL, 0)
              SSP := SSP + 1
              RETURN

        case S_RV:
            LOAD(H2[X])
            RETURN

        case S_VECAP:
         { LET A, B = H2[X], H3[X]
            IF H1[A]=S_NAME DO A, B := H3[X], H2[X]
            LOAD(A)
            LOAD(B)
            OUT1(S_PLUS)
            SSP := SSP - 1
            RETURN   }  }1

AND LOADZERO() BE { OUT2(S_LN, 0)
                     SSP := SSP + 1  }

AND LOADLIST(X) BE UNLESS X=0 DO
    { UNLESS H1[X]=S_COMMA DO { LOAD(X); RETURN  }

       LOADLIST(H2[X])
       LOADLIST(H3[X])  }
.

//    TRN5


GET "TRNHDR"

LET EVALCONST(X) = VALOF
    {1 IF X=0 DO { TRANSREPORT(117, CURRENTBRANCH)
                     RESULTIS 0  }

        SWITCHON H1[X] INTO
     { DEFAULT: TRANSREPORT(118, X)
                 RESULTIS 0

        case S_NAME:
         { LET T = CELLWITHNAME(X)
            IF DVEC[T+1]=S_NUMBER RESULTIS DVEC[T+2]
            TRANSREPORT(119, X)
            RESULTIS 0  }

        case S_NUMBER: RESULTIS H2[X]
        case S_TRUE: RESULTIS TRUE
        case S_FALSE: RESULTIS FALSE

        case S_NEG: RESULTIS - EVALCONST(H2[X])

        case S_MULT: RESULTIS EVALCONST(H2[X]) * EVALCONST(H3[X])
        case S_DIV:  RESULTIS EVALCONST(H2[X]) / EVALCONST(H3[X])
        case S_PLUS: RESULTIS EVALCONST(H2[X]) + EVALCONST(H3[X])
        case S_MINUS:RESULTIS EVALCONST(H2[X]) - EVALCONST(H3[X])
                    }1


AND ASSIGN(X, Y) BE
    {1 IF X=0 \/ Y=0 DO
            { TRANSREPORT(110, CURRENTBRANCH)
               RETURN  }

        SWITCHON H1[X] INTO
     { case S_COMMA:
            UNLESS H1[Y]=S_COMMA DO
                       { TRANSREPORT(112, CURRENTBRANCH)
                          RETURN   }
            ASSIGN(H2[X], H2[Y])
            ASSIGN(H3[X], H3[Y])
            RETURN

        case S_NAME:
            LOAD(Y)
            TRANSNAME(X, S_SP, S_SG, S_SL, 0)
            SSP := SSP - 1
            RETURN

        case S_RV: case S_VECAP: case S_COND:
            LOAD(Y)
            LOADLV(X)
            OUT1(S_STIND)
            SSP := SSP - 2
            RETURN

        DEFAULT: TRANSREPORT(109, CURRENTBRANCH)   }1


AND TRANSNAME(X, P, G, L, N) BE
    {1 LET T = CELLWITHNAME(X)
        LET K, A = DVEC[T+1], DVEC[T+2]

        IF T=0 DO { TRANSREPORT(115, X)
                     OUT2(G, 2)
                     RETURN  }

        SWITCHON K INTO
        { case S_LOCAL: IF T<DVECP DO TRANSREPORT(116, X)
                         OUT2(P, A); RETURN

           case S_GLOBAL: OUT2(G, A); RETURN

           case S_LABEL: OUT2P(L, A); RETURN

           case S_NUMBER: IF N=0 DO { TRANSREPORT(113, X)
                                       N := P  }
                          OUT2(N, A)  }1

.

//    TRN6


GET "TRNHDR"

LET COMPLAB(L) BE OUT2P(S_LAB, L)

AND COMPENTRY(N, L) BE
    {  LET S = @N[2]
        OUT3P(S_ENTRY, getbyte(S, 0), L)
        FOR I = 1 TO getbyte(S, 0) DO OUTC(getbyte(S, I))
        WRC('*S')  }

AND COMPDATALAB(L) BE OUT2P(S_DATALAB, L)

AND COMPJUMP(L) BE OUT2P(S_JUMP, L)

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

           case S_MULT:    RESULTIS "MULT"
           case S_DIV:     RESULTIS "DIV"
           case S_REM:     RESULTIS "REM"
           case S_PLUS:    RESULTIS "PLUS"
           case S_MINUS:   RESULTIS "MINUS"
           case S_EQ:      RESULTIS "EQ"
           case S_NE:      RESULTIS "NE"
           case S_LS:      RESULTIS "LS"
           case S_GR:      RESULTIS "GR"
           case S_LE:      RESULTIS "LE"
           case S_GE:      RESULTIS "GE"
           case S_LSHIFT:  RESULTIS "LSHIFT"
           case S_RSHIFT:  RESULTIS "RSHIFT"
           case S_LOGAND:  RESULTIS "LOGAND"
           case S_LOGOR:   RESULTIS "LOGOR"
           case S_EQV:     RESULTIS "EQV"
           case S_NEQV:    RESULTIS "NEQV"

           case S_NEG:     RESULTIS "NEG"
           case S_NOT:     RESULTIS "NOT"
           case S_RV:      RESULTIS "RV"

           case S_TRUE:    RESULTIS "TRUE"
           case S_FALSE:   RESULTIS "FALSE"
           case S_QUERY:   RESULTIS "QUERY"

           case S_LP:      RESULTIS "LP"
           case S_LG:      RESULTIS "LG"
           case S_LN:      RESULTIS "LN"
           case S_LSTR:    RESULTIS "LSTR"
           case S_LL:      RESULTIS "LL"

           case S_LLP:     RESULTIS "LLP"
           case S_LLG:     RESULTIS "LLG"
           case S_LLL:     RESULTIS "LLL"

           case S_SP:      RESULTIS "SP"
           case S_SG:      RESULTIS "SG"
           case S_SL:      RESULTIS "SL"
           case S_STIND:   RESULTIS "STIND"

           case S_JUMP:    RESULTIS "JUMP"
           case S_JT:      RESULTIS "JT"
           case S_JF:      RESULTIS "JF"
           case S_goto:    RESULTIS "goto"
           case S_LAB:     RESULTIS "LAB"
           case S_STACK:   RESULTIS "STACK"
           case S_STORE:   RESULTIS "STORE"

           case S_ENTRY:   RESULTIS "ENTRY"
           case S_SAVE:    RESULTIS "SAVE"
           case S_FNAP:    RESULTIS "FNAP"
           case S_FNRN:    RESULTIS "FNRN"
           case S_RTAP:    RESULTIS "RTAP"
           case S_RTRN:    RESULTIS "RTRN"
           case S_ENDPROC: RESULTIS "ENDPROC"
           case S_RES:     RESULTIS "RES"
           case S_RSTACK:  RESULTIS "RSTACK"
           case S_FINISH:  RESULTIS "FINISH"

           case S_SWITCHON:RESULTIS "SWITCHON"
           case S_GLOBAL:  RESULTIS "GLOBAL"
           case S_DATALAB: RESULTIS "DATALAB"
           case S_ITEML:   RESULTIS "ITEML"
           case S_ITEMN:   RESULTIS "ITEMN"   }

        FOR I = 1 TO getbyte(S, 0) DO WRC(getbyte(S, I))   }1


AND WRN(N) BE { IF N<0 DO { WRC('-'); N := - N  }
                 WRPN(N)  }

AND WRPN(N) BE { IF N>9 DO WRPN(N/10)
                  WRC(N REM 10 + '0')  }

AND ENDOCODE() BE { putchar('*N'); OCOUNT := 0  }

WRC(ch)
{
    OCOUNT := OCOUNT + 1
    IF OCOUNT>62 /\ ch='*S' DO {
        putchar('*N');
        OCOUNT := 0;
        RETURN
    }
    putchar(ch)
}
