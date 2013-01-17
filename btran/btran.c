#include <stdio.h>

/*
 * Functions from standard B library.
 */
#define getbyte(s, i)   ((unsigned char*)s) [i]
#define newline()       putchar('\n')

/*
 * AE operators and symbols
 */
#define S_NUMBER	1
#define S_NAME          2
#define S_STRING	3
#define S_TRUE	        4
#define S_FALSE	        5
#define S_VALOF	        6
#define S_LV	        7
#define S_RV	        8
#define S_VECAP	        9
#define S_FNAP	        10
#define S_MULT	        11
#define S_DIV	        12
#define S_REM	        13
#define S_PLUS	        14
#define S_MINUS	        15
#define S_QUERY	        16
#define S_NEG	        17
#define S_EQ	        20
#define S_NE	        21
#define S_LS	        22
#define S_GR	        23
#define S_LE	        24
#define S_GE	        25
#define S_NOT	        30
#define S_LSHIFT	31
#define S_RSHIFT	32
#define S_LOGAND	33
#define S_LOGOR	        34
#define S_EQV	        35
#define S_NEQV	        36
#define S_COND	        37
#define S_COMMA	        38
#define S_TABLE	        39

#define S_AND	        40
#define S_VALDEF	41
#define S_VECDEF	42
#define S_CONSTDEF	43
#define S_FNDEF	        44
#define S_RTDEF	        45

#define S_ASS           50
#define S_RTAP	        51
#define S_GOTO	        52
#define S_RESULTIS	53
#define S_COLON	        54
#define S_TEST	        55
#define S_FOR	        56
#define S_IF	        57
#define S_UNLESS	58
#define S_WHILE	        59
#define S_UNTIL	        60
#define S_REPEAT	61
#define S_REPEATWHILE	62
#define S_REPEATUNTIL	63
#define S_LOOP	        65
#define S_BREAK	        66
#define S_RETURN	67
#define S_FINISH	68
#define S_ENDCASE	69
#define S_SWITCHON	70
#define S_CASE	        71
#define S_DEFAULT	72
#define S_SEQ	        73
#define S_LET	        74
#define S_MANIFEST	75
#define S_GLOBAL	76
#define S_LOCAL	        77
#define S_LABEL	        78
#define S_STATIC	79

/*
 * Other canonical symbols
 */
#define S_BE	        89
#define S_END	        90
#define S_LSECT	        91
#define S_RSECT	        92
#define S_GET	        93
#define S_SEMICOLON	97
#define S_INTO	        98
#define S_TO	        99
#define S_BY	        100
#define S_DO	        101
#define S_OR	        102
#define S_VEC	        103
#define S_LPAREN	105
#define S_RPAREN	106

/*
 * OCODE instructions
 */
#define S_LP	        40
#define S_LG	        41
#define S_LN	        42
#define S_LSTR	        43
#define S_LL	        44
#define S_LLP	        45
#define S_LLG	        46
#define S_LLL	        47

#define S_SP	        80
#define S_SG	        81
#define S_SL	        82
#define S_STIND	        83
#define S_JUMP	        85
#define S_JT	        86
#define S_JF	        87
#define S_LAB	        90
#define S_STACK	        91
#define S_STORE	        92
#define S_RSTACK	93
#define S_ENTRY	        94
#define S_SAVE	        95
#define S_FNRN	        96
#define S_RTRN	        97
#define S_RES	        98
#define S_RESLAB	99
#define S_DATALAB	100
#define S_ITEML	        101
#define S_ITEMN	        102
#define S_ENDPROC	103

/*
 * Selectors
 */
#define H1	0
#define H2	1
#define H3	2
#define H4	3
#define H5	4
#define H6	5

/*
 * Globals used in lex
 */
//int chbuf;
//int decval;
int *getv;
int getp;
//int wordv;
//int wordsize;
//int charv;
//int charp;
//int prsource;
int prline;
//int symb;
//int wordnode;
//int ch;
//int rdtag;
//int nextsymb;
//int declsyswords;
//int nlpending;
//int lookupword;
//int rch;
//int pptrace;
//int option;
int chcount;
//int linecount;
//int nulltag;
//int rec_p;
//int rec_l;

/*
 * Globals used in CAE
 */
//int rdblockbody;
//int rdsect;
//int rnamelist;
//int rname;
//int rexp;
//int rdef;
//int rcom;
//int rdcdefs;
//int nametable;
//int nametablesize;
//int formtree;
//int caereport;
//int checkfor;
//int ignore;
//int performget;
//int rexplist;
//int rdseq;
//int list1;
//int list2;
//int list3;
//int list4;
//int list5;
//int newvec;
//int treep;
//int treevec;
//int list6;
//int charcode;
//int reportcount;
//int reportmax;
//int sourcestream;
//int sysprint;
//int ocode;
//int sysin;

/*
 * Globals used in translator
 */
//int option;

//int charcode;
//int reportcount;
//int reportmax;
//int sysprint;
//int ocode;

//int trans;
//int declnames;
//int decldyn;
//int declstat;
//int checkdistinct;
//int addname;
//int transdef;
//int scanlabel;
//int decllabels;
//int jumpcond;
//int transswitch;
//int transfor;
//int assign;
//int load;
//int loadlv;
//int loadlist;
//int compdatalab;
//int evalconst;
//int loadzero;
//int complab;
//int compjump;
//int nextparam;
//int paramnumber;

int ocount;
int *dvec;
int dvecs;
int dvece;
int dvecp;
int dvect;
//int casek;
//int casel;
//int casep;
//int caset;
//int caseb;
int currentbranch;
//int breaklabel;
//int resultlabel;
//int defaultlabel;
//int endcaselabel;
//int looplabel;
//int ssp;
//int vecssp;
//int savespacesize;
//int globdecl;
//int globdecls;
//int globdeclt;
int comcount;

//int out1;
//int out2;
//int out2p;
//int out3;
//int out3p;
//int outn;
//int outl;
//int outc;
//int writeop;

/*
 * Other globals
 */
//int chbuf;
//int prsource;
//int pptrace;
//int option;
//int formtree;
//int treep;
//int treevec;
//int reportcount;
//int reportmax;
//int sourcestream;
//int sysprint;
//int ocode;
//int sysin;
//int savespacesize;

int cellwithname(n)
{
    int x = dvece;

    do {
        x = x - 3;
    } while (x != 0 && dvec[x] != n);
    return x;
}

#if 0
GET "LIBHDR"

comp(V, treemax)
{
    int B = VEC 63;
    int A;
    chbuf := B

    for (;;) {
        treep, treevec := V+treemax, V

        A = formtree()
        IF A=0 break

        printf("\nTREE SIZE %u\n", treemax+treevec-treep)

        IF option[2] DO {
            printf("AE TREE\n");
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

    printf("\nBCPL %u\n", @start)

    option := OPT
    savespacesize := 2
    pptrace := FALSE
    prsource := FALSE
    FOR i = 0 TO 20 DO
        OPT[i] := FALSE

    sourcestream := findinput("OPTIONS")

    UNLESS sourcestream=0 DO {
        LET ch = 0
        AND n = 0
        selectinput(sourcestream)
        printf("OPTIONS  ")

        for (;;) {
            ch := getchar()
L:          IF ch='\n' \/ ch=ENDSTREAMCH
                break
            putchar(ch)
            IF ch='P' DO n := 1
            IF ch='T' DO n := 2
            IF ch='C' DO n := 3
            IF ch='M' DO n := 4
            IF ch='N' DO n := 5
            IF ch='S' DO prsource := TRUE
            IF ch='E' DO pptrace := TRUE
            IF ch='L' DO {
                TREESIZE := readn()
                printf("%u", TREESIZE);
                ch := terminator
                goto L
            }
            IF ch='3' DO savespacesize := 3
            option[n] := TRUE
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

    //if (option[4]) mapstore();

    printf("\nPHASE 1 COMPLETE\n");
    if (reportcount != 0) {
        stop(8);
    }
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
         return

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
           rdtag(ch)
           SYMB := lookupword()
           IF SYMB=S_GET DO { performget(); goto next  }
           return

    case '$': rch()
              UNLESS ch='(' \/ ch=')' DO caereport(91)
              SYMB := ch='(' -> S_LSECT, S_RSECT
              rdtag('$')
              lookupword()
              return

    case '[':
    case '(': SYMB := S_LPAREN; goto L
    case ']':
    case ')': SYMB := S_RPAREN; goto L

    case '#': SYMB := S_NUMBER
              rch()
              IF '0'<=ch<='7' DO { readnumber(8); return  }
              IF ch='B' DO { rch(); readnumber(2); return  }
              IF ch='O' DO { rch(); readnumber(8); return  }
              IF ch='X' DO { rch(); readnumber(16); return  }
              caereport(33)

    case '?': SYMB := S_QUERY; goto L
    case '+': SYMB := S_PLUS; goto L
    case ',': SYMB := S_COMMA; goto L
    case ';': SYMB := S_SEMICOLON; goto L
    case '@': SYMB := S_LV; goto L
    case '&': SYMB := S_LOGAND; goto L
    case '=': SYMB := S_EQ; goto L
    case '!': SYMB := S_VECAP; goto L
    case '_': SYMB := S_ASS; goto L
    case '*': SYMB := S_MULT; goto L

    case '/': rch()
              IF ch='\\' DO { SYMB := S_LOGAND; goto L }
              IF ch='/' goto COMMENT
              UNLESS ch='*' DO { SYMB := S_DIV; return  }

              rch()

              UNTIL ch=ENDSTREAMCH DO TEST ch='*'

                    THEN { rch()
                            UNLESS ch='/' LOOP
                            rch()
                            goto next  }

                    OR { IF ch='\n' DO linecount := linecount+1
                          rch()  }

              caereport(63)


    COMMENT: rch() REPEATUNTIL ch='\n' \/ ch=ENDSTREAMCH
             goto next

    case '|': rch()
              IF ch='|' goto COMMENT
              SYMB := S_LOGOR
              return

    case '\\': rch()
              IF ch='/' DO { SYMB := S_LOGOR; goto L  }
              IF ch='=' DO { SYMB := S_NE; goto L  }
              SYMB := S_NOT
              return

    case '<': rch()
              IF ch='=' DO { SYMB := S_LE; goto L  }
              IF ch='<' DO { SYMB := S_LSHIFT; goto L }
              SYMB := S_LS
              return

    case '>': rch()
              IF ch='=' DO { SYMB := S_GE; goto L  }
              IF ch='>' DO { SYMB := S_RSHIFT; goto L  }
              SYMB := S_GR
              return

    case '-': rch()
              IF ch='>' DO { SYMB := S_COND; goto L  }
              SYMB := S_MINUS
              return

    case ':': rch()
              IF ch='=' DO { SYMB := S_ASS; goto L  }
              SYMB := S_COLON
              return

     case '\'': case '\"':
          {1 LET QUOTE = ch
              CHARP := 0

           { rch()
              IF ch=QUOTE \/ CHARP=255 DO
                     { UNLESS ch=QUOTE DO caereport(95)
                        IF CHARP=1 & ch='\'' DO
                                { SYMB := S_NUMBER
                                   goto L  }
                        CHARV[0] := CHARP
                        WORDSIZE := packstring(CHARV, WORDV)
                        SYMB := S_STRING
                        goto L   }


              IF ch='\n' DO linecount := linecount + 1

              IF ch='*' DO
                     { rch()
                        IF ch='\n' DO
                            { linecount := linecount+1
                               rch() REPEATWHILE ch=' ' \/ ch='\t'
                               UNLESS ch='*' DO caereport(34)
                               LOOP  }
                        IF ch='T' DO ch := '\t'
                        IF ch='S' DO ch := ' '
                        IF ch='N' DO ch := '\n'
                        IF ch='B' DO ch := '\b'
                        IF ch='P' DO ch := '\f'  }

              DECVAL, CHARP := ch, CHARP+1
              CHARV[CHARP] := ch  } REPEAT  }1



    default: IF ch=ENDSTREAMCH DO
    case '.':    { IF getp=0 DO
                          { SYMB := S_END
                             return   }

                    endread()
                    getp := getp - 3
                    sourcestream := getv[getp]
                    selectinput(sourcestream)
                    linecount := getv[getp+1]
                    ch := getv[getp+2]
                    goto next  }

                ch := '*S'
                caereport(94)
                rch()
                goto next

L:      rch()
    }
}1

AND readnumber(RADIX) BE
    { LET d = value(ch)
       DECVAL := d
       IF d>=RADIX DO caereport(33)

       { rch()
          d := value(ch)
          IF d>=RADIX return
          DECVAL := RADIX*DECVAL + d  } REPEAT
    }


int value(ch)
{
    return ('0' <= ch && ch <= '9') ? (ch - '0') :
           ('A' <= ch && ch <= 'F') ? (ch - 'A' + 10) :
           ('a' <= ch && ch <= 'f') ? (ch - 'a' + 10) : 100;
}

//    LEX2

GET "SYNHDR"

void d(S, ITEM)
{
    unpackstring(S, CHARV)
    WORDSIZE := packstring(CHARV, WORDV)
    lookupword()
    WORDNODE[0] := ITEM
}

void declsyswords()
{
    d("AND", S_AND)

    d("BE", S_BE)
    d("BREAK", S_BREAK)
    d("BY", S_BY)

    d("CASE", S_case)

    d("DO", S_DO)
    d("DEFAULT", S_DEFAULT)

    d("EQ", S_EQ)
    d("EQV", S_EQV)
    d("ELSE", S_OR)
    d("ENDcase", S_ENDcase)

    d("FALSE", S_FALSE)
    d("FOR", S_FOR)
    d("FINISH", S_FINISH)

    d("GOTO", S_GOTO)
    d("GE", S_GE)
    d("GR", S_GR)
    d("GLOBAL", S_GLOBAL)
    d("GET", S_GET)

    d("IF", S_IF)
    d("INTO", S_INTO)

    d("LET", S_LET)
    d("LV", S_LV)
    d("LE", S_LE)
    d("LS", S_LS)
    d("LOGOR", S_LOGOR)
    d("LOGAND", S_LOGAND)
    d("LOOP", S_LOOP)
    d("LSHIFT", S_LSHIFT)

    d("MANIFEST", S_MANIFEST)

    d("NE", S_NE)
    d("NOT", S_NOT)
    d("NEQV", S_NEQV)

    d("OR", S_OR)

    d("RESULTIS", S_RESULTIS)
    d("RETURN", S_RETURN)
    d("REM", S_REM)
    d("RSHIFT", S_RSHIFT)
    d("RV", S_RV)
    d("REPEAT", S_REPEAT)
    d("REPEATWHILE", S_REPEATWHILE)
    d("REPEATUNTIL", S_REPEATUNTIL)

    d("SWITCHON", S_SWITCHON)
    d("STATIC", S_STATIC)

    d("TO", S_TO)
    d("TEST", S_TEST)
    d("TRUE", S_TRUE)
    d("THEN", S_DO)
    d("TABLE", S_TABLE)

    d("UNTIL", S_UNTIL)
    d("UNLESS", S_UNLESS)

    d("VEC", S_VEC)
    d("VALOF", S_VALOF)

    d("WHILE", S_WHILE)

    d("$", 0);
    NULLTAG := WORDNODE;
}

AND lookupword() = VALOF

{1     LET HASHVAL = (WORDV[0]+WORDV[WORDSIZE] >> 1) REM NAMETABLESIZE
        LET M = @NAMETABLE[HASHVAL]

  next: WORDNODE := *M
        UNLESS WORDNODE=0 DO
             {2 FOR I = 0 TO WORDSIZE DO
                   IF WORDNODE[I+2] NE WORDV[I] DO
                   { M := WORDNODE+1
                      goto next  }
                 RESULTIS WORDNODE[0]  }2

        WORDNODE := newvec(WORDSIZE+2)
        WORDNODE[0], WORDNODE[1] := S_NAME, NAMETABLE[HASHVAL]
        FOR I = 0 TO WORDSIZE DO WORDNODE[I+2] := WORDV[I]
        NAMETABLE[HASHVAL] := WORDNODE
        RESULTIS S_NAME
}1

.

//    LEX3


GET "SYNHDR"

void rch()
{
    ch = getchar();

    if (prsource && getp == 0 && ch != ENDSTREAMCH) {
        if (linecount != prline) {
            printf("%4u  ", linecount);
            prline = linecount;
        }
        putchar(ch);
    }

    chcount = chcount + 1;
    chbuf[chcount & 63] = ch;
}

void wrchbuf()
{
    printf("\n...");
    FOR p = chcount-63 TO chcount DO {
        int k = chbuf[p & 63];
        if (k != 0)
            putchar(k);
    }
    newline();
}

AND rdtag(x) BE
    { CHARP, CHARV[1] := 1, x

        {  rch()
            UNLESS 'A'<=ch<='Z' \/
                   'a'<=ch<='z' \/
                   '0'<=ch<='9' \/
                    ch='.' break
            CHARP := CHARP+1
            CHARV[CHARP] := ch  } REPEAT

       CHARV[0] := CHARP
       WORDSIZE := packstring(CHARV, WORDV)  }


AND performget() BE
    { nextsymb()
       UNLESS SYMB=S_STRING THEN caereport(97)

       IF option[5] return

       getv[getp] := sourcestream
       getv[getp+1] := linecount
       getv[getp+2] := ch
       getp := getp + 3
       linecount := 1
       sourcestream := findinput(WORDV)
       IF sourcestream=0 THEN
           sourcestream := findlibinput(WORDV)
       IF sourcestream=0 THEN caereport(96,WORDV)
       selectinput(sourcestream)
       rch()   }

AND append(d, S) BE
    { LET ND = getbyte(d, 0)
       AND NS = getbyte(S, 0)
       FOR I = 1 TO NS DO {
           ND := ND + 1
           putbyte(d, ND, getbyte(S, I)) }
       putbyte(d, 0, ND) }

AND findlibinput(NAME) = VALOF
    { LET PATH = VEC 64
       AND DIR = "/usr/lib/bcpl/"
       TEST getbyte(DIR, 0) + getbyte(NAME, 0) > 255
       THEN RESULTIS 0
         OR { putbyte(PATH, 0, 0)
               append(PATH, DIR)
               append(PATH, NAME)
               RESULTIS findinput(PATH) }
    }


.

//    CAE0


GET "SYNHDR"

LET newvec(n) = VALOF
    { treep := treep - n - 1
       IF treep<=treevec DO
                { reportmax := 0
                   caereport(98)  }
        RESULTIS treep  }

AND list1(x) = VALOF
    { LET P = newvec(0)
       P[0] := x
       RESULTIS P  }

AND list2(x, y) = VALOF
     { LET P = newvec(1)
        P[0], P[1] := x, y
        RESULTIS P   }

AND list3(x, y, z) = VALOF
     { LET P = newvec(2)
        P[0], P[1], P[2] := x, y, z
        RESULTIS P     }

AND list4(x, y, z, T) = VALOF
     { LET P = newvec(3)
        P[0], P[1], P[2], P[3] := x, y, z, T
        RESULTIS P   }

AND list5(x, y, z, T, U) = VALOF
     { LET P = newvec(4)
        P[0], P[1], P[2], P[3], P[4] := x, y, z, T, U
        RESULTIS P   }

AND list6(x, y, z, T, U, V) = VALOF
     { LET P = newvec(5)
        P[0], P[1], P[2], P[3], P[4], P[5] := x, y, z, T, U, V
        RESULTIS P  }

AND caereport(n, A) BE
     { reportcount := reportcount + 1
        printf("\nSYNTAX ERROR NEAR LINE %u:  ", linecount)
        caemessage(n, A)
        wrchbuf()
        IF reportcount GR reportmax DO {
            printf("\nCOMPILATION ABORTED\n");
            stop(8);
        }
        nlpending := FALSE

        UNTIL SYMB=S_LSECT \/ SYMB=S_RSECT \/
              SYMB=S_LET \/ SYMB=S_AND \/
              SYMB=S_END \/ nlpending DO nextsymb()
        longjump(REC_P, REC_L)   }

AND formtree() =  VALOF
    {1 chcount := 0
        FOR I = 0 TO 63 DO chbuf[I] := 0

     { LET V = VEC 10   // FOR 'GET' STREAMS
        getv, getp := V, 0

     { LET V = VEC 100
        WORDV := V

     { LET V = VEC 256
        CHARV, CHARP := V, 0

     { LET V = VEC 100
        NAMETABLE, NAMETABLESIZE := V, 100
        FOR I = 0 TO 100 DO NAMETABLE[I] := 0

        REC_P, REC_L := level(), L

        linecount, prline := 1, 0
        rch()

        IF ch=ENDSTREAMCH RESULTIS 0
        declsyswords()

     L: nextsymb()

        IF option[1] DO   //   PP DEBUGGING OPTION
             { printf("%u %s\n", SYMB, WORDV)
                IF SYMB=S_END RESULTIS 0
                goto L  }

     { LET A = rdblockbody()
        UNLESS SYMB=S_END DO { caereport(99); goto L  }

        RESULTIS A        }1



AND caemessage(n, A) BE
    { LET S = VALOF

         SWITCHON n INTO

         { default:  printf("%u", n); return;

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

         printf(S, A)  }


.

//    CAE1


GET "SYNHDR"

LET rdblockbody() = VALOF
    {1 LET P, L = REC_P, REC_L
        LET A = 0

        REC_P, REC_L := level(), RECOVER

        ignore(S_SEMICOLON)

        SWITCHON SYMB INTO
     { case S_MANIFEST:
        case S_STATIC:
        case S_GLOBAL:
            {  LET OP = SYMB
                nextsymb()
                A := rdsect(rdcdefs)
                A := list3(OP, A, rdblockbody())
                goto RET  }


        case S_LET: nextsymb()
                    A := rdef()
           RECOVER: WHILE SYMB=S_AND DO
                          { nextsymb()
                             A := list3(S_AND, A, rdef())  }
                    A := list3(S_LET, A, rdblockbody())
                    goto RET

        default: A := rdseq()

                 UNLESS SYMB=S_RSECT \/ SYMB=S_END DO
                          caereport(51)

        case S_RSECT: case S_END:
        RET:   REC_P, REC_L := P, L
               RESULTIS A   }1

AND rdseq() = VALOF
    { LET A = 0
       ignore(S_SEMICOLON)
       A := rcom()
       IF SYMB=S_RSECT \/ SYMB=S_END RESULTIS A
       RESULTIS list3(S_SEQ, A, rdseq())   }


AND rdcdefs() = VALOF
    {1 LET A, B = 0, 0
        LET PTR = @A
        LET P, L = REC_P, REC_L
        REC_P, REC_L := level(), RECOVER

        { B := rname()
           TEST SYMB=S_EQ \/ SYMB=S_COLON THEN nextsymb()
                                            OR caereport(45)
           *PTR := list4(S_CONSTDEF, 0, B, rexp(0))
           PTR := @H2[*PTR]
  RECOVER: ignore(S_SEMICOLON) } REPEATWHILE SYMB=S_NAME

        REC_P, REC_L := P, L
        RESULTIS A  }1

AND rdsect(R) = VALOF
    {  LET TAG, A = WORDNODE, 0
        checkfor(S_LSECT, 6)
        A := R()
        UNLESS SYMB=S_RSECT DO caereport(7)
        TEST TAG=WORDNODE
             THEN nextsymb()
               OR IF WORDNODE=NULLTAG DO
                      { SYMB := 0
                         caereport(9)  }
        RESULTIS A   }


AND rnamelist() = VALOF
    {  LET A = rname()
        UNLESS SYMB=S_COMMA RESULTIS A
        nextsymb()
        RESULTIS list3(S_COMMA, A, rnamelist())   }


AND rname() = VALOF
    { LET A = WORDNODE
       checkfor(S_NAME, 8)
       RESULTIS A  }

AND ignore(ITEM) BE IF SYMB=ITEM DO nextsymb()

AND checkfor(ITEM, n) BE
      { UNLESS SYMB=ITEM DO caereport(n)
         nextsymb()  }

.

//    CAE2

GET "SYNHDR"

LET rbexp() = VALOF
  {1   LET A, OP = 0, SYMB

        SWITCHON SYMB INTO

    {  default:
            caereport(32)

        case S_QUERY:
            nextsymb(); RESULTIS list1(S_QUERY)

        case S_TRUE:
        case S_FALSE:
        case S_NAME:
            A := WORDNODE
            nextsymb()
            RESULTIS A

        case S_STRING:
            A := newvec(WORDSIZE+1)
            A[0] := S_STRING
            FOR I = 0 TO WORDSIZE DO A[I+1] := WORDV[I]
            nextsymb()
            RESULTIS A

        case S_NUMBER:
            A := list2(S_NUMBER, DECVAL)
            nextsymb()
            RESULTIS A

        case S_LPAREN:
            nextsymb()
            A := rexp(0)
            checkfor(S_RPAREN, 15)
            RESULTIS A

        case S_VALOF:
            nextsymb()
            RESULTIS list2(S_VALOF, rcom())

        case S_VECAP: OP := S_RV
        case S_LV:
        case S_RV: nextsymb(); RESULTIS list2(OP, rexp(35))

        case S_PLUS: nextsymb(); RESULTIS rexp(34)

        case S_MINUS: nextsymb()
                      A := rexp(34)
                      TEST H1[A]=S_NUMBER
                          THEN H2[A] := - H2[A]
                            OR A := list2(S_NEG, A)
                      RESULTIS A

        case S_NOT: nextsymb(); RESULTIS list2(S_NOT, rexp(24))

        case S_TABLE: nextsymb()
                      RESULTIS list2(S_TABLE, rexplist())   }1



AND rexp(n) = VALOF
    {1 LET A = rbexp()

        LET B, C, P, Q = 0, 0, 0, 0

  L: { LET OP = SYMB

        IF nlpending RESULTIS A

        SWITCHON OP INTO
    {B default: RESULTIS A

        case S_LPAREN: nextsymb()
                       B := 0
                       UNLESS SYMB=S_RPAREN DO B := rexplist()
                       checkfor(S_RPAREN, 19)
                       A := list3(S_FNAP, A, B)
                       goto L

        case S_VECAP: P := 40; goto LASSOC

        case S_REM: case S_MULT: case S_DIV: P := 35; goto LASSOC

        case S_PLUS: case S_MINUS: P := 34; goto LASSOC

        case S_EQ: case S_NE:
        case S_LE: case S_GE:
        case S_LS: case S_GR:
                IF n>=30 RESULTIS A

            {R nextsymb()
                B := rexp(30)
                A := list3(OP, A, B)
                TEST C=0 THEN C :=  A
                           OR C := list3(S_LOGAND, C, A)
                A, OP := B, SYMB  }R REPEATWHILE S_EQ<=OP<=S_GE

                A := C
                goto L

        case S_LSHIFT: case S_RSHIFT: P, Q := 25, 30; goto DIADIC

        case S_LOGAND: P := 23; goto LASSOC

        case S_LOGOR: P := 22; goto LASSOC

        case S_EQV: case S_NEQV: P := 21; goto LASSOC

        case S_COND:
                IF n>=13 RESULTIS A
                nextsymb()
                B := rexp(0)
                checkfor(S_COMMA, 30)
                A := list4(S_COND, A, B, rexp(0))
                goto L

        LASSOC: Q := P

        DIADIC: IF n>=P RESULTIS A
                nextsymb()
                A := list3(OP, A, rexp(Q))
                goto L                     }B     }1

LET rexplist() = VALOF
    {1 LET A = 0
        LET PTR = @A

     { LET B = rexp(0)
        UNLESS SYMB=S_COMMA DO { *PTR := B
                                  RESULTIS A  }
        nextsymb()
        *PTR := list3(S_COMMA, B, 0)
        PTR := @H3[*PTR]  } REPEAT
    }1

LET rdef() = VALOF
    {1 LET n = rnamelist()

        SWITCHON SYMB INTO

     { case S_LPAREN:
             { LET A = 0
                nextsymb()
                UNLESS H1[n]=S_NAME DO caereport(40)
                IF SYMB=S_NAME DO A := rnamelist()
                checkfor(S_RPAREN, 41)

                IF SYMB=S_BE DO
                     { nextsymb()
                        RESULTIS list5(S_RTDEF, n, A, rcom(), 0)  }

                IF SYMB=S_EQ DO
                     { nextsymb()
                        RESULTIS list5(S_FNDEF, n, A, rexp(0), 0)  }

                caereport(42)  }

        default: caereport(44)

        case S_EQ:
                nextsymb()
                IF SYMB=S_VEC DO
                     { nextsymb()
                        UNLESS H1[n]=S_NAME DO caereport(43)
                        RESULTIS list3(S_VECDEF, n, rexp(0))  }
                RESULTIS list3(S_VALDEF, n, rexplist())  }1

.


//    CAE4

GET "SYNHDR"

LET rbcom() = VALOF
   {1 LET A, B, OP = 0, 0, SYMB

        SWITCHON SYMB INTO
     { default: RESULTIS 0

        case S_NAME: case S_NUMBER: case S_STRING:
        case S_TRUE: case S_FALSE: case S_LV: case S_RV: case S_VECAP:
        case S_LPAREN:
                A := rexplist()

                IF SYMB=S_ASS  THEN
                    {  OP := SYMB
                        nextsymb()
                        RESULTIS list3(OP, A, rexplist())  }

                IF SYMB=S_COLON DO
                     { UNLESS H1[A]=S_NAME DO caereport(50)
                        nextsymb()
                        RESULTIS list4(S_COLON, A, rbcom(), 0)  }

                IF H1[A]=S_FNAP DO
                     { H1[A] := S_RTAP
                        RESULTIS A  }

                caereport(51)
                RESULTIS A

        case S_GOTO: case S_RESULTIS:
                nextsymb()
                RESULTIS list2(OP, rexp(0))

        case S_IF: case S_UNLESS:
        case S_WHILE: case S_UNTIL:
                nextsymb()
                A := rexp(0)
                ignore(S_DO)
                RESULTIS list3(OP, A, rcom())

        case S_TEST:
                nextsymb()
                A := rexp(0)
                ignore(S_DO)
                B := rcom()
                checkfor(S_OR, 54)
                RESULTIS list4(S_TEST, A, B, rcom())

        case S_FOR:
            {  LET I, J, K = 0, 0, 0
                nextsymb()
                A := rname()
                checkfor(S_EQ, 57)
                I := rexp(0)
                checkfor(S_TO, 58)
                J := rexp(0)
                IF SYMB=S_BY DO { nextsymb()
                                   K := rexp(0)  }
                ignore(S_DO)
                RESULTIS list6(S_FOR, A, I, J, K, rcom())  }

        case S_LOOP:
        case S_BREAK: case S_RETURN: case S_FINISH: case S_ENDcase:
                A := WORDNODE
                nextsymb()
                RESULTIS A

        case S_SWITCHON:
                nextsymb()
                A := rexp(0)
                checkfor(S_INTO, 60)
                RESULTIS list3(S_SWITCHON, A, rdsect(rdseq))

        case S_case:
                nextsymb()
                A := rexp(0)
                checkfor(S_COLON, 61)
                RESULTIS list3(S_case, A, rbcom())

        case S_DEFAULT:
                nextsymb()
                checkfor(S_COLON, 62)
                RESULTIS list2(S_DEFAULT, rbcom())

        case S_LSECT:
                RESULTIS rdsect(rdblockbody)   }1


AND rcom() = VALOF
    {1 LET A = rbcom()

        IF A=0 DO caereport(51)

        WHILE SYMB=S_REPEAT \/ SYMB=S_REPEATWHILE \/
                    SYMB=S_REPEATUNTIL DO
                  { LET OP = SYMB
                     nextsymb()
                     TEST OP=S_REPEAT
                         THEN A := list2(OP, A)
                           OR A := list3(OP, A, rexp(0))   }

        RESULTIS A  }1

.

//    PLIST


GET "SYNHDR"

void plist(x, n, d)
{
    int size = 0;
    static char *v[20];

    if (x == 0) {
        printf("NIL");
        return;
    }

    switch (H1[x]) {
    case S_NUMBER:
        printf("%d", H2[x]);
        return;

    case S_NAME:
        printf("%s", x+2);
        return

    case S_STRING:
        printf("\"%s\"", x+1);
        return

    case S_FOR:
        size = size + 2;

    case S_COND: case S_FNDEF: case S_RTDEF:
    case S_TEST: case S_CONSTDEF:
        size = size + 1;

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
        size = size + 1;

    case S_VALOF: case S_LV: case S_RV: case S_NEG: case S_NOT:
    case S_TABLE: case S_GOTO: case S_RESULTIS: case S_REPEAT:
    case S_DEFAULT:
        size = size + 1;

    case S_LOOP:
    case S_BREAK: case S_RETURN: case S_FINISH: case S_ENDcase:
    case S_TRUE: case S_FALSE: case S_QUERY:
    default:
        size = size + 1;
        if (n == d) {
            printf("ETC");
            return;
        }
        printf("OP");
        printf("%u", H1[x]);
        for (i = 2; i <= size; i++) {
            newline();
            for (j = 0; j < n; j++)
                printf(v[j]);
            printf("*-");
            v[n] = (i == size) ? "  " : "! ";
            plist(H1[x+i-1], n+1, d);
        }
        return;
    }
}

//    TRN0

GET "TRNHDR"

LET nextparam() = VALOF
    { PARAMNUMBER := PARAMNUMBER + 1
       RESULTIS PARAMNUMBER  }

void trnmessage(n)
{
    char *s;

    switch (n) {
    default:  printf("COMPILER ERROR  %u", n); return;

    case 141: s = "TOO MANY CASES"; break;
    case 104: s = "ILLEGAL USE OF BREAK, LOOP OR RESULTIS"; break;
    case 101:
    case 105: s = "ILLEGAL USE OF case OR DEFAULT"; break;
    case 106: s = "TWO caseS WITH SAME CONSTANT"; break;
    case 144: s = "TOO MANY GLOBALS"; break;
    case 142: s = "NAME DECLARED TWICE"; break;
    case 143: s = "TOO MANY NAMES DECLARED"; break;
    case 115: s = "NAME NOT DECLARED"; break;
    case 116: s = "DYNAMIC FREE VARIABLE USED"; break;
    case 117: case 118: case 119:
              s = "ERROR IN CONSTANT EXPRESSION"; break;
    case 110: case 112:
              s = "LHS AND RHS DO NOT MATCH"; break;
    case 109: case 113:
              s = "LTYPE EXPRESSION EXPECTED"; break;
    }
    printf(s);
}

LET compileae(x) BE
{1
    LET A = VEC 1200
       LET d = VEC 100
       LET K = VEC 150
       LET l = VEC 150

       dvec, dvecs, dvece, dvecp, dvect := A, 3, 3, 3, 1200
       dvec[0], dvec[1], dvec[2] := 0, 0, 0

       GLOBDECL, GLOBDECLS, GLOBDECLT := d, 0, 100

       caseK, caseL, caseP, caseT, caseB := K, l, 0, 150, -1
       endcaselabel, DEFAULTLABEL := 0, 0

       RESULTLABEL, breaklabel, LOOPLABEL := -1, -1, -1

       comcount = 0;
       currentbranch = x;

       ocount := 0

       PARAMNUMBER := 0
       SSP := savespacesize
       out2(S_STACK, SSP)
       decllabels(x)
       trans(x)
       out2(S_GLOBAL, GLOBDECLS/2)

    { LET I = 0
       UNTIL I=GLOBDECLS DO
          { outn(GLOBDECL[I])
             outl(GLOBDECL[I+1])
             I := I + 2  }

       endocode()
}1
.

//    TRN1

GET "TRNHDR"

LET trans(x) BE
  {TR
next:
 { LET SW = FALSE
    IF x=0 return
    currentbranch := x

    SWITCHON H1[x] INTO
{  default: transreport(100, x); return

    case S_LET:
      { LET A, B, S, S1 = dvece, dvecs, SSP, 0
         LET V = VECSSP
         declnames(H2[x])
         checkdistinct(B, dvecs)
         dvece := dvecs
         VECSSP, S1 := SSP, SSP
         SSP := S
         transdef(H2[x])
         UNLESS SSP=S1 DO transreport(110, x)
         UNLESS SSP=VECSSP DO { SSP := VECSSP
                                 out2(S_STACK, SSP)  }
         out1(S_STORE)
         decllabels(H3[x])
         trans(H3[x])
         VECSSP := V
         UNLESS SSP=S DO out2(S_STACK, S)
         dvece, dvecs, SSP := A, B, S
         return   }

    case S_STATIC:
    case S_GLOBAL:
    case S_MANIFEST:
     {1 LET A, B, S = dvece, dvecs, SSP
         AND OP = H1[x]
         AND y = H2[x]

         IF OP=S_MANIFEST DO OP := S_NUMBER

         UNTIL y=0 DO
           { TEST OP=S_STATIC THEN
                { LET M = nextparam()
                   addname(H3[y], S_LABEL, M)
                   compdatalab(M)
                   out2(S_ITEMN, evalconst(H4[y]))  }

                OR addname(H3[y], OP, evalconst(H4[y]))

              y := H2[y]
              dvece := dvecs  }

         decllabels(H3[x])
         trans(H3[x])
         dvece, dvecs, SSP := A, B, S
         return   }1


    case S_ASS:
       assign(H2[x], H3[x])
       return

    case S_RTAP:
     { LET S = SSP
        SSP := SSP+savespacesize
        out2(S_STACK, SSP)
        loadlist(H3[x])
        load(H2[x])
        out2(S_RTAP, S)
        SSP := S
        return  }

    case S_GOTO:
        load(H2[x])
        out1(S_GOTO)
        SSP := SSP-1
        return

    case S_COLON:
        complab(H4[x])
        trans(H3[x])
        return

    case S_UNLESS: SW := TRUE
    case S_IF:
     { LET l = nextparam()
        jumpcond(H2[x], SW, l)
        trans(H3[x])
        complab(l)
        return   }

    case S_TEST:
     { LET l, M = nextparam(), nextparam()
        jumpcond(H2[x], FALSE, l)
        trans(H3[x])
        compjump(M)
        complab(l)
        trans(H4[x])
        complab(M)
        return   }

    case S_LOOP:
        IF LOOPLABEL<0 DO transreport(104, x)
        IF LOOPLABEL=0 DO LOOPLABEL := nextparam()
        compjump(LOOPLABEL)
        return

    case S_BREAK:
        IF breaklabel<0 DO transreport(104, x)
        IF breaklabel=0 DO breaklabel := nextparam()
        compjump(breaklabel)
        return

    case S_RETURN: out1(S_RTRN)
                   return

    case S_FINISH: out1(S_FINISH)
                   return

    case S_RESULTIS:
        IF RESULTLABEL<0 DO transreport(104, x)
        load(H2[x])
        out2p(S_RES, RESULTLABEL)
        SSP := SSP - 1
        return

    case S_WHILE: SW := TRUE
    case S_UNTIL:
     { LET l, M = nextparam(), nextparam()
        LET BL, LL = breaklabel, LOOPLABEL
        breaklabel, LOOPLABEL := 0, M

        compjump(M)
        complab(l)
        trans(H3[x])
        complab(M)
        jumpcond(H2[x], SW, l)
        UNLESS breaklabel=0 DO complab(breaklabel)
        breaklabel, LOOPLABEL := BL, LL
        return   }

    case S_REPEATWHILE: SW := TRUE
    case S_REPEATUNTIL:
    case S_REPEAT:
     { LET l, BL, LL = nextparam(), breaklabel, LOOPLABEL
        breaklabel, LOOPLABEL := 0, 0
        complab(l)
        TEST H1[x]=S_REPEAT
            THEN { LOOPLABEL := l
                    trans(H2[x])
                    compjump(l)  }
              OR { trans(H2[x])
                    UNLESS LOOPLABEL=0 DO complab(LOOPLABEL)
                    jumpcond(H3[x], SW, l)  }
        UNLESS breaklabel=0 DO complab(breaklabel)
        breaklabel, LOOPLABEL := BL, LL
        return   }

    case S_case:
     { LET l, K = nextparam(), evalconst(H2[x])
        IF caseP>=caseT DO transreport(141, x)
        IF caseB<0 DO transreport(105, x)
        FOR I = caseB TO caseP-1 DO
                    IF caseK[I]=K DO transreport(106, x)
        caseK[caseP] := K
        caseL[caseP] := l
        caseP := caseP + 1
        complab(l)
        trans(H3[x])
        return   }

    case S_DEFAULT:
        IF caseB<0 DO transreport(105, x)
        UNLESS DEFAULTLABEL=0 DO transreport(101, x)
        DEFAULTLABEL := nextparam()
        complab(DEFAULTLABEL)
        trans(H2[x])
        return

    case S_ENDcase: IF caseB<0 DO transreport(105, x)
                    compjump(endcaselabel)
                    return

    case S_SWITCHON:
        transswitch(x)
        return

    case S_FOR: transfor(x)
                return

    case S_SEQ:
        trans(H2[x])
        comcount := comcount + 1;
        x := H3[x]
        goto next        }TR
.

//    TRN2

GET "TRNHDR"

LET declnames(x) BE UNLESS x=0 SWITCHON H1[x] INTO

     {  default: transreport(102, currentbranch)
                  return

         case S_VECDEF: case S_VALDEF:
               decldyn(H2[x])
               return

         case S_RTDEF: case S_FNDEF:
               H5[x] := nextparam()
               declstat(H2[x], H5[x])
               return

         case S_AND:
               declnames(H2[x])
               declnames(H3[x])
               return    }


AND decldyn(x) BE UNLESS x=0 DO

    { IF H1[x]=S_NAME DO
          { addname(x, S_LOCAL, SSP)
             SSP := SSP + 1
             return   }

       IF H1[x]=S_COMMA DO
          { addname(H2[x], S_LOCAL, SSP)
             SSP := SSP + 1
             decldyn(H3[x])
             return  }

       transreport(103, x)   }

AND declstat(x, l) BE
    {1 LET T = cellwithname(x)

       IF dvec[T+1]=S_GLOBAL DO
          { LET n = dvec[T+2]
             addname(x, S_GLOBAL, n)
             IF GLOBDECLS>=GLOBDECLT DO transreport(144, x)
             GLOBDECL[GLOBDECLS] := n
             GLOBDECL[GLOBDECLS+1] := l
             GLOBDECLS := GLOBDECLS + 2
             return  }


    { LET M = nextparam()
       addname(x, S_LABEL, M)
       compdatalab(M)
       out2p(S_ITEML, l)    }1


AND decllabels(x) BE
    { LET B = dvecs
       scanlabels(x)
       checkdistinct(B, dvecs)
       dvece := dvecs   }


AND checkdistinct(E, S) BE
       UNTIL E=S DO
          { LET P = E + 3
             AND n = dvec[E]
             WHILE P<S DO
                { IF dvec[P]=n DO transreport(142, n)
                   P := P + 3  }
             E := E + 3  }


AND addname(n, P, A) BE
    { IF dvecs>=dvect DO transreport(143, currentbranch)
       dvec[dvecs], dvec[dvecs+1], dvec[dvecs+2] := n, P, A
       dvecs := dvecs + 3  }


AND scanlabels(x) BE UNLESS x=0 SWITCHON H1[x] INTO

    { default: return

       case S_COLON:
            H4[x] := nextparam()
            declstat(H2[x], H4[x])

       case S_IF: case S_UNLESS: case S_WHILE: case S_UNTIL:
       case S_SWITCHON: case S_case:
            scanlabels(H3[x])
            return

       case S_SEQ:
            scanlabels(H3[x])

       case S_REPEAT:
       case S_REPEATWHILE: case S_REPEATUNTIL: case S_DEFAULT:
            scanlabels(H2[x])
            return

       case S_TEST:
            scanlabels(H3[x])
            scanlabels(H4[x])
            return    }


AND transdef(x) BE
    {1 transdyndefs(x)
        IF statdefs(x) DO
           { LET l, S= nextparam(), SSP
              compjump(l)
              transstatdefs(x)
              SSP := S
              out2(S_STACK, SSP)
              complab(l)  }1


AND transdyndefs(x) BE
        SWITCHON H1[x] INTO
     { case S_AND:
            transdyndefs(H2[x])
            transdyndefs(H3[x])
            return

        case S_VECDEF:
            out2(S_LLP, VECSSP)
            SSP := SSP + 1
            VECSSP := VECSSP + 1 + evalconst(H3[x])
            return

        case S_VALDEF: loadlist(H3[x])
                       return

        default: return  }

AND transstatdefs(x) BE
        SWITCHON H1[x] INTO
     { case S_AND:
             transstatdefs(H2[x])
             transstatdefs(H3[x])
             return

        case S_FNDEF: case S_RTDEF:
         {2 LET A, B, C = dvece, dvecs, dvecp
             AND BL, LL = breaklabel, LOOPLABEL
             AND RL, CB = RESULTLABEL, caseB
             breaklabel, LOOPLABEL := -1, -1
             RESULTLABEL, caseB := -1, -1

             compentry(H2[x], H5[x])
             SSP := savespacesize

             dvecp := dvecs
             decldyn(H3[x])
             checkdistinct(B, dvecs)
             dvece := dvecs
             decllabels(H4[x])

             out2(S_SAVE, SSP)

             TEST H1[x]=S_FNDEF
                THEN { load(H4[x]); out1(S_FNRN)  }
                  OR { trans(H4[x]); out1(S_RTRN)  }

             out2(S_ENDPROC, 0)

             breaklabel, LOOPLABEL := BL, LL
             RESULTLABEL, caseB := RL, CB
             dvece, dvecs, dvecp := A, B, C   }2

        default: return   }

AND statdefs(x) = H1[x]=S_FNDEF \/ H1[x]=S_RTDEF -> TRUE,
                  H1[x] NE S_AND -> FALSE,
                  statdefs(H2[x]) -> TRUE,
                  statdefs(H3[x])


.

//    TRN3


GET "TRNHDR"

LET jumpcond(x, B, l) BE
{JC LET SW = B
     SWITCHON H1[x] INTO
     { case S_FALSE: B := NOT B
        case S_TRUE: IF B DO compjump(l)
                     return

        case S_NOT: jumpcond(H2[x], NOT B, l)
                    return

        case S_LOGAND: SW := NOT SW
        case S_LOGOR:
         TEST SW THEN { jumpcond(H2[x], B, l)
                         jumpcond(H3[x], B, l)  }

                   OR { LET M = nextparam()
                         jumpcond(H2[x], NOT B, M)
                         jumpcond(H3[x], B, l)
                         complab(M)  }

         return

        default: load(x)
                 out2p(B -> S_JT, S_JF, l)
                 SSP := SSP - 1
                 return     }JC

AND transswitch(x) BE
    {1 LET P, B, DL = caseP, caseB, DEFAULTLABEL
        AND ECL = endcaselabel
        LET l = nextparam()
        endcaselabel := nextparam()
        caseB := caseP

        compjump(l)
        DEFAULTLABEL := 0
        trans(H3[x])
        compjump(endcaselabel)

        complab(l)
        load(H2[x])
        IF DEFAULTLABEL=0 DO DEFAULTLABEL := endcaselabel
        out3p(S_SWITCHON, caseP-P, DEFAULTLABEL)

        FOR I = caseB TO caseP-1 DO { outn(caseK[I])
                                       outl(caseL[I])  }

        SSP := SSP - 1
        complab(endcaselabel)
        endcaselabel := ECL
        caseP, caseB, DEFAULTLABEL := P, B, DL   }1

AND transfor(x) BE
     { LET A, B = dvece, dvecs
        LET l, M = nextparam(), nextparam()
        LET BL, LL = breaklabel, LOOPLABEL
        LET K, n = 0, 0
        LET STEP = 1
        LET S = SSP
        breaklabel, LOOPLABEL := 0, 0

        addname(H2[x], S_LOCAL, S)
        dvece := dvecs
        load(H3[x])

        TEST H1[H4[x]]=S_NUMBER
            THEN K, n := S_LN, H2[H4[x]]
              OR { K, n := S_LP, SSP
                    load(H4[x])  }

        UNLESS H5[x]=0 DO STEP := evalconst(H5[x])

        out1(S_STORE)
        compjump(l)
        decllabels(H6[x])
        complab(M)
        trans(H6[x])
        UNLESS LOOPLABEL=0 DO complab(LOOPLABEL)
        out2(S_LP, S); out2(S_LN, STEP); out1(S_PLUS); out2(S_SP, S)
        complab(l)
        out2(S_LP, S); out2(K, n); out1(STEP<0 -> S_GE, S_LE)
        out2p(S_JT, M)

        UNLESS breaklabel=0 DO complab(breaklabel)
        breaklabel, LOOPLABEL, SSP := BL, LL, S
        out2(S_STACK, SSP)
        dvece, dvecs := A, B  }

.

//    TRN4


GET "TRNHDR"

LET load(x) BE
    {1 IF x=0 DO { transreport(148, currentbranch)
                     loadzero()
                     return  }

     { LET OP = H1[x]

        SWITCHON OP INTO
     { default: transreport(147, currentbranch)
                 loadzero()
                 return

        case S_DIV: case S_REM: case S_MINUS:
        case S_LS: case S_GR: case S_LE: case S_GE:
        case S_LSHIFT: case S_RSHIFT:
            load(H2[x])
            load(H3[x])
            out1(OP)
            SSP := SSP - 1
            return

        case S_VECAP: case S_MULT: case S_PLUS: case S_EQ: case S_NE:
        case S_LOGAND: case S_LOGOR: case S_EQV: case S_NEQV:
         { LET A, B = H2[x], H3[x]
            IF H1[A]=S_NAME \/ H1[A]=S_NUMBER DO
                               A, B := H3[x], H2[x]
            load(A)
            load(B)
            IF OP=S_VECAP DO { out1(S_PLUS); OP := S_RV  }
            out1(OP)
            SSP := SSP - 1
            return   }

        case S_NEG: case S_NOT: case S_RV:
            load(H2[x])
            out1(OP)
            return

        case S_TRUE: case S_FALSE: case S_QUERY:
            out1(OP)
            SSP := SSP + 1
            return

        case S_LV: loadlv(H2[x])
                   return

        case S_NUMBER:
            out2(S_LN, H2[x])
            SSP := SSP + 1
            return

        case S_STRING:
         { LET S = @H2[x]
            out2(S_LSTR, getbyte(S, 0))
            FOR I = 1 TO getbyte(S, 0) DO outc(getbyte(S, I))
            wrc('*S')
            SSP := SSP + 1
            return   }

        case S_NAME:
             transname(x, S_LP, S_LG, S_LL, S_LN)
             SSP := SSP + 1
             return

        case S_VALOF:
         { LET RL = RESULTLABEL
            LET A, B = dvecs, dvece
            decllabels(H2[x])
            RESULTLABEL := nextparam()
            trans(H2[x])
            complab(RESULTLABEL)
            out2(S_RSTACK, SSP)
            SSP := SSP + 1
            dvecs, dvece := A, B
            RESULTLABEL := RL
            return   }


        case S_FNAP:
         { LET S = SSP
            SSP := SSP + savespacesize
            out2(S_STACK, SSP)
            loadlist(H3[x])
            load(H2[x])
            out2(S_FNAP, S)
            SSP := S + 1
            return   }

        case S_COND:
         { LET l, M = nextparam(), nextparam()
            LET S = SSP
            jumpcond(H2[x], FALSE, M)
            load(H3[x])
            compjump(l)
            SSP := S; out2(S_STACK, SSP)
            complab(M)
            load(H4[x])
            complab(l)
            return   }

        case S_TABLE:
         { LET M = nextparam()
            compdatalab(M)
            x := H2[x]
            WHILE H1[x]=S_COMMA DO
                  { out2(S_ITEMN, evalconst(H2[x]))
                     x := H3[x]   }
            out2(S_ITEMN, evalconst(x))
            out2p(S_LLL, M)
            SSP := SSP + 1
            return  }                         }1


AND loadlv(x) BE
    {1 IF x=0 goto ERR

        SWITCHON H1[x] INTO
     { default:
        ERR:     transreport(113, currentbranch)
                 loadzero()
                 return

        case S_NAME:
              transname(x, S_LLP, S_LLG, S_LLL, 0)
              SSP := SSP + 1
              return

        case S_RV:
            load(H2[x])
            return

        case S_VECAP:
         { LET A, B = H2[x], H3[x]
            IF H1[A]=S_NAME DO A, B := H3[x], H2[x]
            load(A)
            load(B)
            out1(S_PLUS)
            SSP := SSP - 1
            return   }  }1

AND loadzero() BE { out2(S_LN, 0)
                     SSP := SSP + 1  }

AND loadlist(x) BE UNLESS x=0 DO
    { UNLESS H1[x]=S_COMMA DO { load(x); return  }

       loadlist(H2[x])
       loadlist(H3[x])  }
.

//    TRN5

GET "TRNHDR"

LET evalconst(x) = VALOF
    {1 IF x=0 DO { transreport(117, currentbranch)
                     RESULTIS 0  }

        SWITCHON H1[x] INTO
     { default: transreport(118, x)
                 RESULTIS 0

        case S_NAME:
         { LET T = cellwithname(x)
            IF dvec[T+1]=S_NUMBER RESULTIS dvec[T+2]
            transreport(119, x)
            RESULTIS 0  }

        case S_NUMBER: RESULTIS H2[x]
        case S_TRUE: RESULTIS TRUE
        case S_FALSE: RESULTIS FALSE

        case S_NEG: RESULTIS - evalconst(H2[x])

        case S_MULT: RESULTIS evalconst(H2[x]) * evalconst(H3[x])
        case S_DIV:  RESULTIS evalconst(H2[x]) / evalconst(H3[x])
        case S_PLUS: RESULTIS evalconst(H2[x]) + evalconst(H3[x])
        case S_MINUS:RESULTIS evalconst(H2[x]) - evalconst(H3[x])
                    }1


AND assign(x, y) BE
    {1 IF x=0 \/ y=0 DO
            { transreport(110, currentbranch)
               return  }

        SWITCHON H1[x] INTO
     { case S_COMMA:
            UNLESS H1[y]=S_COMMA DO
                       { transreport(112, currentbranch)
                          return   }
            assign(H2[x], H2[y])
            assign(H3[x], H3[y])
            return

        case S_NAME:
            load(y)
            transname(x, S_SP, S_SG, S_SL, 0)
            SSP := SSP - 1
            return

        case S_RV: case S_VECAP: case S_COND:
            load(y)
            loadlv(x)
            out1(S_STIND)
            SSP := SSP - 2
            return

        default: transreport(109, currentbranch)
}1
#endif

void wrc(ch)
{
    ocount = ocount + 1;
    if (ocount > 62 && ch == ' ') {
        putchar('\n');
        ocount = 0;
        return;
    }
    putchar(ch);
}

void transreport(n, x)
{
    selectoutput(sysprint);
    reportcount = reportcount + 1;
    if (reportcount >= reportmax) {
        printf("\nCOMPILATION ABORTED\n");
        stop(8);
    }
    printf("\nREPORT:   ");
    trnmessage(n);
    printf("\nCOMMANDS COMPILED %u\n", comcount);
    plist(x, 0, 4);
    newline();
    selectoutput(ocode);
}

void writeop(x)
{
    char *s;
    int i, n;

    switch (x) {
    default: transreport(199, currentbranch); s = "ERROR"; break;

    case S_MULT:     s = "MULT";     break;
    case S_DIV:      s = "DIV";      break;
    case S_REM:      s = "REM";      break;
    case S_PLUS:     s = "PLUS";     break;
    case S_MINUS:    s = "MINUS";    break;
    case S_EQ:       s = "EQ";       break;
    case S_NE:       s = "NE";       break;
    case S_LS:       s = "LS";       break;
    case S_GR:       s = "GR";       break;
    case S_LE:       s = "LE";       break;
    case S_GE:       s = "GE";       break;
    case S_LSHIFT:   s = "LSHIFT";   break;
    case S_RSHIFT:   s = "RSHIFT";   break;
    case S_LOGAND:   s = "LOGAND";   break;
    case S_LOGOR:    s = "LOGOR";    break;
    case S_EQV:      s = "EQV";      break;
    case S_NEQV:     s = "NEQV";     break;

    case S_NEG:      s = "NEG";      break;
    case S_NOT:      s = "NOT";      break;
    case S_RV:       s = "RV";       break;

    case S_TRUE:     s = "TRUE";     break;
    case S_FALSE:    s = "FALSE";    break;
    case S_QUERY:    s = "QUERY";    break;

    case S_LP:       s = "LP";       break;
    case S_LG:       s = "LG";       break;
    case S_LN:       s = "LN";       break;
    case S_LSTR:     s = "LSTR";     break;
    case S_LL:       s = "LL";       break;

    case S_LLP:      s = "LLP";      break;
    case S_LLG:      s = "LLG";      break;
    case S_LLL:      s = "LLL";      break;

    case S_SP:       s = "SP";       break;
    case S_SG:       s = "SG";       break;
    case S_SL:       s = "SL";       break;
    case S_STIND:    s = "STIND";    break;

    case S_JUMP:     s = "JUMP";     break;
    case S_JT:       s = "JT";       break;
    case S_JF:       s = "JF";       break;
    case S_GOTO:     s = "GOTO";     break;
    case S_LAB:      s = "LAB";      break;
    case S_STACK:    s = "STACK";    break;
    case S_STORE:    s = "STORE";    break;

    case S_ENTRY:    s = "ENTRY";    break;
    case S_SAVE:     s = "SAVE";     break;
    case S_FNAP:     s = "FNAP";     break;
    case S_FNRN:     s = "FNRN";     break;
    case S_RTAP:     s = "RTAP";     break;
    case S_RTRN:     s = "RTRN";     break;
    case S_ENDPROC:  s = "ENDPROC";  break;
    case S_RES:      s = "RES";      break;
    case S_RSTACK:   s = "RSTACK";   break;
    case S_FINISH:   s = "FINISH";   break;

    case S_SWITCHON: s = "SWITCHON"; break;
    case S_GLOBAL:   s = "GLOBAL";   break;
    case S_DATALAB:  s = "DATALAB";  break;
    case S_ITEML:    s = "ITEML";    break;
    case S_ITEMN:    s = "ITEMN";    break;
    }

    n = getbyte(s, 0);
    for (i = 1; i <= n; i++)
        wrc(getbyte(s, i));
}

void wrpn(n)
{
    if (n > 9)
        wrpn(n / 10);
    wrc(n % 10 + '0');
}

void wrn(n)
{
    if (n < 0) {
        wrc('-');
        n = - n;
    }
    wrpn(n);
}

void endocode()
{
    putchar('\n');
    ocount = 0;
}

void out1(x)
{
    writeop(x);
    wrc(' ');
}

void out2(x, y)
{
    writeop(x);
    wrc(' ');
    wrn(y);
    wrc(' ');
}

void out2p(x, y)
{
    writeop(x);
    wrc(' ');
    wrc('L');
    wrn(y);
    wrc(' ');
}

void out3p(x, y, z)
{
    writeop(x);
    wrc(' ');
    wrn(y);
    wrc(' ');
    wrc('L');
    wrn(z);
    wrc(' ');
}

void outn(n)
{
    wrn(n);
}

void outl(x)
{
    wrc(' ');
    wrc('L');
    wrn(x);
    wrc(' ');
}

void outc(x)
{
    wrn(x);
    wrc(' ');
}

void complab(l)
{
    out2p(S_LAB, l);
}

void compentry(n, l)
    int *n;
{
    int *s = &n[2];
    int i, len;

    len = getbyte(s, 0);
    out3p(S_ENTRY, len, l);
    for (i = 1; i <= len; i++)
        outc(getbyte(s, i));
    wrc(' ');
}

void compdatalab(l)
{
    out2p(S_DATALAB, l);
}

void compjump(l)
{
    out2p(S_JUMP, l);
}

void transname(x, p, g, l, n)
{
    int t = cellwithname(x);
    int k = dvec[t+1];
    int a = dvec[t+2];

    if (t == 0) {
        transreport(115, x);
        out2(g, 2);
        return;
    }

    switch (k) {
    case S_LOCAL:
        if (t < dvecp)
            transreport(116, x);
        out2(p, a);
        return;

    case S_GLOBAL:
        out2(g, a);
        return;

    case S_LABEL:
        out2p(l, a);
        return;

    case S_NUMBER:
        if (n == 0) {
            transreport(113, x);
            n = p;
        }
        out2(n, a);
    }
}
