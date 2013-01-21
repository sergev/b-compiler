//    TRN0

GET "TRNHDR"

NEXTPARAM()
{
    PARAMNUMBER := PARAMNUMBER + 1
    RESULTIS PARAMNUMBER
}

TRNMESSAGE(N)
{
    auto S = 0

    SWITCHON N INTO {
        DEFAULT: writef("COMPILER ERROR  %N", N); RETURN

        CASE 141: S := "TOO MANY CASES"; ENDCASE
        CASE 104: S := "ILLEGAL USE OF BREAK, LOOP OR RESULTIS"; ENDCASE
        CASE 101:
        CASE 105: S := "ILLEGAL USE OF CASE OR DEFAULT"; ENDCASE
        CASE 106: S := "TWO CASES WITH SAME CONSTANT"; ENDCASE
        CASE 144: S := "TOO MANY GLOBALS"; ENDCASE
        CASE 142: S := "NAME DECLARED TWICE"; ENDCASE
        CASE 143: S := "TOO MANY NAMES DECLARED"; ENDCASE
        CASE 115: S := "NAME NOT DECLARED"; ENDCASE
        CASE 116: S := "DYNAMIC FREE VARIABLE USED"; ENDCASE
        CASE 117: CASE 118: CASE 119:
                  S := "ERROR IN CONSTANT EXPRESSION"; ENDCASE
        CASE 110: CASE 112:
                  S := "LHS AND RHS DO NOT MATCH"; ENDCASE
        CASE 109: CASE 113:
                  S := "LTYPE EXPRESSION EXPECTED"; ENDCASE
    }
    writes(S)
}

TRANSREPORT(N, X)
{
       selectoutput(SYSPRINT)
       REPORTCOUNT := REPORTCOUNT + 1
       IF REPORTCOUNT GE REPORTMAX DO
                { writes("*NCOMPILATION ABORTED*N")
                   stop(8)  }
       writes("*NREPORT:   "); TRNMESSAGE(N)
       writef("*NCOMMANDS COMPILED %N*N", COMCOUNT)
       PLIST(X, 0, 4); newline()
       selectoutput(OCODE)
}

COMPILEAE(X)
{
       auto A = VEC 1200
       auto D = VEC 100
       auto K = VEC 150
       auto L = VEC 150

       DVEC, DVECS, DVECE, DVECP, DVECT := A, 3, 3, 3, 1200
       DVEC!0, DVEC!1, DVEC!2 := 0, 0, 0

       GLOBDECL, GLOBDECLS, GLOBDECLT := D, 0, 100

       CASEK, CASEL, CASEP, CASET, CASEB := K, L, 0, 150, -1
       ENDCASELABEL, DEFAULTLABEL := 0, 0

       RESULTLABEL, BREAKLABEL, LOOPLABEL := -1, -1, -1

       COMCOUNT, CURRENTBRANCH := 0, X

       OCOUNT := 0

       PARAMNUMBER := 0
       SSP := SAVESPACESIZE
       OUT2(S.STACK, SSP)
       DECLLABELS(X)
       TRANS(X)
       OUT2(S.GLOBAL, GLOBDECLS/2)

    { auto I = 0
       UNTIL I=GLOBDECLS DO
          { OUTN(GLOBDECL!I)
             OUTL(GLOBDECL!(I+1))
             I := I + 2  }

       ENDOCODE()  }
}
.

//    TRN1

GET "TRNHDR"

TRANS(X)
{
NEXT:
 { auto SW = FALSE
    IF X=0 RETURN
    CURRENTBRANCH := X

    SWITCHON H1!X INTO
{  DEFAULT: TRANSREPORT(100, X); RETURN

    CASE S.LET:
       { auto A, B, S, S1 = DVECE, DVECS, SSP, 0
         auto V = VECSSP
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
     {   auto A, B, S = DVECE, DVECS, SSP
         auto OP = H1!X
         auto Y = H2!X

         IF OP=S.MANIFEST DO OP := S.NUMBER

         UNTIL Y=0 DO
            { TEST OP=S.STATIC THEN
                 { auto M = NEXTPARAM()
                   ADDNAME(H3!Y, S.LABEL, M)
                   COMPDATALAB(M)
                   OUT2(S.ITEMN, EVALCONST(H4!Y))  }

                OR ADDNAME(H3!Y, OP, EVALCONST(H4!Y))

              Y := H2!Y
              DVECE := DVECS  }

         DECLLABELS(H3!X)
         TRANS(H3!X)
         DVECE, DVECS, SSP := A, B, S
         RETURN   }


    CASE S.ASS:
       ASSIGN(H2!X, H3!X)
       RETURN

    CASE S.RTAP:
     { auto S = SSP
        SSP := SSP+SAVESPACESIZE
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
     { auto L = NEXTPARAM()
        JUMPCOND(H2!X, SW, L)
        TRANS(H3!X)
        COMPLAB(L)
        RETURN   }

    CASE S.TEST:
     { auto L, M = NEXTPARAM(), NEXTPARAM()
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
     { auto L, M = NEXTPARAM(), NEXTPARAM()
        auto BL, LL = BREAKLABEL, LOOPLABEL
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
     { auto L, BL, LL = NEXTPARAM(), BREAKLABEL, LOOPLABEL
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
     { auto L, K = NEXTPARAM(), EVALCONST(H2!X)
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
        GOTO NEXT        }
}}
.

//    TRN2


GET "TRNHDR"

DECLNAMES(X)
{
    UNLESS X=0 SWITCHON H1!X INTO {
         DEFAULT: TRANSREPORT(102, CURRENTBRANCH)
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
               RETURN
    }
}

DECLDYN(X)
{
    UNLESS X=0 DO
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
}

DECLSTAT(X, L)
{
    auto T = CELLWITHNAME(X)

       IF DVEC!(T+1)=S.GLOBAL DO
          { auto N = DVEC!(T+2)
             ADDNAME(X, S.GLOBAL, N)
             IF GLOBDECLS>=GLOBDECLT DO TRANSREPORT(144, X)
             GLOBDECL!GLOBDECLS := N
             GLOBDECL!(GLOBDECLS+1) := L
             GLOBDECLS := GLOBDECLS + 2
             RETURN  }


    { auto M = NEXTPARAM()
       ADDNAME(X, S.LABEL, M)
       COMPDATALAB(M)
       OUT2P(S.ITEML, L)    }
}

SCANLABELS(X)
{
    UNLESS X=0 SWITCHON H1!X INTO

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
}

DECLLABELS(X)
{
       auto B = DVECS
       SCANLABELS(X)
       CHECKDISTINCT(B, DVECS)
       DVECE := DVECS
}

CHECKDISTINCT(E, S)
{
       UNTIL E=S DO
          {  auto P = E + 3
             auto N = DVEC!E
             WHILE P<S DO
                { IF DVEC!P=N DO TRANSREPORT(142, N)
                   P := P + 3  }
             E := E + 3  }
}

ADDNAME(N, P, A)
{
    IF DVECS>=DVECT DO TRANSREPORT(143, CURRENTBRANCH)
    DVEC!DVECS, DVEC!(DVECS+1), DVEC!(DVECS+2) := N, P, A
    DVECS := DVECS + 3
}

CELLWITHNAME(N)
{
    auto X = DVECE

    X := X - 3 REPEATUNTIL X=0 \/ DVEC!X=N

    RESULTIS X
}

TRANSDYNDEFS(X)
{
    SWITCHON H1!X INTO {
        CASE S.AND:
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
}

TRANSSTATDEFS(X)
{
    SWITCHON H1!X INTO {
        CASE S.AND:
             TRANSSTATDEFS(H2!X)
             TRANSSTATDEFS(H3!X)
             RETURN

        CASE S.FNDEF: CASE S.RTDEF:
         {   auto A, B, C = DVECE, DVECS, DVECP
             auto BL, LL = BREAKLABEL, LOOPLABEL
             auto RL, CB = RESULTLABEL, CASEB
             BREAKLABEL, LOOPLABEL := -1, -1
             RESULTLABEL, CASEB := -1, -1

             COMPENTRY(H2!X, H5!X)
             SSP := SAVESPACESIZE

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
             DVECE, DVECS, DVECP := A, B, C   }

        DEFAULT: RETURN
    }
}

STATDEFS(X)
{
    RESULTIS H1!X=S.FNDEF \/ H1!X=S.RTDEF -> TRUE,
                          H1!X NE S.AND -> FALSE,
                         STATDEFS(H2!X) -> TRUE,
                                           STATDEFS(H3!X)
}

TRANSDEF(X)
{
        TRANSDYNDEFS(X)
        IF STATDEFS(X) DO
           { auto L, S= NEXTPARAM(), SSP
              COMPJUMP(L)
              TRANSSTATDEFS(X)
              SSP := S
              OUT2(S.STACK, SSP)
              COMPLAB(L)  }
}
.

//    TRN3

GET "TRNHDR"

JUMPCOND(X, B, L)
{    auto SW = B
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

                   OR { auto M = NEXTPARAM()
                         JUMPCOND(H2!X, NOT B, M)
                         JUMPCOND(H3!X, B, L)
                         COMPLAB(M)  }

         RETURN

        DEFAULT: LOAD(X)
                 OUT2P(B -> S.JT, S.JF, L)
                 SSP := SSP - 1
                 RETURN     }
}

TRANSSWITCH(X)
{
        auto P, B, DL = CASEP, CASEB, DEFAULTLABEL
        auto ECL = ENDCASELABEL
        auto L = NEXTPARAM()
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
        CASEP, CASEB, DEFAULTLABEL := P, B, DL   }

TRANSFOR(X)
{
        auto A, B = DVECE, DVECS
        auto L, M = NEXTPARAM(), NEXTPARAM()
        auto BL, LL = BREAKLABEL, LOOPLABEL
        auto K, N = 0, 0
        auto STEP = 1
        auto S = SSP
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

LOAD(X)
{
    IF X=0 DO { TRANSREPORT(148, CURRENTBRANCH)
                     LOADZERO()
                     RETURN  }

     { auto OP = H1!X

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
         { auto A, B = H2!X, H3!X
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
         { auto S = @H2!X
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
         { auto RL = RESULTLABEL
            auto A, B = DVECS, DVECE
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
         { auto S = SSP
            SSP := SSP + SAVESPACESIZE
            OUT2(S.STACK, SSP)
            LOADLIST(H3!X)
            LOAD(H2!X)
            OUT2(S.FNAP, S)
            SSP := S + 1
            RETURN   }

        CASE S.COND:
         { auto L, M = NEXTPARAM(), NEXTPARAM()
            auto S = SSP
            JUMPCOND(H2!X, FALSE, M)
            LOAD(H3!X)
            COMPJUMP(L)
            SSP := S; OUT2(S.STACK, SSP)
            COMPLAB(M)
            LOAD(H4!X)
            COMPLAB(L)
            RETURN   }

        CASE S.TABLE:
         { auto M = NEXTPARAM()
            COMPDATALAB(M)
            X := H2!X
            WHILE H1!X=S.COMMA DO
                  { OUT2(S.ITEMN, EVALCONST(H2!X))
                     X := H3!X   }
            OUT2(S.ITEMN, EVALCONST(X))
            OUT2P(S.LLL, M)
            SSP := SSP + 1
            RETURN  }                         }
    }
}

LOADLV(X)
{
    IF X=0 GOTO ERR

    SWITCHON H1!X INTO {
        DEFAULT:
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
         { auto A, B = H2!X, H3!X
            IF H1!A=S.NAME DO A, B := H3!X, H2!X
            LOAD(A)
            LOAD(B)
            OUT1(S.PLUS)
            SSP := SSP - 1
            RETURN   }  }
}

LOADZERO()
{
    OUT2(S.LN, 0)
    SSP := SSP + 1
}

LOADLIST(X)
{
    UNLESS X=0 DO
    { UNLESS H1!X=S.COMMA DO { LOAD(X); RETURN  }

       LOADLIST(H2!X)
       LOADLIST(H3!X)  }
}
.

//    TRN5

GET "TRNHDR"

EVALCONST(X)
{
    IF X=0 DO { TRANSREPORT(117, CURRENTBRANCH)
                     RESULTIS 0  }

    SWITCHON H1!X INTO
     { DEFAULT: TRANSREPORT(118, X)
                 RESULTIS 0

        CASE S.NAME:
         { auto T = CELLWITHNAME(X)
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
                    }
}

ASSIGN(X, Y)
{
    IF X=0 \/ Y=0 DO
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

        DEFAULT: TRANSREPORT(109, CURRENTBRANCH)   }
}

TRANSNAME(X, P, G, L, N)
{
    auto T = CELLWITHNAME(X)
    auto K, A = DVEC!(T+1), DVEC!(T+2)

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
                          OUT2(N, A)  }
}
.

//    TRN6

GET "TRNHDR"

COMPLAB(L)
{
    OUT2P(S.LAB, L)
}

COMPENTRY(N, L)
{
    auto S = @N!2
    OUT3P(S.ENTRY, getbyte(S, 0), L)
    FOR I = 1 TO getbyte(S, 0) DO OUTC(getbyte(S, I))
    WRC('*S')
}

COMPDATALAB(L)
{
    OUT2P(S.DATALAB, L)
}

COMPJUMP(L)
{
    OUT2P(S.JUMP, L)
}

OUT1(X)
{
    WRITEOP(X); WRC('*S')
}

OUT2(X, Y)
{
    WRITEOP(X); WRC('*S')
    WRN(Y); WRC('*S')
}

OUT2P(X, Y)
{
    WRITEOP(X); WRC('*S'); WRC('L')
    WRN(Y); WRC('*S')
}

OUT3P(X, Y, Z)
{
    WRITEOP(X); WRC('*S')
    WRN(Y); WRC('*S'); WRC('L')
    WRN(Z); WRC('*S')
}

OUTN(N)
{
    WRN(N)
}

OUTL(X)
{
    WRC('*S'); WRC('L'); WRN(X); WRC('*S')
}

OUTC(X)
{
    WRN(CHARCODE(X)); WRC('*S')
}

WRITEOP(X)
{
    auto S = 0

    SWITCHON X INTO {
        DEFAULT: TRANSREPORT(199, CURRENTBRANCH); S := 'ERROR'; ENDCASE

        CASE S.MULT:    S := "MULT"; ENDCASE
        CASE S.DIV:     S := "DIV"; ENDCASE
        CASE S.REM:     S := "REM"; ENDCASE
        CASE S.PLUS:    S := "PLUS"; ENDCASE
        CASE S.MINUS:   S := "MINUS"; ENDCASE
        CASE S.EQ:      S := "EQ"; ENDCASE
        CASE S.NE:      S := "NE"; ENDCASE
        CASE S.LS:      S := "LS"; ENDCASE
        CASE S.GR:      S := "GR"; ENDCASE
        CASE S.LE:      S := "LE"; ENDCASE
        CASE S.GE:      S := "GE"; ENDCASE
        CASE S.LSHIFT:  S := "LSHIFT"; ENDCASE
        CASE S.RSHIFT:  S := "RSHIFT"; ENDCASE
        CASE S.LOGAND:  S := "LOGAND"; ENDCASE
        CASE S.LOGOR:   S := "LOGOR"; ENDCASE
        CASE S.EQV:     S := "EQV"; ENDCASE
        CASE S.NEQV:    S := "NEQV"; ENDCASE

        CASE S.NEG:     S := "NEG"; ENDCASE
        CASE S.NOT:     S := "NOT"; ENDCASE
        CASE S.RV:      S := "RV"; ENDCASE

        CASE S.TRUE:    S := "TRUE"; ENDCASE
        CASE S.FALSE:   S := "FALSE"; ENDCASE
        CASE S.QUERY:   S := "QUERY"; ENDCASE

        CASE S.LP:      S := "LP"; ENDCASE
        CASE S.LG:      S := "LG"; ENDCASE
        CASE S.LN:      S := "LN"; ENDCASE
        CASE S.LSTR:    S := "LSTR"; ENDCASE
        CASE S.LL:      S := "LL"; ENDCASE

        CASE S.LLP:     S := "LLP"; ENDCASE
        CASE S.LLG:     S := "LLG"; ENDCASE
        CASE S.LLL:     S := "LLL"; ENDCASE

        CASE S.SP:      S := "SP"; ENDCASE
        CASE S.SG:      S := "SG"; ENDCASE
        CASE S.SL:      S := "SL"; ENDCASE
        CASE S.STIND:   S := "STIND"; ENDCASE

        CASE S.JUMP:    S := "JUMP"; ENDCASE
        CASE S.JT:      S := "JT"; ENDCASE
        CASE S.JF:      S := "JF"; ENDCASE
        CASE S.GOTO:    S := "GOTO"; ENDCASE
        CASE S.LAB:     S := "LAB"; ENDCASE
        CASE S.STACK:   S := "STACK"; ENDCASE
        CASE S.STORE:   S := "STORE"; ENDCASE

        CASE S.ENTRY:   S := "ENTRY"; ENDCASE
        CASE S.SAVE:    S := "SAVE"; ENDCASE
        CASE S.FNAP:    S := "FNAP"; ENDCASE
        CASE S.FNRN:    S := "FNRN"; ENDCASE
        CASE S.RTAP:    S := "RTAP"; ENDCASE
        CASE S.RTRN:    S := "RTRN"; ENDCASE
        CASE S.ENDPROC: S := "ENDPROC"; ENDCASE
        CASE S.RES:     S := "RES"; ENDCASE
        CASE S.RSTACK:  S := "RSTACK"; ENDCASE
        CASE S.FINISH:  S := "FINISH"; ENDCASE

        CASE S.SWITCHON:S := "SWITCHON"; ENDCASE
        CASE S.GLOBAL:  S := "GLOBAL"; ENDCASE
        CASE S.DATALAB: S := "DATALAB"; ENDCASE
        CASE S.ITEML:   S := "ITEML"; ENDCASE
        CASE S.ITEMN:   S := "ITEMN"; ENDCASE
    }

    FOR I = 1 TO getbyte(S, 0) DO
        WRC(getbyte(S, I))
}

WRN(N)
{
    IF N<0 DO { WRC('-'); N := - N  }
    WRPN(N)
}

WRPN(N)
{
    IF N>9 DO WRPN(N/10)
    WRC(N REM 10 + '0')
}

ENDOCODE()
{
    wrch('*N'); OCOUNT := 0
}

WRC(CH)
{
    OCOUNT := OCOUNT + 1
    IF OCOUNT>62 /\ CH='*S' DO {
        wrch('*N'); OCOUNT := 0; RETURN
    }
    wrch(CH)
}
