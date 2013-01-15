#include <stdio.h>
#include <string.h>

//
// OCODE operators
//
#define S_TRUE	        4
#define S_FALSE	        5
#define S_RV	        8
#define S_FNAP	        10
#define S_MULT	        11
#define S_DIV	        12
#define S_REM	        13
#define S_PLUS	        14
#define S_MINUS	        15
#define S_QUERY         16
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
#define S_LP	        40
#define S_LG	        41
#define S_LN	        42
#define S_LSTR	        43
#define S_LL	        44
#define S_LLP	        45
#define S_LLG	        46
#define S_LLL	        47

#define S_RTAP          51
#define S_GOTO          52
#define S_RETURN	67
#define S_FINISH	68
#define S_SWITCHON	70
#define S_GLOBAL	76

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
#define S_END	        104
#define S_CHAR	        105
#define ERROR           108
#define S_DEBUG         109

#define M_N	0
#define M_I	1
#define M_P	2
#define M_IP	3
#define M_L	4
#define M_IL	5
#define M_G	6
#define M_IG	7

#define F_L	'L'
#define F_S	'S'
#define F_A	'A'
#define F_J	'J'
#define F_T	'T'
#define F_F	'F'
#define F_K	'K'
#define F_X	'X'
#define F_D	'D'
#define F_C	'C'

#define TRUE	-1
#define FALSE	0
#define NIL	0
#define AD	1
#define AC	2
#define ACAD	3

int ch;
int *wordv;
int ssp;
int state;
int ad_a;
int ad_k;
int *datav;
int datap;
int datat;
int proglength;
int linep;
int param;
int op;

static int wp = 0;
static int strsize = 0;

void putbyte(s, i, c)
    int *s;
{
    int shift = (i & 3) << 3;
    s += i >> 2;
    int w = *s & ~(0xff << shift);
    *s = w | (c << shift);
}

int packstring(src, dest)
    int *src, *dest;
{
    int i, c;

    for (i=0; (c = src[i]); i++) {
        putbyte(dest, i, c);
    }
    do {
        putbyte(dest, i++, 0);
    } while (i & 3);
    return i >> 2;
}

int t(str)
    char *str;
{
#if 1
    return strcmp (str, (char*) wordv) == 0;
#else
    int i;
    int *s = (int*)str;

    for (i = 0; i < strsize; i++)
        if (s[i] != wordv[i])
            return FALSE;
    return TRUE;
#endif
}

int readop()
{
    int s [20];

    do {
        ch = getchar();
    } while (ch == '\n' || ch == ' ');
    wp = 0;

    while ('A' <= ch && ch <= 'Z') {
        s[wp] = ch;
        wp = wp + 1;
        ch = getchar();
    }

    s[wp] = 0;
    strsize = packstring(s, wordv);

    switch (s[0]) {
    default:
        if (ch == EOF)
            return S_END;
        return ERROR;

    case 'D':
        return t("DATALAB") ? S_DATALAB :
               t("DIV")     ? S_DIV :
               t("DEBUG")   ? S_DEBUG : ERROR;

    case 'E':
        return t("EQ")      ? S_EQ :
               t("ENTRY")   ? S_ENTRY :
               t("EQV")     ? S_EQV :
               t("ENDPROC") ? S_ENDPROC :
               t("END")     ? S_END : ERROR;

    case 'F':
        return t("FNAP")   ? S_FNAP :
               t("FNRN")   ? S_FNRN :
               t("FALSE")  ? S_FALSE :
               t("FINISH") ? S_FINISH : ERROR;

    case 'G':
        return t("GOTO")   ? S_GOTO :
               t("GE")     ? S_GE :
               t("GR")     ? S_GR :
               t("GLOBAL") ? S_GLOBAL : ERROR;

    case 'I':
        return t("ITEMN") ? S_ITEMN :
               t("ITEML") ? S_ITEML :  ERROR;

    case 'J':
        return t("JUMP") ? S_JUMP :
               t("JF")   ? S_JF :
               t("JT")   ? S_JT : ERROR;

    case 'L':
        if (wp == 2) {
            switch (s[1]) {
            default:  return ERROR;
            case 'E': return S_LE;
            case 'N': return S_LN;
            case 'G': return S_LG;
            case 'P': return S_LP;
            case 'L': return S_LL;
            case 'S': return S_LS;
            }
        }
        return t("LAB")    ? S_LAB :
               t("LLG")    ? S_LLG :
               t("LLL")    ? S_LLL :
               t("LLP")    ? S_LLP :
               t("LOGAND") ? S_LOGAND :
               t("LOGOR")  ? S_LOGOR :
               t("LSHIFT") ? S_LSHIFT :
               t("LSTR")   ? S_LSTR : ERROR;

    case 'M':
        return t("MINUS") ? S_MINUS :
               t("MULT")  ? S_MULT : ERROR;

    case 'N':
        return t("NE")   ? S_NE :
               t("NEG")  ? S_NEG :
               t("NEQV") ? S_NEQV :
               t("NOT")  ? S_NOT : ERROR;

    case 'P':
        return t("PLUS") ? S_PLUS : ERROR;

    case 'Q':
        return t("QUERY") ? S_QUERY : ERROR;

    case 'R':
        return t("RES")    ? S_RES :
               t("REM")    ? S_REM :
               t("RTAP")   ? S_RTAP :
               t("RTRN")   ? S_RTRN :
               t("RSHIFT") ? S_RSHIFT :
               t("RSTACK") ? S_RSTACK :
               t("RV")     ? S_RV : ERROR;

    case 'S':
        return t("SG")       ? S_SG :
               t("SP")       ? S_SP :
               t("SL")       ? S_SL :
               t("STIND")    ? S_STIND :
               t("STACK")    ? S_STACK :
               t("SAVE")     ? S_SAVE :
               t("SWITCHON") ? S_SWITCHON :
               t("STORE")    ? S_STORE : ERROR;

    case 'T':
        return t("TRUE") ? S_TRUE : ERROR;
    }
}

int rdn()
{
    int a = 0;
    int neg = FALSE;

    do {
        ch = getchar();
    } while (ch == '\n' || ch == ' ');

    if (ch == '-') {
        neg = TRUE;
        ch = getchar();
    }

    while ('0' <= ch && ch <= '9') {
        a = a*10 + ch - '0';
        ch = getchar();
    }

    return neg ? -a : a;
}

int rdl()
{
    int a = 0;

    do {
        ch = getchar();
    } while (ch == '\n' || ch == ' ');

    if (ch == 'L') {
        ch = getchar();
    }

    while ('0' <= ch && ch <= '9') {
        a = a*10 + ch - '0';
        ch = getchar();
    }

    return a;
}

void wr(ch)
{
    if (ch == '\n') {
        putchar('\n');
        linep = 0;
        return;
    }

    if (linep == 71) {
        putchar('/');
        putchar('\n');
        linep = 0;
    }
    linep = linep + 1;
    putchar(ch);
}

void wrn(n)
{
    if (n < 0) {
        wr('-');
        n = -n;
    }
    if (n > 9) {
        wrn(n/10);
    }
    wr(n % 10 + '0');
}

void code(f, a, k)
{
    wr(f);
    switch (k) {
    case M_I: wr('I');
    case M_N: break;

    case M_IG: wr('I');
    case M_G:  wr('G'); break;

    case M_IP: wr('I');
    case M_P:  wr('P'); break;

    case M_IL: wr('I');
    case M_L:  wr('L'); break;
    }

    wrn(a);
    wr(' ');
    proglength = proglength + 1;
}

void force_nil()
{
    switch (state) {
    case ACAD: code(F_S, ssp-2, M_P);
    case AD:   code(F_L, ad_a, ad_k);
    case AC:   code(F_S, ssp-1, M_P);
               state = NIL;
    case NIL:  ;
    }
}

void force_ad()
{
    switch (state) {
    case ACAD: code(F_S, ssp-2, M_P);
               goto L;
    case AC:   code(F_S, ssp-1, M_P);
    case NIL:  ad_a = ssp-1;
               ad_k = M_IP;
L:             state = AD;
    case AD:   ;
    }
}

void force_ac()
{
    switch (state) {
    case NIL:  code(F_L, ssp-1, M_IP);
               goto L;
    case ACAD: code(F_S, ssp-2, M_P);
    case AD:   code(F_L, ad_a, ad_k);
L:             state = AC;
    case AC:   ;
    }
}

void force_acad()
{
    switch (state) {
    case AD:   code(F_L, ssp-2, M_IP);
               goto L;
    case AC:   code(F_S, ssp-1, M_P);
    case NIL:  code(F_L, ssp-2, M_IP);
               ad_a = ssp-1;
               ad_k = M_IP;
L:             state = ACAD;
    case ACAD: ;
    }
}

void load(a, k)
{
    switch (state) {
    case NIL:   state = AD;
                goto M;
    case ACAD:
    case AD:    force_ac();
    case AC:    state = ACAD;
M:              ad_a = a;
                ad_k = k;
                ssp = ssp + 1;
    }
}

void storein(a, k)
{
    force_ac();
    code(F_S, a, k);
    ssp = ssp-1;
    state = NIL;
}

int nextparam()
{
    param = param - 1;
    return param;
}

void data(k, v)
{
    int p = datap;

    datav[p] = k;
    datav[p+1] = v;
    datap = datap + 2;
    if (datap > datat) {
        fprintf(stderr, "Too many constants!\n");
        datap = 0;
    }
}

void cgstring(n)
{
    int l = nextparam();
    int i;

    data(S_DATALAB, l);
    data(S_CHAR, n);
    for (i = 0; i < n; i++) {
        data(S_CHAR, rdn());
    }
    load(l, M_L);
}

void complab(n)
{
    wrn(n);
    wr(' ');
}

void wrdata(k, n)
{
    switch (k) {
    case S_DATALAB: complab(n);        return;
    case S_ITEMN:   code(F_D, n, M_N); return;
    case S_ITEML:   code(F_D, n, M_L); return;
    case S_CHAR:    code(F_C, n, M_N); return;
    }
}

int opcode(op)
{
    switch (op) {
    case S_RV:       return 1;
    case S_NEG:      return 2;
    case S_NOT:      return 3;
    case S_RTRN:     return 4;
    case S_MULT:     return 5;
    case S_DIV:      return 6;
    case S_REM:      return 7;
    case S_PLUS:     return 8;
    case S_MINUS:    return 9;
    case S_EQ:       return 10;
    case S_NE:       return 11;
    case S_LS:       return 12;
    case S_GE:       return 13;
    case S_GR:       return 14;
    case S_LE:       return 15;
    case S_LSHIFT:   return 16;
    case S_RSHIFT:   return 17;
    case S_LOGAND:   return 18;
    case S_LOGOR:    return 19;
    case S_NEQV:     return 20;
    case S_EQV:      return 21;
    case S_FINISH:   return 22;
    case S_SWITCHON: return 23;

    default:
        fprintf(stderr, "Unknown op %u\n", op);
        return 0;
    }
}

void gencode()
{
    int i;
next:
    op = readop();

    switch (op) {
    default:
        fprintf(stderr, "Unknown key word: '%s'\n", (char*) wordv);
        goto next;

    case S_END: return;

    case S_DEBUG:
         fprintf(stderr, "STATE=%u, SSP=%#x, AD.A=%u, AD.K=%u\n",
                   state,    ssp,    ad_a,    ad_k);
         goto next;

    case S_LP: load(rdn(), M_IP); goto next;
    case S_LG: load(rdn(), M_IG); goto next;
    case S_LL: load(rdl(), M_IL); goto next;
    case S_LN: load(rdn(), M_N);  goto next;

    case S_LSTR: cgstring(rdn()); goto next;

    case S_TRUE:  load(-1, M_N); goto next;
    case S_FALSE: load(0,  M_N); goto next;

    case S_LLP: load(rdn(), M_P); goto next;
    case S_LLG: load(rdn(), M_G); goto next;
    case S_LLL: load(rdl(), M_L); goto next;

    case S_SP: storein(rdn(), M_P); goto next;
    case S_SG: storein(rdn(), M_G); goto next;
    case S_SL: storein(rdl(), M_L); goto next;

    case S_STIND:
        force_acad();
        code(F_S, ad_a, ad_k);
        ssp   = ssp-2;
        state = NIL;
        goto next;

    case S_MULT:   case S_DIV:   case S_REM:
    case S_MINUS:  case S_EQ:    case S_NE:
    case S_LS:     case S_GR:    case S_LE:   case S_GE:
    case S_LSHIFT: case S_RSHIFT:
    case S_LOGAND: case S_LOGOR: case S_NEQV: case S_EQV:
        force_acad();
        code(F_L, ad_a, ad_k);
        code(F_X, opcode(op), M_N);
        state = AC;
        ssp   = ssp-1;
        goto next;

    case S_RV: case S_NEG: case S_NOT:
         force_ac();
         code(F_X, opcode(op), M_N);
         goto next;

    case S_PLUS:
        force_acad();
        code(F_A, ad_a, ad_k);
        state = AC;
        ssp   = ssp-1;
        goto next;

    case S_JUMP:
        force_nil();
        code(F_J, rdl(), M_L);
        goto next;

    case S_JT: case S_JF:
        force_ac();
        code((op == S_JT) ? F_T : F_F, rdl(), M_L);
        ssp   = ssp-1;
        state = NIL;
        goto next;

    case S_GOTO:
        force_ad();
        code(F_J, ad_a, ad_k);
        ssp   = ssp-1;
        state = NIL;
        goto next;

    case S_LAB:   force_nil(); complab(rdl()); goto next;
    case S_QUERY: force_nil(); ssp = ssp + 1;  goto next;
    case S_STACK: force_nil(); ssp = rdn();    goto next;
    case S_STORE: force_nil();                 goto next;

    case S_ENTRY: {
        int n = rdn();
        int l = rdl();

        wr('\n');
        wr('$');
        for (i = 0; i < n; i++)
            rdn();
        wr(' ');
        complab(l);
        goto next;
    }

    case S_SAVE:    ssp = rdn(); goto next;
    case S_ENDPROC: rdn();       goto next;

    case S_RTAP:
    case S_FNAP: {
        int k = rdn();

        force_ac();
        code(F_K, k, M_N);
        if (op == S_FNAP) {
            ssp   = k+1;
            state = AC;
        } else {
            ssp   = k;
            state = NIL;
        }
        goto next;
    }

    case S_FNRN:
        force_ac();
        ssp = ssp - 1;
    case S_RTRN:
        code(F_X, opcode(S_RTRN), M_N);
        state = NIL;
        goto next;

    case S_RES:
        force_ac();
        code(F_J, rdl(), M_L);
        ssp   = ssp-1;
        state = NIL;
        goto next;

    case S_RSTACK:
        force_nil();
        ssp   = rdn()+1;
        state = AC;
        goto next;

    case S_FINISH: code(F_X, opcode(op), M_N); goto next;

    case S_SWITCHON: {
        int n = rdn();
        int d = rdl();

        force_ac();
        code(F_X, opcode(op), M_N);
        code(F_D, n, M_N);
        code(F_D, d, M_L);
        ssp   = ssp-1;
        state = NIL;
        for (i = 0; i < n; i++) {
            code(F_D, rdn(), M_N);
            code(F_D, rdl(), M_L);
        }
        goto next;
    }

    case S_GLOBAL:
        wr('\n');
        for (i = 0; i < datap-2; i += 2) {
            wrdata(datav[i], datav[i+1]);
        }
        wr('\n');
        for (i = rdn(); i > 0; i--) {
            wr('G');
            wrn(rdn());
            wr('L');
            wrn(rdl());
            wr(' ');
        }
        wr('\n');
        wr('Z');
        wr('\n');
        return;

    case S_DATALAB:
    case S_ITEML: data(op, rdl()); goto next;
    case S_ITEMN: data(op, rdn()); goto next;
    }
}

int main()
{
    int v [4000];
    int w [50];

    datav = v;
    datat = 4000;
    wordv = w;

    if (freopen ("INTCODE", "w", stdout) != stdout) {
        perror ("INTCODE");
        return -1;
    }
    proglength = 0;

    do {
        ssp   = 2;
        state = NIL;
        datap = 0;
        linep = 0;
        param = 500;
        gencode();
    } while (op == S_GLOBAL);

    fprintf(stderr, "Program length = %u instructions\n", proglength);
    return 0;
}
