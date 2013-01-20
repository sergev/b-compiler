//   BLIB

GET "LIBHDR"

LET writes(s)
{
    FOR i = 1 TO getbyte(s, 0) DO
        wrch(getbyte(s, i))
}

LET unpackstring(s, v)
{
    FOR i = 0 TO getbyte(s, 0) DO
        v!i := getbyte(s, i)
}

LET packstring(v, s)
{
    LET n = v!0 & 255
    LET i = n/4
    FOR p = 0 TO n DO putbyte(s, p, v!p)
    SWITCHON n&3 INTO {
        CASE 0: putbyte(s, n+3, 0)
        CASE 1: putbyte(s, n+2, 0)
        CASE 2: putbyte(s, n+1, 0)
        CASE 3:
    }
    RESULTIS i
}

// THE DEFINITIONS THAT FOLLOW ARE MACHINE INDEPENDENT

LET writed(n, d)
{
    LET t = VEC 20
    LET i, k = 0, n
    IF n<0 DO
        d, k := d-1, -n
    t!i, k, i := k REM 10, k/10, i+1 REPEATUNTIL k=0
    FOR j = i+1 TO d DO
        wrch('*S')
    IF n<0 DO
        wrch('-')
    FOR j = i-1 TO 0 BY -1 DO
        wrch(t!j+'0')
}

LET writen(n)
{
    writed(n, 0)
}

LET newline()
{
    wrch('*N')
}

LET newpage()
{
    wrch('*P')
}

LET readn()
{
    LET sum = 0
    LET neg = FALSE

L:  terminator := rdch()
    SWITCHON terminator INTO {
        CASE '*S':
        CASE '*T':
        CASE '*N': GOTO L

        CASE '-':  neg := TRUE
        CASE '+':  terminator := rdch()
    }
    WHILE '0'<=terminator<='9' DO {
        sum := 10*sum + terminator - '0'
        terminator := rdch()
    }
    IF neg DO
        sum := -sum
    RESULTIS sum
}

LET writeoct(n, d)
{
    IF d>1 DO
        writeoct(n>>3, d-1)
    wrch((n/\7)+'0')
}

LET writehex(n, d)
{
    IF d>1 DO
        writehex(n>>4, d-1)
    wrch((n&15)!TABLE
        '0','1','2','3','4','5','6','7',
        '8','9','A','B','C','D','E','F')
}

LET writef(format, a, b, c, d, e, f, g, h, i, j, k)
{
    LET t = @a

    FOR p = 1 TO getbyte(format, 0) DO {
        LET k = getbyte(format, p)

        TEST k='%' THEN {
            LET f, q, n = 0, t!0, 0
            LET type = getbyte(format, p+1)
            p := p + 1
            SWITCHON type INTO {
                DEFAULT: wrch(type); ENDCASE

                CASE 'S': f := writes; GOTO L
                CASE 'C': f := wrch; GOTO L
                CASE 'O': f := writeoct; GOTO M
                CASE 'X': f := writehex; GOTO M
                CASE 'I': f := writed; GOTO M
                CASE 'N': f := writed; GOTO L

                M: p := p + 1
                   n := getbyte(format, p)
                   n := '0'<=n<='9' -> n-'0', n-'A'+10

                L: f(q, n); t := t + 1
            }
        }
        OR wrch(k)
    }
}

//LET mapstore() { writes("*Nmapstore NOT IMPLEMENTED*N"); }
