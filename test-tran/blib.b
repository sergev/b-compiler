//   BLIB

GET "LIBHDR"

unpackstring(s, v)
{
    FOR i = 0 TO getbyte(s, 0) DO
        v!i := getbyte(s, i)
}

packstring(v, s)
{
    auto n = v!0 & 255
    auto i = n/4
    FOR p = 0 TO n DO putbyte(s, p, v!p)
    SWITCHON n&3 INTO {
        CASE 0: putbyte(s, n+3, 0)
        CASE 1: putbyte(s, n+2, 0)
        CASE 2: putbyte(s, n+1, 0)
        CASE 3:
    }
    RESULTIS i
}

// The definitions that follow are machine independent

writes(s)
{
    FOR i = 1 TO getbyte(s, 0) DO
        wrch(getbyte(s, i))
}

writed(n, d)
{
    auto t = VEC 20
    auto i, k = 0, n
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

writen(n)
{
    writed(n, 0)
}

newline()
{
    wrch('*N')
}

newpage()
{
    wrch('*P')
}

readn()
{
    auto sum = 0
    auto neg = FALSE

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

writeoct(n, d)
{
    IF d>1 DO
        writeoct(n>>3, d-1)
    wrch((n/\7)+'0')
}

writehex(n, d)
{
    IF d>1 DO
        writehex(n>>4, d-1)
    wrch((n&15)!TABLE
        '0','1','2','3','4','5','6','7',
        '8','9','A','B','C','D','E','F')
}

writef(format, a, b, c, d, e, f, g, h, i, j, k)
{
    auto t = @a

    FOR p = 1 TO getbyte(format, 0) DO {
        auto k = getbyte(format, p)

        TEST k='%' THEN {
            auto f, q, n = 0, t!0, 0
            auto type = getbyte(format, p+1)
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
