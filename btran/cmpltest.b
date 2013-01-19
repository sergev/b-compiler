//
// This is a universal code-generator test program
// written by M. Richards originally to test the
// CII 10070 code-generator.
//
GET "LIBHDR"

GLOBAL $(
    f : 100
    g : 101
    h : 102
    testno : 103;
    failcount : 104
    v : 105
    testcount : 106
    quiet : 107
    t : 108
$)

STATIC $(
    a = 10
    b = 11
    c = 12
    w = 0
$)

MANIFEST $(
    K0 = 0
    K1 = 1
    K2 = 2
$)

LET t(x, y) = VALOF
$(
    testno := testno + 1
    testcount := testcount + 1
    IF x=y & quiet RESULTIS y
    writef("%I3 %I5 ", testno, y)
    TEST x=y THEN
        writes("OK*N")
    ELSE $(
        writef("FAILED %X8(%N) %X8(%N)*N", x, x, y, y)
        failcount := failcount + 1
    $)
    RESULTIS y
$)

LET t1(a,b,c,D,E,f,g) = t(a+b+c+D+E+f, g)

LET start(parm) BE
$(1
    LET v1 = VEC 200
    AND v2 = VEC 200
    tester(0, 1, 2, v1, v2)
$)1

AND tester(x, y, z, v1, v2) BE
$(1
    writef("*NCGTESTER ENTERED *N*N")

    //
    // First initialize certain variables
    //
    f, g, h := 100, 101, 102
    testno, testcount, failcount := 0, 0, 0
    v, w := v1, v2

    FOR i = 0 TO 200 DO v!i, w!i := 1000+i, 10000+i

    quiet := FALSE
    //quiet := GETBYTE(parm,0)>0 & GETBYTE(parm,1)='Q' -> TRUE, FALSE

    //
    // Test simple variables and expressions
    //
    t(a+b+c, 33)
    t(f+g+h, 303)
    t(x+y+z, 3)

    t(123+321-400, 44)
    t(x=0, TRUE)
    t(y=0, FALSE)
    t(!(@y+x), 1)
    t(!(@b+x), 11)
    t(!(@g+x), 101)

    x, a, f := 5, 15, 105
    t(x, 5)
    t(a, 15)
    t(f, 105)

    v!1, v!2 := 1234, 5678
    t(v!1, 1234)
    t(v!z, 5678)

    t(x*a, 75)
    t(1*x+2*y+3*z+f*4,433)
    t(x*a+a*x, 150)

    t(100/2, 50)
    t(a/x, 3)
    t(a/-x, -3)
    t((-a)/x, -3)
    t((-a)/(-x), 3)
    t((a+a)/a, 2)
    t((a*x)/(x*a), 1)
    t((a+b)*(x+y)*123/(6*123), 26)

    t(7 REM 2, 1)
    t(f REM 100, 5)
    t(a REM x, 0)

    t(-f, -105)
    t(f=105, TRUE)
    t(f NE 105, FALSE)
    t(f<105, FALSE)
    t(f>=105, TRUE)
    t(f>105, FALSE)
    t(f<=105, TRUE)

    t(#1775<<3, #17750)
    t(#1775>>3, #177)
    t(#1775<<z+1, #17750)
    t(#1775>>z+1, #177)

    t(#B1100&#B1010, #B1000)
    t(#B1100 \/ #B1010, #B1110)
    t((#B1100 EQV   #B1010) & #B11111, #B11001)
    t(#B1100 NEQV  #B1010, #B0110)

    t(NOT TRUE, FALSE)
    t(NOT FALSE, TRUE)
    t(NOT(1234 EQV -4321), 1234 NEQV -4321)

    t(-f, -105)

    t(!v, 1000)
    t(v!0, 1000)
    t(v!1, 1234)
    t(v!(!v-998), 5678)
    t(!w, 10000)
    t(w!0, 10000)
    t(0!w, 10000)
    t(1!w, 10001)
    t(w!1, 10001)
    t(!(w+200), 10200)

    a := TRUE
    b := FALSE

    IF a DO x := 16
    t(x, 16)
    x := 16

    IF b DO x := 15
    t(x, 16)
    x := 15

    $(
        LET w = VEC 20
        GOTO L1
    L2: writes("GOTO ERROR*N")
        failcount := failcount+1
    $)

L1: a := VALOF RESULTIS 11
    t(a, 11)

    //
    // Test simulated stack routines
    //
    testno := 100

    $(
        LET v1 = VEC 1
        v1!0, v1!1 := -1, -2
        $(
            LET v2 = VEC 10
            FOR i = 0 TO 10 DO
                v2!i := -i
            t(v2!5, -5)
        $)
        t(v1!1, -2)
    $)

    x := x + t(x,15, t(f, 105), t(a, 11)) - 15
    t(x, 15)

    x := x+1
    t(x, 16)
    x := x-1
    t(x, 15)
    x := x+7
    t(x, 22)
    x := x-22
    t(x, 0)
    x := x+15
    t(x, 15)
    x := x + f
    t(x, 120)
    x := 1

    //
    // Test switchon commands
    //
    testno := 200

    $(SW
        LET s1, s1f = 0, 0
        AND s2, s2f = 0, 0

        FOR i = -200 TO 200 DO $(
            SWITCHON i INTO $(
            DEFAULT: s1 := s1+1000; ENDCASE
            CASE -1000: s1f := s1f + i; ENDCASE
            CASE -200: s1 := s1 + 1
            CASE -190: s1 := s1 + 1
            CASE -180: s1 := s1 + 1
            CASE   -5: s1 := s1 + 1
            CASE    0: s1 := s1 + 1
            CASE -145: s1 := s1 + 1
            CASE    7: s1 := s1 + 1
            CASE    8: s1 := s1 + 1
            CASE  200: s1 := s1 + 1
            CASE  190: s1 := s1 + 1
            CASE  100: s1 := s1 + 1
            CASE   90: s1 := s1 + 1
            CASE  199: s1 := s1 + 1
            CASE   95: s1 := s1 + 1
            CASE   76: s1 := s1 + 1
            CASE   88: s1 := s1 + 1
            CASE   99: s1 := s1 + 1
            CASE  -98: s1 := s1 + 1
            CASE   11: s1 := s1 + 1
            CASE   12: s1 := s1 + 1
            CASE   13: s1 := s1 + 1
            CASE   41: s1 := s1 + 1
            CASE   91: s1 := s1 + 1
            CASE   92: s1 := s1 + 1
            CASE   71: s1 := s1 + 1
            CASE   73: s1 := s1 + 1
            CASE   74: s1 := s1 + 1
            CASE   81: s1 := s1 + 1
            CASE   82: s1 := s1 + 1
            CASE   61: s1 := s1 + 1
            CASE -171: s1 := s1 + 1
            CASE -162: s1 := s1 + 1
            $)

            SWITCHON i+10000 INTO $(
            DEFAULT: s2 := s2+1000; ENDCASE
            CASE 10020: s2 := s2 + 1
            CASE 10021: s2 := s2 + 1
            CASE 10022: s2 := s2 + 1
            CASE 10023: s2 := s2 + 1
            CASE 10024: s2 := s2 + 1
            CASE 10025: s2 := s2 + 1
            CASE 10026: s2 := s2 + 1
            CASE 10027: s2 := s2 + 1
            CASE 10028: s2 := s2 + 1
            CASE 10029: s2 := s2 + 1
            CASE 10010: s2 := s2 + 1
            CASE 10011: s2 := s2 + 1
            CASE 10012: s2 := s2 + 1
            CASE 10013: s2 := s2 + 1
            CASE 10014: s2 := s2 + 1
            CASE 10015: s2 := s2 + 1
            $)
        $)
        t(s1f, 0)
        t(s2f, 0)
        t(s1, (401-32)*1000 + 32*(32+1)/2)
        t(s2, (401-16)*1000 + 16*(16+1)/2)
    $)SW

    //
    // Test function calling
    //
    testno := 250

    t1(1,2,3,4,5,6, 21)
    t1(t(1,1), t(2,2), t(3,3), t(4,4), t(5,5), t(6,6),
       t(21,21))
    t1(VALOF RESULTIS 1,
       VALOF RESULTIS 2,
       VALOF RESULTIS 3,
       VALOF RESULTIS 4,
       VALOF RESULTIS 5,
       VALOF RESULTIS 6,
       21)
    t1(VALOF RESULTIS 1,
       t(2,2),
       VALOF RESULTIS 3,
       t(4,4),
       VALOF RESULTIS 5,
       t(6,6),
       21)
    t1( 1, t(2,2), VALOF RESULTIS 3,
        4, t(5,5), VALOF RESULTIS 6,
        21)
    t1(!v,v!0,v!200,!w,w!0,w!200, 2*1000+1200+2*10000+10200)
    (t1+(x+x)/x-2)(1,1,1,1,1,1,6)
    (!@t1)(1,2,3,4,5,6,21)

    //
    // Test expression operators
    //
    testno := 300

    t((2+3)+f+6,116)
    t(f+2+3+6,116)
    t(6+3+2+f, 116)
    t(f-104, 1)
    t((x+2)=(x+2)->99,98, 99)
    t(f<f+1->21,22, 21)
    t(f>f+1->31,32, 32)
    t(f<=105->41,42, 41)
    t(f>=105->51,52, 51)

    //
    // Test register allocation etc.
    //
    testno := 400

    x := 0
    y := 1
    z := 2
    t(x, 0)
    t(y, 1)
    t(z, 2)
    f,g,h := 101,102,103
    a,b,c := 11,12,13
    t(x+1,1)
    t(f+1, 102)
    t(a+1, 12)
    t(!(@a*2/2+f-101),11)
    a := @f
    t(!a, 101)
    b := @g
    a := @b
    t(!!a, 102)
    w!0 := @w!1
    w!1 := @h
    t(z*y+(w!0)!0!0-2, 103)
    t(z*y+w!1!0-2, 103)
    t(t(123,123),t(123,123))

    writef("*N%N TESTS COMPLETED, %N FAILURE(S)*N",
            testcount, failcount)
$)1
