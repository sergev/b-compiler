extern {
    start        : 1

    // Built-in functions of INTCODE interpreter
    selectinput  : 11
    selectoutput : 12
    rdch         : 13
    wrch         : 14
    unrdch       : 15
    input        : 16
    output       : 17

    stop         : 30
    level        : 31
    longjump     : 32

    rewind       : 35

    aptovec      : 40
    findoutput   : 41
    findinput    : 42

    endread      : 46
    endwrite     : 47

    getbyte      : 85
    putbyte      : 86
}

writes(s)
{
    FOR i = 1 TO getbyte(s, 0) DO
        wrch(getbyte(s, i))
}

start()
{
    writes("Hello, World!*N ")
}
