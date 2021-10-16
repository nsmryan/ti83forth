
.nolist
#include "ti83plus.inc"
#define ProgStart    $9D95
#define B_CALL(xxxx) rst 28h \ .dw xxxx 
#include forth.inc

.list
.org    ProgStart - 2
    .db    t2ByteTok, tAsmCmp
;Entry point
    ;backup stack pointer
    ;initialize stacks
    ld (OLD_SP), sp
    ld sp, PS_LOCATION
    ld ix, RS_LOCATION
    ld bc, 0 ;initialize TOS
    ld hl, 0 ;initialize W (working register)
    
    B_CALL(_ClrLCDFull)
    B_CALL(_ClrScrnFull)
    B_CALL(_runindicoff) 
    ;Display message
    ld hl, WELCOME_MSG
    B_CALL(_PutS)
    B_CALL(_NewLine)
    ;ld hl, VERSION_MSG
    ;B_CALL(_PutS)
    ;B_CALL(_NewLine)
    ;B_CALL(_GetKey)
    ;B_CALL(_ClrLCDFull)

    ;user memory can be on Floating Point Stack.
    ;FPS as here crashes real calc, runs on emulator. IDK why protected memory
    ;ld hl, DICT_ALLOC
    ;B_CALL(_AllocFPS)
    ;ld hl, DICT_ALLOC
    ;B_CALL(_DeallocFPS)
    ;ld hl,(FPS)
    ;ld (HERE_VAL), hl

LOAD_START_TEXT:
    ;this code allows some startup text. Can be empty if not needed.
    ld de, INBUF_LOCATION
    ld hl, START_TEXT
    ld bc, START_TEXT_END-START_TEXT
    ldir
    ld (BUFTOP), de

SETUP_BLOCK:
    ;user memory can be on BLOCK file. This file can be uploaded, but if it
    ;it not found it should be created to a default size. Also crashes real calc
    ;because ti cannot execute in file mem location
    ;ld hl, DICT_NAME
    ;rst 20h ;mov9toOP1
    ;push ix
    ;B_CALL(_ChkFindSym)
    ;pop ix
    ;jp nc, CONT_INIT
    ;ld hl, DICT_NAME
    ;rst 20h ;mov9toOP1
    ;ld hl, DICT_SIZE
    ;B_CALL(_CreateProg)
    
EDIT_MODE:
    ;ld de, END_PROG
    ;B_CALL(_EditProg)
CONT_INIT:
    ;inc de
    ;inc de
    ;inc de
    ;inc de
    ;ex de, hl
    ;ld (HERE_VAL), hl

    call _CR
    ld hl, MEM_MSG
    B_CALL(_PutS)
    B_CALL(_MemChk)
    B_CALL(_DispHL)
    B_CALL(_NewLine)
    B_CALL(_CursorOn)

    ;enter QUIT to start up Forth system
    ld de, COLD_START
    NEXT
    ;shouldn't ever reach here
    ret
;This can be used to load BLOCK file location to here, include high level
;word file, or any other startup stuff.
;remeber that the inbuf has up to 768 bytes before it overwrites stuff.
START_TEXT:
    .db " : DEFER   CREATE 0 , DOES> @ EXECUTE ; "
    .db " : IS      ` 3 + ! ; "
    .db " : STACK   CREATE 0 HERE @ ! 1+ CELLS ALLOT DOES> ; "
    .db " : PUSH    DUP @ 1+ 2DUP SWAP ! CELLS + ! ; "
    .db " : POP     DUP @ 0DUP UNLESS DROP EXIT THEN "
    .db "               2DUP CELLS + @ -ROT 1- SWAP ! ; "
    .db " : MAX     2DUP > IF DROP ELSE NIP THEN ;"
    .db " : MIN     2DUP < IF DROP ELSE NIP THEN ;"
    .db " : CASE:   CREATE ] DOES> OVER + + @ EXECUTE ;"
    .db " : [CHAR]  POSTPONE LIT CHAR , ; IMMEDIATE "
    .db " : (:      0 POSTPONE [ ; "
    .db " : VALUE   VARIABLE LATEST @ DFA >R 1+ ; "
    .db " : :)      0 DO R> ! LOOP POSTPONE ] ; "
    ;todo make this stuff work.
    .db " : DEPTH   S0 DSP@ - 2 / ;"
    ;.db " : .S      CR DSP@ DEPTH 0 DO I CELLS + @ . LOOP ;"
    .db kEnter
START_TEXT_END:
    .db " INCLUDE HLW ", kEnter;if rest of language is in HLW file

;Fragments

;Common DO definitions for words to jump to.
_DOCOL:
    dec ix
    ld (ix+0), d
    dec ix
    ld (ix+0), e
    inc hl
    inc hl
    inc hl
    SHORTNEXT
_DOCON:
    push bc
    inc hl
    inc hl
    inc hl
    ld c, (hl)
    inc hl
    ld b, (hl)
    NEXT
_DOVAR:
    inc hl
    inc hl
    inc hl
    push bc
    ld b, h
    ld c, l
    NEXT
;"jp" to DOES> action but do a "call" to _DODOES to put addr on stack.
_DODOES:
    dec ix
    ld (ix+0), d
    dec ix
    ld (ix+0), e
    ld d, b
    ld e, c
    inc hl
    inc hl
    inc hl
    ld b, h
    ld c, l
    pop hl
    push de
    SHORTNEXT

Dictionary:
;Begin Definitions
;First link is 0
;last link is LATEST_DEF

;NAME_DEF:
;    MAKEWORD(name, size, type, lastlink)
;NAME:
;    Assembly definition
;    NEXT

;or

;NAME_DEF:
;    MAKEWORD(name, size, type, lastlink)
;NAME:
;    jp _DOCOL
;    .dw any number of
;    .dw links to words
;    .dw EXIT

;DOCOL compiles addr of _DOCOL. First word in the dictionary.
DOCOL_DEF:
    MAKEWORD("DOCOL", 5, NORMAL, 0)
DOCOL:
    push bc
    ld hl, (HERE_VAL)
    ld bc, _DOCOL
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl
    ld (HERE_VAL), hl
    pop bc
    NEXT
;Common EXIT code fragment to jump to pops off last return value into IP.
EXIT_DEF:
    MAKEWORD("EXIT", 4, NORMAL, DOCOL_DEF)
EXIT:
    ld e, (ix+0)
    inc ix
    ld d, (ix+0)
    inc ix
    NEXT

DOCON_DEF:
    MAKEWORD("DOCON", 5, NORMAL, EXIT_DEF)
DOCON:
    push bc
    ld hl, (HERE_VAL)
    ld bc, _DOCON
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl
    ld (HERE_VAL), hl
    pop bc
    NEXT
DOVAR_DEF:
    MAKEWORD("DOVAR", 5, NORMAL, DOCON_DEF)
DOVAR:
    push bc
    ld hl, (HERE_VAL)
    ld bc, _DOVAR
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl
    ld (HERE_VAL), hl
    pop bc
    NEXT
;DUP simply push TOS, so that TOS is now in the stack and in bc register.
DUP_DEF:
    MAKEWORD("DUP", 3, NORMAL, DOVAR_DEF)
DUP:
    push bc
    NEXT
;0DUP simply push TOS, so that TOS is now in the stack and in bc register.
ZDUP_DEF:
    MAKEWORD("0DUP", 4, NORMAL, DUP_DEF)
ZDUP:
    ld a, c
    cp 0
    jp nz, ZDUP_DUP
    ld a, b
    cp 0
    jp nz, ZDUP_DUP
    jp ZDUP_END
ZDUP_DUP:
    push bc
ZDUP_END:
    NEXT

;DROP overwrites bc with actual TOS.
DROP_DEF:
    MAKEWORD("DROP", 4, NORMAL, ZDUP_DEF)
DROP:
    pop bc
    NEXT

;SWAP exchanges (sp) register and sp.
SWAP_DEF:
    MAKEWORD("SWAP", 4, NORMAL, DROP_DEF)
SWAP:
    ld h, b
    ld l, c
    ex (sp), hl
    ld b, h
    ld c, l
    NEXT
;OVER gets (sp) and pushes TOS register bc, then loads what was in (SP) into
;be, making it TOS
OVER_DEF:
    MAKEWORD("OVER", 4, NORMAL, SWAP_DEF)
OVER:
    pop hl
    push hl
    push bc
    ld c,l
    ld b,h
    NEXT
;NIP just moves the stack pointer past the current value.
NIP_DEF:
    MAKEWORD("NIP", 3, NORMAL, OVER_DEF)
NIP:
    inc sp
    inc sp
    NEXT
;TUCK ( n m -- m n m )
TUCK_DEF:
    MAKEWORD("TUCK", 4, NORMAL, NIP_DEF)
TUCK:
    pop hl
    push bc
    push hl
    NEXT
PICK_DEF:
    MAKEWORD("PICK", 4, NORMAL, TUCK_DEF)
PICK:
    jp _DOCOL
    .dw INCR, CELLS
    .dw DSPFETCH, ADD
    .dw FETCH
    .dw EXIT

;ROT ( a b c - b c a ) pops top two ideas of actual stack and pushes back two items, then
;loads into TOS register bc
ROT_DEF:
    MAKEWORD("ROT", 3, NORMAL, PICK_DEF)
ROT:
    pop af
    pop hl
    push af
    push bc
    ld b, h
    ld c, l
    NEXT
;ROT2 ( a b c -- c a b ) is the opposite of rot.
ROT2_DEF:
    MAKEWORD("-ROT", 4, NORMAL, ROT_DEF)
ROT2:
    pop hl
    pop af
    push bc
    push af
    ld b, h
    ld c, l
    ;push hl
    ;pop bc
    NEXT
;2DROP ( n m -- ) moves past the second value and overwrites TOS register.
TWODROP_DEF:
    MAKEWORD("2DROP", 5, NORMAL, ROT2_DEF)
TWODROP:
    inc sp
    inc sp
    pop bc
    NEXT
;2DUP duplicates first two items on stack
;( n m -- n m n m )
TWODUP_DEF:
    MAKEWORD("2DUP", 4, NORMAL, TWODROP_DEF)
TWODUP:
    pop hl
    push hl
    push bc
    push hl
    NEXT
;2SWAP ( a b c d -- c d a b )
TWOSWAP_DEF:
    MAKEWORD("2SWAP", 5, NORMAL, TWODUP_DEF)
TWOSWAP:
    pop hl
    pop af
    ex (sp),hl
    push bc
    push hl
    push af
    pop bc
    NEXT
;?DUP compare with bc and if not 0, push to stack
;leaving current value in bc.
QDUP_DEF:
    MAKEWORD("?DUP", 4, NORMAL, TWOSWAP_DEF)
QDUP:
    ld a, b
    or c
    jp z, QDUP_DONE
    push bc
QDUP_DONE:
    NEXT
;+1 increments TOS by 1
INCR_DEF:
    MAKEWORD("1+", 2, NORMAL, QDUP_DEF)
INCR:
    inc bc
    NEXT
;-1 decrements TOS by 1
DECR_DEF:
    MAKEWORD("1-", 2, NORMAL, INCR_DEF)
DECR:
    dec bc
    NEXT

;= checks equality with actual TOS and bc loads 1 if same, 0 if not use kEE
;for now for "=" sign.
EQUA_DEF:
    MAKEWORD('=', 1, NORMAL, DECR_DEF)
EQUA:
    pop hl
    dec bc;for some reason this makes sbc work
    sbc hl, bc
    dec hl
    jp z, EQUAL
    ld bc, 0
    NEXT
EQUAL:
    ld bc, 1
    NEXT
;0= ( n -- f ) f flag n == 0. 
ZEQUA_DEF:
    MAKEWORD("0=", 2, NORMAL, EQUA_DEF)
ZEQUA:
    ld a, b
    or c
    jp nz, ZEQUA_NOT
    ld bc, 1
    NEXT
ZEQUA_NOT:
    ld bc, 0
    NEXT

TRUE_DEF:
    MAKEWORD("TRUE", 4, NORMAL, ZEQUA_DEF)
TRUE:
    push bc
    ld bc, 1
    NEXT
FALSE_DEF:
    MAKEWORD("FALSE", 5, NORMAL, TRUE_DEF)
FALSE:
    push bc
    ld bc, 0
    NEXT
;COMPILE compiles the next two bytes as an xt. Useful for defining words in asm
;and maybe some defining words.
COMPILE_DEF:
    MAKEWORD("COMPILE", 7, NORMAL, FALSE_DEF)
COMPILE:
    push bc
    ex de, hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    ex de, hl
    ld b, h
    ld c, l
    ld hl, (HERE_VAL)
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl
    ld (HERE_VAL), hl
    pop bc
    NEXT
    
;POSTPONE compiles the next word in the input stream, without regard to
;immediate words.
POSTPONE_DEF:
    MAKEWORD("POSTPONE", 8, IMD_VAL, COMPILE_DEF)
POSTPONE:
    jp _DOCOL
    ;.dw COMPILE
    .dw TICK
    .dw COMMA
    .dw EXIT
MOD_DEF:
    MAKEWORD("MOD", 3, NORMAL, POSTPONE_DEF)
MOD:
    jp _DOCOL
    .dw DIVMOD,SWAP, DROP
    .dw EXIT
DIV_DEF:
    MAKEWORD('/', 1, NORMAL, MOD_DEF)
DIV:
    jp _DOCOL
    .dw DIVMOD, DROP
    .dw EXIT
BL_DEF:
    MAKEWORD("BL", 2, NORMAL, DIV_DEF)
BL:
    jp _DOCOL
    .dw LIT, 32
    .dw EXIT
SPACE_DEF:
    MAKEWORD("SPACE", 5, NORMAL, BL_DEF)
SPACE:
    jp _DOCOL
    .dw BL, EMIT
    .dw EXIT
CELLS_DEF:
    MAKEWORD("CELLS", 5, NORMAL, SPACE_DEF)
CELLS:
    jp _DOCOL
    .dw TWOTIMES
    .dw EXIT
CELLPLUS_DEF:
    MAKEWORD("CELL+", 5, NORMAL, CELLS_DEF)
CELLPLUS:
    inc bc
    inc bc
    NEXT
NEGATE_DEF:
    MAKEWORD("NEGATE", 6, NORMAL, CELLPLUS_DEF)
NEGATE:
    jp _DOCOL
    .dw LIT, 0, SWAP, SUB
    .dw EXIT
LITERAL_DEF:
    MAKEWORD("LITERAL", 7, IMD_VAL, NEGATE_DEF)
LITERAL:
    jp _DOCOL
    ;.dw TICK, LIT, COMMA, COMMA
    .dw COMPILE, LIT, COMMA
    .dw EXIT
IF_DEF:
    MAKEWORD("IF", 2, IMD_VAL, LITERAL_DEF)
IF:
    jp _DOCOL
    .dw COMPILE, ZBRANCH
    .dw HERE, FETCH
    .dw ZERO, COMMA
    .dw EXIT
THEN_DEF:
    MAKEWORD("THEN", 4, IMD_VAL, IF_DEF)
THEN:
    jp _DOCOL
    .dw DUP, HERE, FETCH, SWAP, SUB
    .dw SWAP, STORE
    .dw EXIT
ELSE_DEF:
    MAKEWORD("ELSE", 4, IMD_VAL, THEN_DEF)
ELSE:
    jp _DOCOL
    .dw COMPILE, BRANCH
    .dw HERE, FETCH, ZERO, COMMA
    .dw SWAP, DUP, HERE, FETCH, SWAP, SUB
    .dw SWAP, STORE
    .dw EXIT
BEGIN_DEF:
    MAKEWORD("BEGIN", 5, IMD_VAL, ELSE_DEF)
BEGIN:
    jp _DOCOL
    .dw HERE, FETCH
    .dw EXIT
UNTIL_DEF:
    MAKEWORD("UNTIL", 5, IMD_VAL, BEGIN_DEF)
UNTIL:
    jp _DOCOL
    .dw COMPILE, ZBRANCH
    .dw HERE, FETCH, SUB, COMMA
    .dw EXIT
AGAIN_DEF:
    MAKEWORD("AGAIN", 5, IMD_VAL, UNTIL_DEF)
AGAIN:
    jp _DOCOL
    .dw COMPILE, BRANCH
    .dw HERE, FETCH, SUB, COMMA
    .dw EXIT
WHILE_DEF:
    MAKEWORD("WHILE", 5, IMD_VAL, AGAIN_DEF)
WHILE:
    jp _DOCOL
    .dw COMPILE, ZBRANCH
    .dw HERE, FETCH, ZERO, COMMA
    .dw EXIT
REPEAT_DEF:
    MAKEWORD("REPEAT", 6, IMD_VAL, WHILE_DEF)
REPEAT:
    jp _DOCOL
    .dw COMPILE, BRANCH
    .dw SWAP, HERE, FETCH, SUB, COMMA
    .dw DUP, HERE, FETCH, SWAP, SUB, COMMA
    .dw SWAP, STORE
    .dw EXIT
UNLESS_DEF:
    MAKEWORD("UNLESS", 6, IMD_VAL, REPEAT_DEF)
UNLESS:
    jp _DOCOL
    .dw COMPILE, NOT
    .dw IF
    .dw EXIT
;
;: DO    COMPILE SWAP COMPILE 2>R
;        HERE @ COMPILE 2R@
;        COMPILE = COMPILE IF HERE @ 0 COMMA ;
DO_DEF:
    MAKEWORD("DO", 2, IMD_VAL, UNLESS_DEF)
DO:
    jp _DOCOL
    .dw COMPILE, SWAP, COMPILE, TWOTORS
    .dw HERE, FETCH, COMPILE, TWORSCOPY
    .dw COMPILE, LST, IF
    ;.dw HERE, FETCH, LIT, 0, COMMA
    .dw EXIT

;: LOOP  COMPILE R> COMPILE 1+ COMPILE >R
;        COMPILE BRANCH SWAP HERE @ - COMMA
;        DUP HERE @ SWAP - SWAP ! 
;        COMPILE 2RDROP ;
LOOP_DEF:
    MAKEWORD("LOOP", 4, IMD_VAL, DO_DEF)
LOOP:
    jp _DOCOL
    .dw COMPILE, FROMRS, COMPILE, INCR, COMPILE, TORS
    .dw COMPILE, BRANCH, SWAP, HERE, FETCH, SUB, COMMA
    .dw THEN
    ;.dw DUP, HERE, FETCH, SWAP, SUB, SWAP, STORE
    .dw COMPILE, TWORDROP
    .dw EXIT
I_DEF:
    MAKEWORD('I', 1, NORMAL, LOOP_DEF)
I:
    jp _DOCOL
    .dw TWORSCOPY, NIP
    .dw EXIT
;doesn't work, must know about return stack
J_DEF:
    MAKEWORD('J', 1, NORMAL, I_DEF)
J:
    jp _DOCOL
    ;.dw TWOFROMRS, FROMRS, RCOPY, ROT2, TWOTORS
    .dw EXIT
UNLOOP_DEF:
    MAKEWORD("UNLOOP", 6, NORMAL, J_DEF)
UNLOOP:
    jp _DOCOL
    .dw 2RDROP
    .dw EXIT
;FORGET does not work yet.
FORGET_DEF:
    MAKEWORD("FORGET", 6, NORMAL, UNLOOP_DEF)
FORGET:
    jp _DOCOL
    .dw WORD, FIND
    .dw DUP, FETCH, LATEST, STORE
    .dw HERE, STORE 
    .dw EXIT
CONSTANT_DEF:
    MAKEWORD("CONSTANT", 8, NORMAL, FORGET_DEF)
CONSTANT:
    jp _DOCOL
    .dw CREATE, LIT, 2, HERE, SUBSTORE
    .dw DOCON
    .dw COMMA, SEMICOLON
    .dw EXIT
    .dw LATEST, FETCH, CFA, LIT, 7, ADD
    .dw LIT, FETCH, SWAP, STORE
    .dw EXIT
    
;breaks dictionary for some reason
;COMMABYTE_DEF:
;    MAKEWORD("C,", 2, NORMAL, CONSTANT_DEF)
;COMMABYTE:
;    jp _DOCOL
;    .dw HERE, FETCH, STOREBYTE
;    .dw LIT, 1, ALLOT
;    .dw EXIT
;
;RECURSE compiles the word currently being defined.
RECURSE_DEF:
    MAKEWORD("RECURSE", 7, IMD_VAL, CONSTANT_DEF)
RECURSE:
    jp _DOCOL
    .dw NEXTLATEST, FETCH, CFA, COMMA
    .dw EXIT

ALLOT_DEF:
    MAKEWORD("ALLOT", 5, NORMAL, RECURSE_DEF)
ALLOT:
    ld hl, (HERE_VAL)
    add hl, bc
    pop bc
    ld (HERE_VAL), hl
    NEXT
VARIABLE_DEF:
    MAKEWORD("VARIABLE", 8, NORMAL, ALLOT_DEF)
VARIABLE:
    jp _DOCOL
    .dw CREATE, LIT, 2, HERE, SUBSTORE
    .dw DOVAR
    .dw LIT, 0, COMMA, SEMICOLON
    .dw EXIT
LDSCRN_DEF
    MAKEWORD("LDSCRN", 6, NORMAL, VARIABLE_DEF)
LDSCRN:
    push de
    B_CALL(_GrBufCpy)
    pop de
    NEXT
;CLRSCRN clears the screen by 0ing out the memory map of the screen.
CLRSCRN_DEF:
    MAKEWORD("CLRSCRN", 7, NORMAL, LDSCRN_DEF)
CLRSCRN:
    push de
    push bc
    B_CALL(_GrBufClr)
    pop bc
    pop de
    NEXT
    
;DISPMEM displays an area of memory as 68 rows of 12 bytes. ( addr -- ).
DISPMEM_DEF:
    MAKEWORD("DISPMEM", 7, NORMAL, CLRSCRN_DEF)
DISPMEM:
    ld h, b
    ld l, c
    push de
    B_CALL(_BufCpy)
    pop de
    pop bc
    NEXT
;SCRN is a constant that puts plotSScreen in stack.
SCRN_DEF:
    MAKEWORD("SCRN", 4, NORMAL, DISPMEM_DEF)
SCRN:
    push bc
    ld bc, plotSScreen
    NEXT
;COMMENT ( begins comment and searchs for a ) on the same line.
COMMENT_DEF:
    MAKEWORD('(', 1, NORMAL, SCRN_DEF)
COMMENT:
    jp _DOCOL
    .dw CHAR, LIT, ')', EQUA, ZBRANCH, -10
    .dw EXIT
    
;NEXTLATEST puts the nextlatest variable on the stack. This could be used
;in the middle of a definition to get the address of the beginning of the
;current definition.
NEXTLATEST_DEF:
    MAKEWORD("NEXTLATEST", 10, NORMAL, COMMENT_DEF)
NEXTLATEST:
    push bc
    ld bc, NEXT_LATEST
    NEXT
;LIT takes next word and puts it on stack,
;then causes IP to skip over it
LIT_DEF:
    MAKEWORD("LIT", 3, NORMAL, NEXTLATEST_DEF)
LIT:
    push bc
    ex de, hl
    ld c, (hl)
    inc hl
    ld b, (hl)
    inc hl
    SHORTNEXT
;! stores actual top of stack to address in TOS
;register bc, and pops next value off to TOS
STORE_DEF:
    MAKEWORD('!', 1, NORMAL, LIT_DEF)
STORE:
    ld h, b
    ld l, c
    pop bc
    ld (hl), c
    inc hl
    ld (hl), b
    pop bc
    NEXT
;@ fetches data from where TOS points and puts it in TOS.
FETCH_DEF:
    MAKEWORD('@', 1, NORMAL, STORE_DEF)
FETCH:
    ld h, b
    ld l, c
    ld c, (hl)
    inc hl
    ld b, (hl)
    NEXT
ADDSTORE_DEF:
    MAKEWORD("+!", 2, NORMAL, FETCH_DEF)
ADDSTORE:
    jp _DOCOL
    .dw DUP, FETCH, ROT, ADD, SWAP, STORE
    .dw EXIT
SUBSTORE_DEF:
    MAKEWORD("-!", 2, NORMAL, ADDSTORE_DEF)
SUBSTORE:
    jp _DOCOL
    .dw DUP, FETCH, ROT, SUB, SWAP, STORE
    .dw EXIT
;C! stores only LS byte into stack.
STOREBYTE_DEF:
    MAKEWORD("C!", 2, NORMAL, SUBSTORE_DEF)
STOREBYTE:
    pop hl
    ld a, l
    ld (bc), a
    pop bc
    NEXT
FETCHBYTE_DEF:
    MAKEWORD("C@", 2, NORMAL, STOREBYTE_DEF)
FETCHBYTE:
    ld h, b
    ld l, c
    ld c, (hl)
    ld b, 0
    NEXT
HERE_DEF:
    MAKEWORD("HERE", 4, NORMAL, FETCHBYTE_DEF)
HERE:
    push bc
    ld bc, HERE_VAL
    NEXT
STATE_DEF:
    MAKEWORD("STATE", 5, NORMAL, HERE_DEF)
STATE:
    push bc
    ld bc, STATE_VAL
    NEXT
PAD_DEF:
    MAKEWORD("PAD", 3, NORMAL, STATE_DEF)
PAD:
    push bc
    ld bc, PAD_VAL
    NEXT
;S0 is a constant holding the bottom of the stack.
SZERO_DEF:
    MAKEWORD("S0", 2, NORMAL, PAD_DEF)
SZERO:
    push bc
    ld bc, PS_LOCATION
    dec bc
    dec bc
    dec bc
    dec bc
    NEXT
BASE_DEF:
    MAKEWORD("BASE", 4, NORMAL, SZERO_DEF)
BASE:
    push bc
    ld bc, BASE_VAL
    NEXT
INBUF_DEF:
    MAKEWORD("INBUF", 5, NORMAL, BASE_DEF)
INBUF:
    push bc
    ld bc, INBUF_VAL
    NEXT
;>IN is a variable holding the location of the next unprocessed cell in the
;input buffer.
INNEXT_DEF:
    MAKEWORD(">IN", 3, NORMAL, INBUF_DEF)
INNEXT:
    push bc
    ld bc, KEY_NEXT
    NEXT
DSPFETCH_DEF:
    MAKEWORD("DSP@", 4, NORMAL, INNEXT_DEF)
DSPFETCH:
    push bc
    ld hl, 0
    add hl, sp
    ld b, h
    ld c, l
    ;push bc
    ;ld (STACK_POS), sp
    ;ld hl, (STACK_POS)
    ;inc hl
    ;inc hl
    ;ld b, h
    ;ld c, l
    NEXT
STACK_POS:
    .dw 0
DSPSTORE_DEF:
    MAKEWORD("DSP!", 4, NORMAL, DSPFETCH_DEF)
DSPSTORE:
    ld h, b
    ld l, c
    ld sp, hl
    pop bc
    NEXT
RSPFETCH_DEF:
    MAKEWORD("RSP@", 4, NORMAL, DSPSTORE_DEF)
    push bc
    ;ld bc, ix TODO how does one load IX?
    NEXT
RSPFETCH:
RSPSTORE_DEF:
    MAKEWORD("RSP!", 4, NORMAL, RSPFETCH_DEF)
RSPSTORE:
    ;TODO implement by changing IX
    NEXT
;TORS >R pop from parameter stack, put in return stack.
TORS_DEF:
    MAKEWORD(">R", 2, NORMAL, RSPSTORE_DEF)
TORS:
    dec ix
    ld (ix+0), b
    dec ix
    ld (ix+0), c
    pop bc
    NEXT

;FROMRS R> pop from return stack onto parameter stack.
FROMRS_DEF:
    MAKEWORD("R>", 2, NORMAL, TORS_DEF)
FROMRS:
    push bc
    ld c,(ix+0)
    inc ix
    ld b,(ix+0)
    inc ix
    NEXT

;R@ copies the first item from the return stack to the stack.
RCOPY_DEF:
    MAKEWORD("R@", 2, NORMAL, FROMRS_DEF)
RCOPY:
    push bc
    ld c, (ix+0)
    ld b, (ix+1)
    NEXT
    
TWOTORS_DEF:
    MAKEWORD("2>R", 3, NORMAL, RCOPY_DEF)
TWOTORS:
    dec ix
    ld (ix+0), b
    dec ix
    ld (ix+0), c
    pop bc
    dec ix
    ld (ix+0), b
    dec ix
    ld (ix+0), c
    pop bc
    NEXT
    
TWOFROMRS_DEF:
    MAKEWORD("2R>", 3, NORMAL, TWOTORS_DEF)
TWOFROMRS:
    push bc
    ld c,(ix+0)
    inc ix
    ld b,(ix+0)
    inc ix
    push bc
    ld c,(ix+0)
    inc ix
    ld b,(ix+0)
    inc ix
    NEXT

TWORSCOPY_DEF:
    MAKEWORD("2R@", 3, NORMAL, TWOFROMRS_DEF)
TWORSCOPY:
    push bc
    ld c, (ix+0)
    ld b, (ix+1)
    push bc
    ld c, (ix+2)
    ld b, (ix+3)
    NEXT
RDROP_DEF:
    MAKEWORD("RDROP", 5, NORMAL, TWORSCOPY_DEF)
RDROP:
    inc ix
    inc ix
    NEXT
TWORDROP_DEF:
    MAKEWORD("2RDROP", 6, NORMAL, RDROP_DEF)
TWORDROP:
    inc ix
    inc ix
    inc ix
    inc ix
    NEXT
;GT greater than.
GRT_DEF:
    MAKEWORD('>', 1, NORMAL, TWORDROP_DEF)
GRT:
    pop hl
    call HLSUBBC
    ld bc, 0
    ld a, h
    or l
    jp z, GRT_DONE
    bit 7, h
    jp nz, GRT_DONE
    ld bc, 1
GRT_DONE:
    NEXT

;LT less than.
LST_DEF:
    MAKEWORD('<', 1, NORMAL, GRT_DEF)
LST:
    pop hl
    call HLSUBBC
    ld bc, 0
    ld a, h
    or l
    jp z, LST_DONE
    bit 7, h
    jp z, LST_DONE
    ld bc, 1
LST_DONE:
    NEXT
;ADD addition.
ADD_DEF:
    MAKEWORD('+', 1, NORMAL, LST_DEF)
ADD:
    pop hl
    add hl, bc
    ld c, l
    ld b, h
    NEXT

;SUB subtraction.
SUB_DEF:
    MAKEWORD('-', 1, NORMAL, ADD_DEF)
SUB:
    pop hl
    call HLSUBBC
    ld c, l
    ld b, h
    NEXT

HLSUBBC:
    ld a, b
    cpl
    ld b, a
    ld a, c
    cpl
    ld c, a
    inc bc
    add hl, bc
    ret
    

;* ( n m -- n*m )
MULT_DEF:
    MAKEWORD('*', 1, NORMAL, SUB_DEF)
MULT:
    pop hl
    push de
    ld d, h
    ld e, l
    ld a, 16;16 bits to check
    ld hl, 0;result starts at 0
MULT_LOOP:
    ;shift and check if found a 1
    srl b
    rr c
    jr nc, SKIP_ADD	
    add hl, de   
SKIP_ADD:
    ;double de
    ;sll_D
    ;rl e
    ;dec de
    ex de, hl
    add hl, hl
    ex de, hl
    dec a
    jr nz, MULT_LOOP 
    ld b, h
    ld c, l
    pop de
    NEXT

;2* ( n -- 2*n )
TWOTIMES_DEF:
    MAKEWORD("2*", 2, NORMAL, MULT_DEF)
TWOTIMES:
    ld h, b
    ld l, c
    add hl, hl
    ld b, h
    ld c, l
    NEXT

;AND ( n m -- r ) where r is bit-wise n AND m.
AND_DEF:
    MAKEWORD("AND", 3, NORMAL, TWOTIMES_DEF)
AND:
    pop hl
    ld a, b
    and h
    ld b, a
    ld a, c
    and l
    ld c, a
    NEXT

;OR is just like AND.
OR_DEF:
    MAKEWORD("OR", 2, NORMAL, AND_DEF)
OR:
    pop hl
    ld a, b
    or h
    ld b, a
    ld a, c
    or l
    ld c, a
    NEXT

XOR_DEF:
    MAKEWORD("XOR", 3, NORMAL, OR_DEF)
    pop hl
    ld a, l
    xor c
    ld c, a
    ld a, h
    xor b
    ld b, a
    NEXT

;INVERT is bitwise invert. ( n -- m ) where m is inverted n.
INVERT_DEF:
    MAKEWORD("INVERT", 6, NORMAL, XOR_DEF)
INVERT:
    ld a, b
    cpl
    ld b, a
    ld a, c
    cpl
    ld c, a
    NEXT

;NOT is the boolean invert operation.
NOT_DEF:
    MAKEWORD("NOT", 3, NORMAL, INVERT_DEF)
NOT:
    ld a, b
    or c
    ld b, 0
    ld c, 1
    jp z, NOT_ZERO
    ld c, 0
NOT_ZERO:
    NEXT

RSHIFT_DEF:
    MAKEWORD(">>", 2, NORMAL, NOT_DEF)
RSHIFT:
    sra b
    rr c
    NEXT
LSHIFT_DEF:
    MAKEWORD("<<", 2, NORMAL, RSHIFT_DEF)
LSHIFT:
    sla c
    rl b
    NEXT
LROT_DEF:
    MAKEWORD("LROT", 4, NORMAL, LSHIFT_DEF)
LROT:
    rlc c
    NEXT

RROT_DEF:
    MAKEWORD("RROT", 4, NORMAL, LROT_DEF)
RROT:
    rrc c
    NEXT
;BIT checks a bit in a byte. ( byte bitnum -- bitval ). One byte only.
BIT_DEF:
    MAKEWORD("BIT", 3, NORMAL, RROT_DEF)
BIT:
;    pop hl
;    ld b, 0
;    ;ld a, c
;    ;cp 0
;    ;jp z, BIT_FINISH
;    ld a, l
;BIT_ROTATE:
;    srl a
;    djnz BIT_ROTATE
;    ld h, 0
;    ld l, a
;    DISPHL
;    and 1
;    ld c, a
;    NEXT
    ld hl, BIT_TABLE
    add hl, bc
    ld a, (hl)
    pop hl
    ld b, 0
    and l
    jp z, BIT_ZERO
    ld c, 1
    NEXT
BIT_ZERO:
    ld c, 0
    NEXT
BIT_TABLE:
    .db 00000001b, 00000010b, 00000100b, 00001000b
    .db 00010000b, 00100000b, 01000000b, 10000000b

;/MOD uses system call DivHLbyDE to do its division and modulous.
;Output is then in hl for quotient and de for mod, to put on stack. 
;( n m -- mod quotient )
DIVMOD_DEF:
    MAKEWORD("/MOD", 4, NORMAL, BIT_DEF)
DIVMOD:
    pop hl
    ld (DIVMOD_DE), de
    push ix
    ld d, b
    ld e, c
    B_CALL(_DivHLbyDE)
    pop ix
    push de
    ld b, h
    ld c, l
    ld de, (DIVMOD_DE)
    NEXT
DIVMOD_DE:
    .dw 0

;QKEY checks to see if a key was pressed. 0 is returned if no key, otherwise
;TOS is the keycode.
QKEY_DEF:
    MAKEWORD("?KEY", 4, NORMAL, DIVMOD_DEF)
QKEY:
    push de
    push ix
    B_CALL(_GetCSC)
    pop ix
    pop de
    ld b, 0
    ld c, a
    NEXT
    
;KEY gets one key and puts it into bc
;letters and numbers are translated from a table and everything else from
;individual checks
KEY_DEF:
    MAKEWORD("KEY", 3, NORMAL, QKEY_DEF)
KEY:
    push bc
    call _KEY
    ld c, a
    ld b, 0
    NEXT

;_KEY fragment is used by KEY and INTERPRET to get input.
;returns key code in a.
_KEY:
    push de
    push hl
    push bc
    B_CALL(_GetKey)
    ;save key in d
    ld d, a
    ld hl, GETKEY_TABLE
    ld bc, GETKEY_TABLE_END - GETKEY_TABLE
    cpir
    jp nz, KEY_ALPHANUM
;use offset to find printable character in CHAR_TABLE
GETKEY_FOUND:
    ld bc, GETKEY_TABLE
    sbc hl, bc
    dec hl
    ld bc, CHAR_TABLE
    add hl, bc
    ld a, (hl)
    jp KEY_DONE
;translate it to the appropriate letter or number
KEY_ALPHANUM:
    ld a, d
    ld l,k0
    sub l
    ld de, ALPHANUM_TABLE
    ld h, 0
    ld l, a
    add hl, de
    ld a, (hl)
KEY_DONE:
    pop bc
    pop hl
    pop de
    ret

;EMIT prints character on TOS.
EMIT_DEF:
    MAKEWORD("EMIT", 4, NORMAL, KEY_DEF)
EMIT:
    ld a, c
    B_CALL(_PutC)
    pop bc
    NEXT

;CR moves to next line. The bcall is used because
;even if where easier to just set CurCol and CurRow,
;newline will scroll for us
CR_DEF:
    MAKEWORD("CR", 2, NORMAL, EMIT_DEF)
CR:
    call _CR
    NEXT
_CR:
    push ix
    push bc
    push de
    B_CALL(_NewLine)
    pop de
    pop bc
    pop ix
    ret
;CLR clears the whole screen with the B_CALL
CLR_DEF:
    MAKEWORD("CLR", 3, NORMAL, CR_DEF)
CLR:
    push de
    push bc
    B_CALL(_ClrLCDFull)
    pop bc
    pop de
    NEXT

CURSORON_DEF:
    MAKEWORD("CURSORON", 8, NORMAL, CLR_DEF)
CURSORON:
    push de
    B_CALL(_CursorOn)
    pop de
    NEXT

CURSOROFF_DEF:
    MAKEWORD("CURSOROFF", 9, NORMAL, CURSORON_DEF)
CURSOROFF:
    push de
    B_CALL(_CursorOFF)
    pop de
    NEXT

;BYE exits forth and returns to the OS.
BYE_DEF:
    MAKEWORD("BYE", 3, NORMAL, CURSOROFF_DEF)
BYE:
QUIT_PROG:
    ;ld hl, DICT_SIZE
    ;bcall(_DeallocFPS)
    ;B_CALL(_CloseProg)
    ld sp, (OLD_SP)
    ret
    ;no NEXT cause this ret exits the whole system.

;WORD ( -- addr n ) where
;n is the length and addr will be the start for word's internal buffer.
;Max word size is now 31 bytes.
WORD_DEF:
    MAKEWORD("WORD", 4, NORMAL, BYE_DEF)
WORD:
    push bc
    call _WORD
    ld b, 0
    ld c, a
    push hl
    NEXT
;now stack should have start addr of word, and bc its length.

;The _WORD code fragment reads from value in KEY_NEXT ignoring leading white
;space until it hits a whitespace character. It will return size in
;register a and location in hl (even though location will always be WORDBUF).
_WORD:
;Initialize
    ;push de
    ;push bc
    ld (WORD_DE), de
    ld (WORD_BC), bc
    ld de, (KEY_NEXT)
;ignore leading whitespace by moving over it
WHITESPACE_LOOP:
    ld hl, (BUFTOP)
    sbc hl, de
    jp m, NO_WORD
    jp z, NO_WORD
    ld a, (de)
    ld hl, WS_TABLE
    ld bc, WS_TABLE_END - WS_TABLE
    cpir
    jp z, SKIP_SPACE
    jp LOOP_INIT
SKIP_SPACE:
    inc de
    jp WHITESPACE_LOOP
LOOP_INIT:
    ld h, d
    ld l, e
    ld de, (WORDBUF)
    ;push hl
    ld (WORD_CUR), hl
;de holds word buffer start, hl the next location in the input buffer.
;now just load each character from input buffer to word's internal buffer,
WORD_LOOP:
    ;pop hl
    ld hl, (WORD_CUR)
    ldi
    ld (WORD_CUR), hl
    ;push hl
    ld a, (hl)
    ld hl, WS_TABLE
    ld bc, WS_TABLE_END - WS_TABLE
    cpir
    jp z, WORD_FINISH
    jp WORD_LOOP
;hit top of buffer before finding word, return 0 as length
NO_WORD:
    ;pop bc
    ;pop de
    ld de, (WORD_DE)
    ld bc, (WORD_BC)
    ld hl, (INBUF_VAL)
    ld (KEY_NEXT), hl
    ld (BUFTOP), hl
    ld hl, (WORDBUF)
    ld a, 0
    ret

WORD_FINISH:
    ;pop hl
    ld hl, (WORD_CUR)
    ld a, 0
    ld (de), a
    inc hl
    ld (KEY_NEXT), hl
    ld h, d
    ld l, e
    ld bc, (WORDBUF)
    sbc hl, bc
    ;hl now has word length
    ld a, l
    ld hl,(WORDBUF)
    ;pop bc
    ;pop de
    ld de, (WORD_DE)
    ld bc, (WORD_BC)
    ret
WORD_DE:
    .dw 0
WORD_BC:
    .dw 0
WORD_CUR:
    .dw 0

;NUMBER parses number in input buffer and attempts to make it
;a string. ( addr l -- n m ) addr is address of string and l is
;the length. n is the number parsed, and m is the
;number of characters that were not digits in this base (actually
;right now it is just the error code (1 is not number error, 0
;is number is fine))
NUMBER_DEF:
    MAKEWORD("NUMBER", 6, NORMAL, WORD_DEF)
NUMBER:
    pop hl
    call _NUMBER
    ;hl has number, bc has error, 1 (not num) or 0 (no prob)
    ;bc is not moved or anything since it is already TOS
    push hl
    NEXT
;_NUMBER turns a string from location in hl and length in bc
;to a binary number left in hl, error level (1 or 0) left in bc
RUNNING_SUM:
    .dw 0
BASE_TO_POW:
    .dw 1
STR_SIZE:
    .db 0
_NUMBER:
    push de
    ld a, c
    ld (STR_SIZE), a
    add hl, bc
    dec hl;hl is now at end letter of word
    ld de, 0
    ld (RUNNING_SUM), de
    ld de, 1
    ld (BASE_TO_POW), de
LOOP_TO_NUM:
    ld a, (STR_SIZE)
    ld e, a
    cp 0
    jp z, STRING_IS_NUM
    ld a, e
    dec a
    ld (STR_SIZE), a
    ;hl now holds address of current letter
    ;this next bit turns the character code into the number it represents
    ;and will adjust for letters before moving on
PROCESS_DIGIT:
    ld a, (hl)
    ld e, a
    dec hl
    ;check if it is a negative sign
    cp '-'
    jp z, NEGATIVE_NUMBER
    ld a, e
    ;ascii trick to get numerical value
    sub '0'
    ld e, a
    cp 10
    jp c, ADD_TO_RUNNING_SUM
    ;subtract 7 to adjust characters to their proper number.
    ld a, e
    sub 7
    ld e, a
    ;a now holds the number of the digit
ADD_TO_RUNNING_SUM:
    ;check if outside base
    ld a, e
    push hl
    ld h, a
    ld a, (BASE_VAL)
    ;BASE is decr so a number higher than the allowed will cause a carry.
    dec a
    cp h
    jp c, NUMBER_ERROR
    ld a, h
    ld de, (BASE_TO_POW)
    B_CALL(_MultAbyDE)
    ;save result in bc for later
    ld b, h
    ld c, l
    ;now update BASE_TO_POWER
    ld de, (BASE_TO_POW)
    ld a, (BASE_VAL)
    B_CALL(_MultAbyDE)
    ;save new BASE_TO_POWER
    ld (BASE_TO_POW), hl
    ;add to running sum
    ld hl, (RUNNING_SUM)
    add hl, bc
    ;save back
    ld (RUNNING_SUM), hl
    ;get back addr
    pop hl
    ;loop
    jp LOOP_TO_NUM
NEGATIVE_NUMBER:
    ld hl, (RUNNING_SUM)
    ld a, h
    cpl
    ld h, a
    ld a, l
    cpl
    ld l, a
    inc hl
    jp END_AND_RETURN
STRING_IS_NUM:
    ld hl, (RUNNING_SUM)
END_AND_RETURN:    
    pop de
    ld bc, 0
    ret
NUMBER_ERROR:
    pop hl
    pop de
    ld bc, 1
    ret

;FIND ( addr n -- addr2 ) addr is of word, and n is the length.
;addr2 is the link to the word that matches this word.
FIND_DEF:
    MAKEWORD("FIND", 4, NORMAL, NUMBER_DEF)
FIND:
    pop hl
    call _FIND
    NEXT

;addr in hl, n in bc
;returns bc as addr
_FIND:
    ld (FIND_ADDR), hl
    ld hl, (LATEST_VAL)
    ld (FIND_DE), de
    ld (FIND_LENGTH), bc

;c should have length, hl addr to check against
FIND_LOOP:
    ld a, h
    or l
    jp z, NOT_IN_DICT
    ;check if it is the same size
    push hl
    inc hl
    inc hl
    ld a, (hl)
    cp c;c holds size
    jp nz, NEXT_WORD
    inc hl
    ld de, (FIND_ADDR)
;hl holds start of this next string to check. de holds addr of string we are 
;finding. bc still holds length of string. 
LETTER_LOOP:
    ld a, (de)
    inc de
    cpi
    jp nz, NEXT_WORD
    ld a, c
    cp 0
    jp z, WORD_FOUND
    jp LETTER_LOOP

NEXT_WORD:
    pop hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    ex de, hl
    ld bc, (FIND_LENGTH)
    jp FIND_LOOP

NOT_IN_DICT:
    ld bc, 0
    jp END_FIND

WORD_FOUND:
    pop bc
END_FIND:
    ld de, (FIND_DE)
    ret

FIND_DE:
    .dw 0
FIND_ADDR:
    .dw 0
FIND_LENGTH:
    .dw 0

;CFA gets the addr of the first instruction in the code field
;( addr -- cfa ) addr of word in dictionary gets cfa (code field addr)
CFA_DEF:
    MAKEWORD("CFA", 3, NORMAL, FIND_DEF)
CFA:
    call _CFA
    NEXT
;this fragment takes bc as addr of a word and returns addr of code field in bc
_CFA:
    inc bc
    inc bc
    ld h, b
    ld l, c
    ld b, 0
    ld c, (hl)
    add hl, bc
    inc hl
    inc hl
    ;hl points to code field
    ld b, h
    ld c, l
    ret
DFA_DEF:
    MAKEWORD("DFA", 3, NORMAL, CFA_DEF)
DFA:
    call _CFA
    inc bc
    inc bc
    inc bc
    NEXT

EXECUTE_DEF:
    MAKEWORD("EXECUTE", 7, NORMAL, DFA_DEF)
EXECUTE:
    ld h, b
    ld l, c
    pop bc
    jp (hl)
    NEXT
;untested
NONAME_DEF:
    MAKEWORD(":NONAME", 7, NORMAL, EXECUTE_DEF)
NONAME:
    jp _DOCOL
    .dw LIT, 0, LIT, 0, HEADER
    .dw HERE, FETCH, DOCOL, RBRAC
    .dw EXIT
;STI sets text inverse
STI_DEF:
    MAKEWORD("STI", 3, NORMAL, NONAME_DEF)
STI:
    set textInverse, (iy+textFlags)
    NEXT

;RTI sets text back to normal
RTI_DEF:
    MAKEWORD("RTI", 3, NORMAL, STI_DEF)
RTI:
    res textInverse, (iy+textFlags)
    NEXT

;CHAR puts the first character of the next word on the stack.
CHAR_DEF:
    MAKEWORD("CHAR", 4, NORMAL, RTI_DEF)
CHAR:
    push bc
    call _WORD
    ld b, 0
    ld c, (hl)
    NEXT
;DOES> for defining words starting with CREATE.
DOES_DEF:
    MAKEWORD("DOES>", 5, IMD_VAL, CHAR_DEF)
DOES:
    push de
    push bc
    ld hl, (HERE_VAL)
    ld de, DOES2
    ld (hl), e
    inc hl
    ld (hl), d
    inc hl
    ex de, hl
    ld hl, CALL_DODOES
    ld bc, CALL_DODOES_END-CALL_DODOES
    ldir
    ld (HERE_VAL), de
    pop bc
    pop de
    NEXT
CALL_DODOES:
    call _DODOES
CALL_DODOES_END:
DOES2_DEF:
    MAKEWORD("DODOES>", 7, NORMAL, DOES_DEF)
DOES2:
    ld hl, (NEXT_LATEST)
    ld (LATEST_VAL), hl
    ld b, h
    ld c, l
    call _CFA
    ld h, b
    ld l, c
    ;ld hl, (HERE_VAL)
    inc hl
    ld (hl), e
    inc hl
    ld (hl), d
    jp EXIT
;CREATE makes the header for a word from next in input stream.  ( "name" -- )
CREATE_DEF:
    MAKEWORD("CREATE", 6, NORMAL, DOES2_DEF)
CREATE:
    ld (CREATE_DE), de
    push bc
    ld hl, (HERE_VAL)
    ld (NEXT_LATEST), hl;so can still find prev def while compiling
    ld de, (LATEST_VAL)
    ;load link to previous
    ld (hl), e
    inc hl
    ld (hl), d
    inc hl
    ex de, hl
    call _WORD
    ex de, hl
    ld c, a
    ld b, 0
    ;now load counted string
    ld (hl), c
    inc hl
    ex de, hl
    ;load word from addr to HERE
    ldir
    ex de, hl
    ld (hl), NORMAL;can be set after creation to IMD_VAL
    inc hl
    ;docol word by default
    ld (hl), JP_BYTE
    ld de, _DOCOL
    inc hl
    ld (hl), e
    inc hl
    ld (hl), d
    inc hl
    ld (HERE_VAL), hl
    ld de, (CREATE_DE)
    pop bc
    NEXT
CREATE_DE:
    .dw 0

;COMMA appends TOS to HERE and increments HERE
COMMA_DEF:
    MAKEWORD(CHAR_COMMA, 1, NORMAL, CREATE_DEF)
COMMA:
    call _COMMA
    pop bc
    NEXT

;this fragment takes in bc and puts it HERE, updating HERE
_COMMA:
    ld hl, (HERE_VAL)
    ld (hl), c
    inc hl
    ld (hl), b
    inc hl
    ld (HERE_VAL), hl;update HERE to 2 bytes later
    ret

;LBRAC
LBRAC_DEF:
    MAKEWORD(LBRACK_CHAR, 1, IMD_VAL, COMMA_DEF)
LBRAC:
    ld a, INTERPRET_MODE
    ld (STATE_VAL), a
    NEXT
;RBRAC
RBRAC_DEF:
    MAKEWORD(']', 1, NORMAL, LBRAC_DEF)
RBRAC:
    ld a, COMPILE_MODE
    ld (STATE_VAL), a
    NEXT
;COLON starts a definition.
COLON_DEF:
    MAKEWORD(':', 1, NORMAL, RBRAC_DEF)
COLON:
    jp _DOCOL
    .dw CREATE;, LIT, 2, HERE, SUBSTORE 
    ;.dw DOCOL
    .dw RBRAC
    .dw EXIT

UPDATE_LATEST:
    ld hl, (NEXT_LATEST)
    ld (LATEST_VAL), hl
    NEXT
;SEMICOLON is translated from imaginary i character right now.
SEMICOLON_DEF:
    MAKEWORD(';', 1, IMD_VAL, COLON_DEF)
SEMICOLON:
    jp _DOCOL
    .dw UPDATE_LATEST
    ;.dw LIT, EXIT, COMMA
    .dw COMPILE, EXIT
    .dw LBRAC
    .dw EXIT

;Unfortunate hack to make sure previous definition is availible
;while current one is being compiled. A "hidden" flag would fix this.
;IMMEDIATE
IMMEDIATE_DEF:
    MAKEWORD("IMMEDIATE", 9, IMD_VAL, SEMICOLON_DEF)
IMMEDIATE:
    push bc
    ld bc, (LATEST_VAL)
    ld a, (STATE_VAL)
    cp COMPILE_MODE
    jp nz, IMD_MODE
    ld bc, (NEXT_LATEST)
IMD_MODE:
    call _CFA
    dec bc
    ld h, b
    ld l, c
    ld (hl), IMD_VAL
    pop bc
    NEXT

;BRANCH adds the offset it expects just after it to BRANCH location + 3.
;It seems to work if its done from [BRA][NCH][OFF][SET] the OFF cell.
BRANCH_DEF:
    MAKEWORD("BRANCH", 6, NORMAL, IMMEDIATE_DEF)
BRANCH:
    ex de, hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    dec hl
    add hl, de
    SHORTNEXT

;ZBRANCH
ZBRANCH_DEF:
    MAKEWORD("0BRANCH", 7, NORMAL, BRANCH_DEF)
ZBRANCH:
    ld a, c
    or b
    pop bc
    jp z, BRANCH
_ZBRANCH_SKIP:
    ;skip over offset
    inc de
    inc de
    NEXT

;TELL prints string ( addr n -- ) addr of string an length n.
TELL_DEF:
    MAKEWORD("TELL", 4, NORMAL, ZBRANCH_DEF)
TELL:
    pop hl
    ;IDK what _PutPS destroys, so I'll just be safe
    push de
    B_CALL(_PutPS)
    pop de
    pop bc
    NEXT

INCLUDE_DEF:
    MAKEWORD("INCLUDE", 7, NORMAL, TELL_DEF)
INCLUDE:
    jp _DOCOL
    .dw FINDFILE, DROP
    .dw INTERPRET_FILE
    .dw RESET
    .dw EXIT
RESET:
BREAKPOINT:
    ld hl, FOREVER
    B_CALL(_PutS)
LOOP_FOREVER:
    jp LOOP_FOREVER
FOREVER:
    .db "LOOPING!",0
    ld hl, INBUF_LOCATION
    ld (INBUF_VAL), hl
    jp RESET_FROM_NO_INPUT
INTERPRET_FILE:
    ld h, b
    ld l, c
    ld (INBUF_VAL), hl
    ld (KEY_NEXT), hl
    ld bc, -1 ;a really large number
    ld a, kEnter
    cpir
    ld (BUFTOP), hl
    pop bc
    jp PARSE_LOOP

;FIND_FILE looks ahead to the next word and push the address of the start of
;its data if the file is found. ( "name" -- fh error )
;TODO replace with more forthy file handling 
FINDFILE_DEF:
    MAKEWORD("FINDFILE", 8, NORMAL, INCLUDE_DEF)
FINDFILE:
    push bc
    push de
    call _WORD
    ;load a null byte at the end in case name is too small
    ld d, h
    ld e, l
    ld b, 0
    ld c, a
    add hl, bc
    ld (hl), 0
    inc bc;to include null byte
    ld h, d
    ld l, e
    ld de, OP1
    ex de, hl
    ld (hl), ProgObj
    inc hl
    ex de, hl
    ;now hl holds word, bc holds length, de holds OP1 location
    ldir
    push ix
    B_CALL(_ChkFindSym)
    pop ix
    jp c, FILE_NOT_FOUND
    xor a
    cp b
    jp nz, FILE_NOT_FOUND
    ;now de holds data of file
    ;skip over header
    inc de
    inc de
    inc de
    inc de
    ld h, d
    ld l, e
    ld bc, 0
    ;TOS holds addr to load from.
    pop de
    push hl
    NEXT
FILE_NOT_FOUND:
    ;call _CR
    ;ld hl, FILE_NOT_FOUND_MSG
    ;B_CALL(_PutS)
    ;call _CR
    pop de
    ld bc, 1
    NEXT
FILE_NOT_FOUND_MSG:
    .db "FILE NOT FOUND OR IS ARCHIVED", 0
    
;QUIT
QUIT_DEF:
    MAKEWORD("QUIT", 4, NORMAL, FINDFILE_DEF)
QUIT:
    jp _DOCOL
    .dw CLRRSP
QUIT_LOOP:
    .dw INTERPRET
    .dw BRANCH
    .dw -4 ;loop back to interpret
    ;Should never reach here, so doesn't need to EXIT

;this fragment is only for QUIT to clear the return stack
CLRRSP:
    ld ix, RS_LOCATION
    NEXT

;TICK puts xt of next word on stack
TICK_DEF:
    MAKEWORD('`', 1, NORMAL, QUIT_DEF)
TICK:
    jp _DOCOL
    .dw WORD
    .dw FIND
    .dw CFA
    .dw EXIT

;U. just prints using DispHL right now.
UDOT_DEF:
    MAKEWORD("U.", 2, NORMAL, TICK_DEF)
UDOT:
    ld l, c
    ld h, b
    push de
    B_CALL(_DispHL)
    pop de
    pop bc
    NEXT
    
    
;DOT does not listen to base right now.
DOT_DEF:
    MAKEWORD('.', 1, NORMAL, UDOT_DEF)
DOT:
    ld l, c
    ld h, b
    bit 7, h
    jp z, DOT_DISP
    ld a, h
    cpl
    ld h, a
    ld a, l
    cpl
    ld l, a
    inc hl
    ld a, '-'
    B_CALL(_PutC)
DOT_DISP:
    push de
    B_CALL(_DispHL)
    pop de
    pop bc
    NEXT
;? not found right now    
QUEST_DEF:
    MAKEWORD('?', 1, NORMAL, DOT_DEF)
QUEST:
    jp _DOCOL
    .dw FETCH, DOT
    .dw EXIT

;Built in constants should react to BASE. Therefore, only -1, 0, 1, and 10
;can be assumed to be good values.
ONE_DEF:
    MAKEWORD('1', 1, NORMAL, DOT_DEF)
ONE:
    push bc
    ld bc, 1
    NEXT
;TEN will always be the base value
TEN_DEF:
    MAKEWORD("10", 2, NORMAL, ONE_DEF)
TEN:
    push bc
    ld b, 0
    ld a, (BASE_VAL)
    ld c, a
    NEXT
NEGONE_DEF:
    MAKEWORD("-1", 2, NORMAL, TEN_DEF)
NEGONE:
    push bc
    ld bc, -1
    NEXT
ZERO_DEF:
    MAKEWORD('0', 1, NORMAL, NEGONE_DEF)
ZERO:
    push bc
    ld bc, 0
    NEXT
;MEMFREE pushes amount of free memory on the stack.
MEMFREE_DEF:
    MAKEWORD("MEMFREE", 7, NORMAL, ZERO_DEF)
MEMFREE:
    push bc
    B_CALL(_MemChk)
    ld b, h
    ld c, l
    NEXT

;BIN sets base to 10
BIN_DEF:
    MAKEWORD("BIN", 3, NORMAL, MEMFREE_DEF)
BIN:
    ld hl, 2
    ld (BASE_VAL), hl
    NEXT

;DECIMAL sets base to 10
DECIMAL_DEF:
    MAKEWORD("DECIMAL", 7, NORMAL, BIN_DEF)
DECIMAL:
    ld hl, 10
    ld (BASE_VAL), hl
    NEXT

;HEX simply sets BASE_VAL to 16
HEX_DEF:
    MAKEWORD("HEX", 3, NORMAL, DECIMAL_DEF)
HEX:
    ld hl, 16
    ld (BASE_VAL), hl
    NEXT

;INTERPRET is the interpreter/compiler! Very important stuff.
;What the interpreter does:
;check if there is input in buffer (buftop == buffer start)
;if there is not input, loop _GetKey and store in buffer until kEnter
;loop back to begining, checking for ctrl characters.
;now there must be input, so begin processing by entering a word loop
;after get word, if got a word
;if not, exit to repeat loop
;if so, try find
;if not word in dict, try number. If error print err message
;if number works and in interpret mode, put on TOS and loop
;if number works and in compile mode, put HERE, update HERE, loop
;if in dict and in interpret mode, run word
;if in dict and in compile mode check if it is an IMMEDIATE word
;if it is an IMMEDIATE mode word, run it
;if it is not then get code word and append it to HERE, and loop.
INTERPRET_DEF:
    MAKEWORD("INTERPRET", 9, NORMAL, HEX_DEF)
INTERPRET:
    ld (INTERPRET_DE), de
    ld (INTERPRET_BC), bc

OUTER_LOOP:
    ld hl, (BUFTOP)
    ld de, (KEY_NEXT)
    sbc hl, de
    jp z, RESET_FROM_NO_INPUT
    jp m, RESET_FROM_NO_INPUT
    jp PARSE_LOOP

RESET_FROM_NO_INPUT:
    ld a, (STATE_VAL)
    cp 0
    ld hl, COMPILE_MSG
    jp nz, CONT_RESET
    ld hl, OKAY_MSG
CONT_RESET:
    B_CALL(_PutS)
    call _CR
    ld hl, INBUF_LOCATION
    ld (INBUF_VAL), hl
    ld de, (INBUF_VAL)
    ld (KEY_NEXT), de
    ld (BUFTOP), de

;Get a line to process
GETLINE:
    ld a, BLANK_SQUARE
    B_CALL(_PutMap)
    call _KEY
    push af
    ld hl, CTRL_CHARS
    ld bc, CTRL_CHARS_END - CTRL_CHARS
    cpir
    jp z, CTRL_PROCESS
    pop af
CONT_GETKEY:
    ld (de), a
    inc de
    B_CALL(_PutC)
    jp GETLINE

INIT_PARSE:
    ld (BUFTOP), de
    ;ld a, BLANK_SQUARE
    ;B_CALL(_PutMap)

;Process input
PARSE_LOOP:
    call _WORD;will load from KEY_NEXT
    ;location in hl, size in a
    ld b, 0
    ld c, a
    ;also save length in de in case we need it later
    ld d, 0
    ld e, a
    cp 0
    ;Word length == 0?
    jp z, EXIT_POINT
PARSE_CONTINUE:
    ;must have a word then. addr in hl, length in bc
    push hl
    call _FIND
    ld a, b;b == 0 for no real accessable addr
    cp 0
    jp z, SET_LIT
    pop af;get rid of old hl in stack
    ;must not be literal then, dict addr in bc
    call _CFA
    ld h, b
    ld l, c
    ld a, (STATE_VAL)
    cp COMPILE_MODE;in compiling state?
    jp z, COMPILE_WORD

;hl holds code field address
INTERPRET_WORD:
    ld bc, (INTERPRET_BC)
    ld de, (INTERPRET_DE)
    jp (hl)
    
;hl holds code field address
COMPILE_WORD:
    ;check the immediate flag.
    dec hl
    ld a, (hl)
    inc hl
    cp IMD_VAL
    jp z, INTERPRET_WORD
    ;bc still holds addr, word is not IMMEDIATE, so must compile it.
    ;hl now holds code field
    ld b, h
    ld c, l
    call _COMMA
    ;word is now compiled and HERE is updated
    jp EXIT_POINT

;hl holds word's address, bc its size
SET_LIT:
    ;set up number call
    pop hl;get back addr of word internal buffer
    push hl;save in case not a number
    ;recover length saved in de
    ld b, d
    ld c, e
    call _NUMBER
    ld a, c
    cp 1;1 in bc means error
    jp z, NOT_FOUND_ERROR
    pop af;get rid of addr of word internal buffer
    ld a, (STATE_VAL)
    cp 1;in compiling state?
    jp z, COMPILE_LIT
    ;else move on to INTERPRET_LIT

;hl holds value
INTERPRET_LIT:
    ld bc, (INTERPRET_BC)
    push bc
    ld (INTERPRET_BC), hl
    jp EXIT_POINT
    
;hl holds value
COMPILE_LIT:
    ld d, h
    ld e, l
    ld bc, LIT
    call _COMMA
    ld b, d
    ld c, e
    call _COMMA
    jp EXIT_POINT

EXIT_POINT:
    ld de, (INTERPRET_DE)
    ld bc, (INTERPRET_BC)
    NEXT

NOT_FOUND_ERROR:
    call _CR
    ld hl, NOT_FOUND_MSG
    call _CR
    B_CALL(_PutS)
    pop hl
    B_CALL(_PutS)
    ;clear stack on error
    ld sp, PS_LOCATION
    jp EXIT_POINT

NOT_FOUND_MSG:
    .db "Word not found: ", 0
COMPILE_MSG:
    .db " COMPILED", 0
OKAY_MSG:
    .db " OK", 0
INTERPRET_DE:
    .dw 0
INTERPRET_BC:
    .dw 0

;CTRL character functions.
CTRL_PROCESS:
    pop af
    ld bc, CTRL_CHARS
    sbc hl, bc
    dec hl
    add hl, hl
    ld bc, CTRL_TABLE
    add hl, bc
    ld c, (hl)
    inc hl
    ld b, (hl)
    ld h, b
    ld l, c
    jp (hl)
        
kEnter_PROCESS:
    ld a, kEnter
    ld (de), a
    inc de
    jp INIT_PARSE

kLeft_PROCESS:
    ld hl, (INBUF_VAL)
    sbc hl, de
    jp p, GETLINE
    ld a, BLANK_SQUARE
    B_CALL(_PutMap)
    dec de
    ld hl, (curCOl)
    dec hl
    ld (curCol), hl
    jp GETLINE

kRight_PROCESS:
    ex de, hl
    ld a, (hl)
    ex de, hl
    B_CALL(_PutC)
    inc de
    jp GETLINE

kUp_PROCESS:
    jp GETLINE

kDown_PROCESS:
    jp GETLINE

kClear_PROCESS:
    call PRINTLN
    push de
    B_CALL(_ClrLCDFull)
    pop de
    jp CONT_RESET

kQuit_PROCESS:
    call QUIT_PROG

;ignore rest of input to process a comment
Comment_PROCESS:
    jp RESET_FROM_NO_INPUT

LATEST_DEF:
    MAKEWORD("LATEST", 6, NORMAL, INTERPRET_DEF)
LATEST:
    push bc
    ld bc, LATEST_VAL
    NEXT
;Leave LATEST as the last definition so LATEST_VAL will point to the right place.
END_OF_DICT:
;End OF Definitions
    ;.block 2000
    
;Value storage
;Holds which state we are in. 0 = interpreting, 1 = compiling.
STATE_VAL:
    .db INTERPRET_MODE
;LATEST is the most recently defined word in the dictionary.
LATEST_VAL:
    .dw LATEST_DEF
;NEXT_LATEST is used while compiling a word so that the previous definition
;is still availible in LATEST. It is copied over to LATEST after compiling.
NEXT_LATEST:
    .dw LATEST_DEF
;BASE is the current number base that we are in.
BASE_VAL:
    .db 10
;HERE is the most recently used memory in the dictionary space. Must be set
;before Forth runs. Can be END_OF_DICT is a block is there, or set to (FPS) or
;BLOCK file, but only in-file block doesn't crash real calc.
HERE_VAL:
    .dw END_PROG;END_OF_DICT ;saveSScreen
;PAD is the scratch memory for applications that don't want to use HERE.
;it is currently in the backup screen area of memory. About 400 bytes.
PAD_VAL:
    .dw PAD_LOCATION
;WORDBUF variable is the current location of the word buffer.
WORDBUF:
    .dw WORDBUF_LOCATION
;INBUF_VAL is the location of the input buffer.
INBUF_VAL:
    .dw INBUF_LOCATION
;characters that need to be translated to the corresponding character
;in CHAR_TABLE.
GETKEY_TABLE:
    .db kLeft, kRight, kUp, kDown, kAdd, kSub, kMul, kDiv, kEnter, kDecPnt
    .db kI, kStore, KCONSTeA, kQuest, kLBrack, kRBrack, kLParen, kRParen, 
    .db kColon, kComma, kEE, kMem, kCatalog, kClear, kSpace, kLn, kQuotE
    .db '\', kQuit, kMode, kExpon, kPi, kSin, kCos
    .db kTan
GETKEY_TABLE_END:
CHAR_TABLE:
    .db kLeft, kRight, kUp, kDown, '+', '-', '*', '/', kEnter, '.'
    .db ';', '!', '@', '?', LBRACK_CHAR, ']', '(', ')'
    .db ':', ',', '=', '`', ' ', kClear, ' ', kLn, "`"
    .db '\', QUIT_CHAR, QUIT_CHAR, GT_CHAR, LT_CHAR, '#', ' '
    .db ' ' 
;Table of regular characters.
ALPHANUM_TABLE:
    .db "0123456789e ABCDEFGHIJKLMNOPQRSTUVWXYZ"
;CTRL characters will interupt the interpreter and call the 
;function in CTRL_TABLE.
CTRL_CHARS:
    .db kEnter, kUp, kRight, kLeft
    .db kDown, kClear, QUIT_CHAR
CTRL_CHARS_END:
CTRL_TABLE:
    .dw kEnter_PROCESS, kUp_PROCESS, kRight_PROCESS, kLeft_PROCESS
    .dw kDown_PROCESS, kClear_PROCESS, kQuit_PROCESS
WS_TABLE:
    .db ' ', NEWLINE, kEnter, kSpace
WS_TABLE_END:
;Stores the sp from before execution.
OLD_SP:
    .dw 0
WELCOME_MSG:
    .db "Welcome to      Forth!", 0
MEM_MSG:
    .db "Free RAM:", 0
VERSION_MSG:
    .db "This is Version:"
VERSION:
    .db ".09", 0
COLD_START:
    .dw QUIT
;current start of buffer
KEY_NEXT:
    .dw INBUF_LOCATION
;current top of used part of buffer
BUFTOP:
    .dw INBUF_LOCATION+START_TEXT_END-START_TEXT
DICT_NAME:
    .db ProgObj,"BLOCK", 0

STR_TIMES:
    .db "Called:", 0
TIMES:
    .db 1
PRINTLN:
    push ix
    push hl
    push de
    push bc
    push af
    ld hl, STR_TIMES
    B_CALL(_PutS)
    ld h, 0
    ld a, (TIMES)
    ld l, a
    inc a
    ld (TIMES), a
    B_CALL(_DispHL)
    pop af
    pop bc
    pop de
    pop hl
    pop ix
    ret
END_PROG:
    .block 2000
    

.end
.end
