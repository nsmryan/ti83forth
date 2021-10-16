
	.nolist

;Macros for inlining NEXT and making words.
#define NEXT  ex de, hl
#defcont \    ld e, (hl)
#defcont \    inc hl
#defcont \    ld d, (hl)
#defcont \    inc hl
#defcont \    ex de, hl
#defcont \    jp (hl) 

#define SHORTNEXT ld e, (hl)
#defcont \        inc hl
#defcont \        ld d, (hl)
#defcont \        inc hl
#defcont \        ex de, hl
#defcont \        jp (hl) 

#define MAKEWORD(NAME,LENGTH,TYPE,PREVIOUS)  .dw PREVIOUS
#defcont \    .db LENGTH
#defcont \    .db NAME
#defcont \    .db TYPE

#define MAKEHIGHWORD(NAME,LENGTH,TYPE,PREVIOUS)  .dw PREVIOUS
#defcont \    .db LENGTH
#defcont \    .db NAME
#defcont \    .db TYPE
#defcont \    jp _DOCOL

#define LN    .db 
#define ".   , NEWLINE
;undefined instruction. shift logical left _register
#define sll_D  .dw $CB32 
#define sll_B  .dw $CB30 

#define DISPHL push de \ push bc \ push af \ push hl 
#defcont \ B_CALL(_DispHL)
#defcont \  pop hl \    pop af \    pop bc \    pop de

;Constants
_PowerOff        EQU $5008
_MultAbyDE       EQU $8045
_Disp32          EQU $51CD
_ArcChk          EQU $5014
_DivHLbyDE       EQU $804B
JP_BYTE          EQU $C3 ;jp opcode
NEWLINE          EQU 10
LBRACK_CHAR      EQU $C1
SEMICOLON_CHAR   EQU 3Bh
LT_CHAR          EQU $3C
GT_CHAR          EQU $3E
ARROW            EQU 1Ch;from z80 heaven chart
;reassign some characters to avoid collisions with used characters
QUIT_CHAR        EQU $F6
FULL_SQUARE      EQU $E0
BLANK_SQUARE     EQU $D6
CHAR_COMMA       EQU 2Ch
;System stuff
NORMAL           EQU 0
IMD_VAL          EQU 1
INTERPRET_MODE   EQU 0
COMPILE_MODE     EQU 1
;HIDDEN          EQU 2h;no hidden words for now
DICT_ALLOC       EQU 500;*9 for fps alloc will be size*9
DICT_SIZE        EQU 5000
WORDBUF_SIZE     EQU 31
PS_SIZE          EQU 96
RS_SIZE          EQU 96
INBUF_SIZE       EQU 512;128
PS_LOCATION      EQU AppBackUpScreen + PS_SIZE 
RS_LOCATION      EQU PS_LOCATION + PS_SIZE
;there is free memory after the RS in appbackupscreen
INBUF_LOCATION   EQU saveSScreen;RS_LOCATION + 1
WORDBUF_LOCATION EQU INBUF_LOCATION + INBUF_SIZE 
PAD_LOCATION     EQU WORDBUF_LOCATION + WORDBUF_SIZE
