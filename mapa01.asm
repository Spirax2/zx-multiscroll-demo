org $c000

; script action definitions
ADD_ENEMY:      EQU 0
WHEN_POS:       EQU 1
SPAWN_POWERUP:  EQU 2
TOTAL:          EQU 3
WAIT:           EQU 4
FX:             EQU 5
FXATTR:         EQU 6
CALM:           EQU 7
ANGRY:          EQU 8
SCRIPT_END:     EQU 9

; Enemy definitions
ghost01:          EQU 0
ghost02:          EQU 1
ghost03:          EQU 2
ghost04:          EQU 3
ghost05:          EQU 4
mapwidth: db 36
mapheight: db 36
start_x: dw 112
start_y: dw 496

ptr_levelscript: dw levelscript
levelmap: 
    db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  9, 10, 11,  0,  9, 10, 11,  0,  0,  0,  0,  0,  0,  0,  9, 10, 10, 10, 11,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 25, 26, 27,  0,  0,  0,  0,  0,  0, 25, 26, 27,  0,  0,  0,  0,  0,  0,  0, 25, 26, 27,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 29, 30, 31,  0,  0,  0,  0,  0,  0, 29, 30, 31,  0,  0,  0,  0,  0,  0,  0, 29, 30, 31,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0, 33, 34, 35,  0,  0,  0,  0,  0,  0, 33, 34, 35,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  1,  2,  2,  3,  0,  0,  0,  0,  9, 10, 11,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  9, 10, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0, 33, 34, 35,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 37, 38, 39,  0,  0,  0,  0,  0,  0,  0,  1,  2,  2,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 25, 26, 27,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0, 25, 26, 27,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 29, 30, 31,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 29, 30, 31,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 33, 34, 34, 34, 34, 34, 35,  0,  0,  0, 33, 34, 35,  0, 33, 34, 35,  0, 33, 34, 35,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  3,  0, 33, 34, 35,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  2,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  9, 10, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  5,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  7,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  0,  0,  1,  2,  2,  2,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 33, 34, 35,  0, 33, 34, 35,  0, 33, 34, 35,  0,  0,  0, 33, 34, 35,  0,  0, 33, 34, 35,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0, 33, 34, 34, 34, 34, 35,  0,  0,  0, 33, 34, 34, 34, 34, 35,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  3,  0,  0,  0,  0,  0,  0,  0, 33, 34, 35,  0, 33, 34, 35,  0,  0, 33, 34, 35,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  2,  2,  3,  0,  0,  0, 33, 34, 35,  0,  0,  0,  0,  0, 33, 34, 35,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  2,  2,  2,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 33, 34, 35,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  0,  0,  1,  2,  2,  2,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  9, 10, 10, 10, 11,  0,  0,  0,  9, 10, 10, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  5,  6,  7,  0,  0,  0,  0,  0,  0,  0,  0,  5,  6,  7,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0, 33, 34, 35,  0,  1,  2,  3,  0, 33, 34, 34, 34, 34, 35,  0,  1,  2,  3,  0, 33, 34, 35,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0, 33, 34, 35,  0, 33, 34, 34, 35,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  5,  6,  7,  0,  0,  0,  5,  6,  7,  0,  0,  0,  0,  5,  6,  7,  0,  0,  0,  5,  6,  7,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  1,  2,  3,  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0,  1,  2,  3,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  5,  6,  6,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 13, 14, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  0,  0,  0
    db  0,  0,  0,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  0,  0,  0
    db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    db  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0

levelscript:
;    db ADD_ENEMY, ghost01, 5, 5 
    db ADD_ENEMY, ghost01, 8, 10
    db TOTAL, 1
    db ADD_ENEMY, ghost02, 15, 5 
    db TOTAL, 0
    db ADD_ENEMY, ghost03, 5, 5 
    db ADD_ENEMY, ghost03, 5, 15 
    db ADD_ENEMY, ghost03, 15, 5 
    db ADD_ENEMY, ghost03, 15, 15 
    db TOTAL, 0
    db SCRIPT_END