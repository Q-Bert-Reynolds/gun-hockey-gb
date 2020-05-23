; sys_info masks
SYS_INFO_DMG EQU %00000000
SYS_INFO_SGB EQU %00000001
SYS_INFO_GBC EQU %00000010
SYS_INFO_GBA EQU %00000100

; banks
MAX_BANKS            EQU 128;2MB
TEMP_BANK            EQU MAX_BANKS-1
GBT_PLAYER_BANK      EQU 1
UI_BANK              EQU 2
START_BANK           EQU 3
TITLE_BANK           EQU 4
NEW_GAME_BANK        EQU 5
PLAY_BALL_BANK       EQU 6
PLAY_BALL_INTRO_BANK EQU 7
LINEUP_BANK          EQU 8
PLAYER_STRINGS_BANK  EQU 9
PLAYER_DATA_BANK     EQU 10
COACHES_BANK         EQU 11
ROLEDEX_BANK         EQU 12
SIM_BANK             EQU 13
SFX_BANK             EQU 14
SONG_BANK            EQU 15
OVERWORLD_BANK       EQU 20
MAPS_BANK            EQU 50
PLAYER_IMG_BANK      EQU 80
SGB_BANK             EQU 100

; sprite props
FLIP_X_PAL  EQU (OAMF_XFLIP | OAMF_PAL1 )
FLIP_Y_PAL  EQU (OAMF_YFLIP | OAMF_PAL1 )
FLIP_XY_PAL EQU (FLIP_X_PAL | FLIP_Y_PAL)
FLIP_XY     EQU (OAMF_XFLIP | OAMF_YFLIP)

; sprite draw flags
SPRITE_FLAGS_SKIP      EQU %00000001
SPRITE_FLAGS_CLEAR_END EQU %00000010

; ui draw flags
DRAW_FLAGS_BKG     EQU %00000000
DRAW_FLAGS_WIN     EQU %00000001
DRAW_FLAGS_PAD_TOP EQU %00000010

; UI Tiles
DOTTED_CIRCLE     EQU 10
BASEBALL          EQU 11
ARROW_LEFT        EQU 12
ARROW_RIGHT       EQU 13
ARROW_RIGHT_BLANK EQU 14
ARROW_DOWN        EQU 28
ARROW_UP          EQU 29
NUMBERS           EQU 48
BOX_UPPER_LEFT    EQU 17
BOX_UPPER_RIGHT   EQU 18
BOX_LOWER_LEFT    EQU 19
BOX_LOWER_RIGHT   EQU 20
BOX_HORIZONTAL    EQU 21
BOX_VERTICAL      EQU 22
BOX_JUNCTION      EQU 31
EMPTY_BASE        EQU 23
OCCUPIED_BASE     EQU 24
AGE               EQU 16
EARNED_RUN_AVG    EQU 25
BATTING_AVG       EQU 26
INNING_BOTTOM     EQU 28
INNING_TOP        EQU 29

;position
PITCHER          EQU 1
CATCHER          EQU 2
FIRST_BASEMAN    EQU 3
SECOND_BASEMAN   EQU 4
THIRD_BASEMAN    EQU 5
SHORTSTOP        EQU 6
LEFT_FIELDER     EQU 7
CENTER_FIELDER   EQU 8
RIGHT_FIELDER    EQU 9

;handedness
THROW_LEFT  EQU %000
THROW_RIGHT EQU %001
BAT_LEFT    EQU %010
BAT_RIGHT   EQU %100

;pitch paths
PITCH_PATH_STRAIGHT EQU 0
PITCH_PATH_CURVE    EQU 1
PITCH_PATH_FADE     EQU 2
PITCH_PATH_SLIDER   EQU 3
PITCH_PATH_RISE     EQU 4
PITCH_PATH_DROP     EQU 5
PITCH_PATH_EEPHUS   EQU 6
PITCH_PATH_SCREW    EQU 7
PITCH_PATH_SLURVE   EQU 8
PITCH_PATH_KNUCKLE  EQU 9
PITCH_PATH_CUT      EQU 10

;types
NONE     EQU 0
NORMAL   EQU 1
FIRE     EQU 2
WATER    EQU 3
ELECTRIC EQU 4
GRASS    EQU 5
ICE      EQU 6
FIGHTING EQU 7
POISON   EQU 8
GROUND   EQU 9
FLYING   EQU 10
PSYCHIC  EQU 11
BUG      EQU 12
ROCK     EQU 13
GHOST    EQU 14
DRAGON   EQU 15

;status
STATUS_OK  EQU 0
STATUS_BRN EQU 1
STATUS_FRZ EQU 2
STATUS_PAR EQU 3
STATUS_PSN EQU 4
STATUS_SLP EQU 5

MAX_MOVES        EQU 4
NAME_LENGTH      EQU 8
NICKNAME_LENGTH  EQU 12
BUFFER_SIZE      EQU 512
MAP_STEP_SIZE    EQU 16

;rolédex
PLAYER_BASE_NUM     EQU 0
PLAYER_BASE_TYPE1   EQU PLAYER_BASE_NUM     + 1
PLAYER_BASE_TYPE2   EQU PLAYER_BASE_TYPE1   + 1
PLAYER_BASE_EV_TO   EQU PLAYER_BASE_TYPE2   + 1
PLAYER_BASE_EV_TYPE EQU PLAYER_BASE_EV_TO   + 1
PLAYER_BASE_EV_AGE  EQU PLAYER_BASE_EV_TYPE + 1
PLAYER_BASE_HEIGHT  EQU PLAYER_BASE_EV_AGE  + 1
PLAYER_BASE_WEIGHT  EQU PLAYER_BASE_HEIGHT  + 1
PLAYER_BASE_HP      EQU PLAYER_BASE_WEIGHT  + 2
PLAYER_BASE_BAT     EQU PLAYER_BASE_HP      + 1
PLAYER_BASE_FIELD   EQU PLAYER_BASE_BAT     + 1
PLAYER_BASE_SPEED   EQU PLAYER_BASE_FIELD   + 1
PLAYER_BASE_THROW   EQU PLAYER_BASE_SPEED   + 1
PLAYER_BASE_BODY_ID EQU PLAYER_BASE_THROW   + 1
PLAYER_BASE_HEAD_ID EQU PLAYER_BASE_BODY_ID + 1
PLAYER_BASE_HAT_ID  EQU PLAYER_BASE_HEAD_ID + 1
PLAYER_BASE_GB_PAL  EQU PLAYER_BASE_HAT_ID  + 1
PLAYER_BASE_SGB_PAL EQU PLAYER_BASE_GB_PAL  + 1
PLAYER_BASE_ANIM    EQU PLAYER_BASE_SGB_PAL + 2
PLAYER_BASE_SIZE    EQU PLAYER_BASE_ANIM    + 32

;audio
SFX_CH_1 EQU $10
SFX_CH_2 EQU $15
SFX_CH_3 EQU $1A
SFX_CH_4 EQU $1F

;notes
C3      EQU 44
Db3     EQU 156
D3      EQU 262
Eb3     EQU 363
E3      EQU 457
F3      EQU 547
Gb3     EQU 631
G3      EQU 710
Ab3     EQU 786
A3      EQU 854
Bb3     EQU 923
B3      EQU 986
C4      EQU 1046
Db4     EQU 1102
D4      EQU 1155
Eb4     EQU 1205
E4      EQU 1253
F4      EQU 1297
Gb4     EQU 1339
G4      EQU 1379
Ab4     EQU 1417
A4      EQU 1452
Bb4     EQU 1486
B4      EQU 1517
C5      EQU 1546
Db5     EQU 1575
D5      EQU 1602
Eb5     EQU 1627
E5      EQU 1650
F5      EQU 1673
Gb5     EQU 1694
G5      EQU 1714
Ab5     EQU 1732
A5      EQU 1750
Bb5     EQU 1767
B5      EQU 1783
C6      EQU 1798
Db6     EQU 1812
D6      EQU 1825
Eb6     EQU 1837
E6      EQU 1849
F6      EQU 1860
Gb6     EQU 1871
G6      EQU 1881
Ab6     EQU 1890
A6      EQU 1899
Bb6     EQU 1907
B6      EQU 1915
C7      EQU 1923
Db7     EQU 1930
D7      EQU 1936
Eb7     EQU 1943
E7      EQU 1949
F7      EQU 1954
Gb7     EQU 1959
G7      EQU 1964
Ab7     EQU 1969
A7      EQU 1974
Bb7     EQU 1978
B7      EQU 1982
C8      EQU 1985
Db8     EQU 1988
D8      EQU 1992
Eb8     EQU 1995
E8      EQU 1998
F8      EQU 2001
Gb8     EQU 2004
G8      EQU 2006
Ab8     EQU 2009
A8      EQU 2011
Bb8     EQU 2013
B8      EQU 2015

;Noise Shift Clock
NOISE_DIV_2   EQU $00
NOISE_DIV_4   EQU $10
NOISE_DIV_8   EQU $20
NOISE_DIV_16  EQU $30
NOISE_DIV_32  EQU $40
NOISE_DIV_64  EQU $50
NOISE_DIV_128 EQU $60
NOISE_DIV_256 EQU $70
NOISE_DIV_512 EQU $80
NOISE_DIV_1K  EQU $90
NOISE_DIV_2K  EQU $A0
NOISE_DIV_4K  EQU $B0
NOISE_DIV_8K  EQU $C0
NOISE_DIV_16K EQU $D0

;Periodic Noise (15 counter steps)
NOISE_STUTTER    EQU $0 ;A square plus a pulse at random pulse widths
NOISE_RUMBLE     EQU $1 ;The same waveform but faster
NOISE_ENGINE     EQU $2 ;The same waveform but even faster
NOISE_LOW_TONE   EQU $3 ;Sounds like D5
NOISE_UNDERTONE  EQU $4 ;Sounds like E5 + 50cents
NOISE_MIDDLETONE EQU $5 ;Sounds like B5 + 50cents
NOISE_OVERTONE   EQU $6 ;Sounds like D6 + 50cents
NOISE_HIGH_TONE  EQU $7 ;Sounds like D7

;Pseudorandom Noise (7 counter steps)
NOISE_EARTHQUAKE EQU $8 ;A square with a thin pulse at random pulse widths
NOISE_SPACESHIP  EQU $9 ;The same waveform but faster
NOISE_OCEAN      EQU $A ;The same waveform but even faster
NOISE_SCRATCH    EQU $B ;You get the idea
NOISE_GLITCH     EQU $C ;A fairly clean white-noise sample, unrelated to other instruments
NOISE_VOLCANO    EQU $D ;A pulse with rapidly changing pulse width
NOISE_SCREAM     EQU $E ;The same waveform but faster
NOISE_STATIC     EQU $F ;The same waveform but even faster
