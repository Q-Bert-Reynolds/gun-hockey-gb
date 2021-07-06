;DMG
DMG_PAL_BDLW EQU %11100100 ;normal
DMG_PAL_WLDB EQU %00011011 ;inverted
DMG_PAL_BDLL EQU %11100101 ;good for testing palettes
DMG_PAL_DLWW EQU %10010000 ;dark,  light, white, white/transparent
DMG_PAL_BDWW EQU %11100000 ;black, dark,  white, white/transparent
DMG_PAL_BLWW EQU %11010000 ;black, light, white, white/transparent
DMG_PAL_BLBW EQU %11011100 ;black, light, black, white/transparent
DMG_PAL_WLBW EQU %00011100 ;white, light, black, white/transparent
DMG_PAL_NORMAL EQU DMG_PAL_BDLW
DMG_PAL_INVERT EQU DMG_PAL_WLDB

;CGB/SGB
WHITE_R EQU 29
WHITE_G EQU 29
WHITE_B EQU 27
COLOR_WHITE       : MACRO RGB 31, 31, 31 ENDM
COLOR_ALMOST_WHITE: MACRO RGB 29, 29, 27 ENDM
COLOR_LIGHT_GREY  : MACRO RGB 23, 23, 23 ENDM
COLOR_DARK_GREY   : MACRO RGB 15, 15, 15 ENDM
COLOR_DARKER_GREY : MACRO RGB  8,  8,  8 ENDM
COLOR_ALMOST_BLACK: MACRO RGB  6,  6,  6 ENDM
COLOR_BLACK       : MACRO RGB  0,  0,  0 ENDM

COLOR_DEEP_PURPLE : MACRO RGB  0,  2,  5 ENDM
COLOR_PURPLE_GREY : MACRO RGB 10, 10, 12 ENDM

COLOR_TAN         : MACRO RGB 25, 21, 18 ENDM
COLOR_DIRT        : MACRO RGB 23, 16, 12 ENDM
COLOR_BROWN       : MACRO RGB 15, 12,  7 ENDM
COLOR_DARK_BROWN  : MACRO RGB  3,  2,  0 ENDM

COLOR_PINK        : MACRO RGB 30, 25, 26 ENDM
COLOR_RED         : MACRO RGB 25, 11, 15 ENDM
COLOR_SALMON      : MACRO RGB 25,  8, 10 ENDM
COLOR_DARK_RED    : MACRO RGB 13,  3,  8 ENDM

COLOR_YELLOW      : MACRO RGB 29, 27, 17 ENDM
COLOR_WARNING     : MACRO RGB 29, 28, 10 ENDM
COLOR_PEACH       : MACRO RGB 28, 24, 17 ENDM

COLOR_LIGHT_GREEN : MACRO RGB 16, 23, 13 ENDM
COLOR_OLIVE       : MACRO RGB 18, 19, 12 ENDM
COLOR_GREEN       : MACRO RGB  5, 16,  8 ENDM
COLOR_GRASS       : MACRO RGB  6, 13, 10 ENDM

COLOR_LIGHT_BLUE  : MACRO RGB 26, 27, 29 ENDM
COLOR_BLUE        : MACRO RGB 11, 15, 25 ENDM
COLOR_BLUE_GREY   : MACRO RGB 14, 16, 18 ENDM
COLOR_DARK_BLUE   : MACRO RGB  5,  8, 16 ENDM

DefaultPalettes::
PaletteGrey::
ColorWhite:: COLOR_ALMOST_WHITE
ColorLight:: COLOR_LIGHT_GREY
ColorDark::  COLOR_DARK_GREY
ColorBlack:: COLOR_BLACK
PALETTE_GREY EQU (PaletteGrey-DefaultPalettes)/8

PaletteSepia::
  COLOR_ALMOST_WHITE
  COLOR_TAN
  COLOR_BROWN
  COLOR_DARK_BROWN
PALETTE_SEPIA EQU (PaletteSepia-DefaultPalettes)/8

PaletteUI::
  COLOR_ALMOST_WHITE
  COLOR_TAN    
  COLOR_RED      
  COLOR_BLACK 
PALETTE_UI EQU (PaletteUI-DefaultPalettes)/8

PaletteGunHockey::
  COLOR_BLACK 
  COLOR_BLUE_GREY
  COLOR_DARK_RED
  COLOR_ALMOST_WHITE
PALETTE_GUN_HOCKEY EQU (PaletteGunHockey-DefaultPalettes)/8

PaletteDark::
  COLOR_ALMOST_WHITE
  COLOR_DARKER_GREY
  COLOR_ALMOST_BLACK
  COLOR_BLACK 
PALETTE_DARK EQU (PaletteDark-DefaultPalettes)/8

PaletteWarning::
  COLOR_ALMOST_WHITE
  COLOR_TAN    
  COLOR_WARNING      
  COLOR_BLACK 
PALETTE_WARNING EQU (PaletteWarning-DefaultPalettes)/8

PaletteGood::
  COLOR_ALMOST_WHITE
  COLOR_TAN    
  COLOR_GREEN      
  COLOR_BLACK 
PALETTE_GOOD EQU (PaletteGood-DefaultPalettes)/8
