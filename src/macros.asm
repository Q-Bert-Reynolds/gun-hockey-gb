SET_LCD_INTERRUPT: MACRO ;interrupt
  di

  ld b, ~IEF_LCDC
  ld a, [rIE]
  and a, b
  ld [rIE], a

  xor a
  ld [rSTAT], a

  ld hl, rLCDInterrupt
  ld bc, \1
  ld a, b
  ld [hli], a
  ld a, c
  ld [hl], a

  ld b, IEF_LCDC
  ld a, [rIE]
  or a, b
  ld [rIE], a

  ld a, STATF_LYC
  ld [rSTAT], a
  
  ei
ENDM

DISABLE_LCD_INTERRUPT: macro
  di
    
  ld b, ~IEF_LCDC
  ld a, [rIE]
  and a, b
  ld [rIE], a

  xor a
  ld [rSTAT], a

  ld hl, rLCDInterrupt
  ld bc, EndLCDInterrupt
  ld a, b
  ld [hli], a
  ld a, c
  ld [hl], a

  ei
ENDM

HIDE_ALL_SPRITES: MACRO
  xor a
  ld b, 160
  ld hl, oam_buffer
.loop\@
  ld [hl], a
  inc l
  dec b
  jr nz, .loop\@
ENDM

CLEAR_TILES: MACRO ;\1 = tile
  ld a, \1
  ld bc, 32*32+20*18
  ld hl, _SCRN0
  call mem_Set
ENDM

CLEAR_SCREEN: MACRO ;\1 = tile
  CLEAR_TILES \1
  ld a, 166
  ld [rWX], a
  ld a, 143
  ld [rWY], a
  HIDE_ALL_SPRITES
ENDM

CLEAR_BKG_AREA: MACRO ;x, y, w, h, tile
  ld de, \3 * \4
  ld hl, tile_buffer
.tilesLoop\@
    ld a, \5
    ld [hli], a
    dec de
    ld a, d
    and a
    jr nz, .tilesLoop\@
    ld a, e
    and a
    jr nz, .tilesLoop\@
  ld d, \1
  ld e, \2
  ld h, \3
  ld l, \4
  ld bc, tile_buffer
  call gbdk_SetBkgTiles
ENDM

CLEAR_WIN_AREA: MACRO ;x, y, w, h, tile
  ld de, \3 * \4
  ld hl, tile_buffer
.tilesLoop\@
    ld a, \5
    ld [hli], a
    dec de
    ld a, d
    and a
    jr nz, .tilesLoop\@
    ld a, e
    and a
    jr nz, .tilesLoop\@
  ld d, \1
  ld e, \2
  ld h, \3
  ld l, \4
  ld bc, tile_buffer
  call gbdk_SetWinTiles
ENDM

FADE_OUT: MACRO
  ld a, $90
  ld [rBGP], a
  ld [rOBP0], a
  ld de, 200
  call gbdk_Delay
  ld a, $40
  ld [rBGP], a
  ld [rOBP0], a
  ld de, 200
  call gbdk_Delay
  xor a
  ld [rBGP], a
  ld [rOBP0], a
  ld de, 200
  call gbdk_Delay
ENDM

FADE_IN: MACRO
  ld a, $40
  ld [rBGP], a
  ld [rOBP0], a
  ld de, 200
  call gbdk_Delay
  ld a, $90
  ld [rBGP], a
  ld [rOBP0], a
  ld de, 200
  call gbdk_Delay
  ld a, BG_PALETTE
  ld [rBGP], a
  ld [rOBP0], a
  ld de, 200
  call gbdk_Delay
ENDM

SET_SPRITE_TILES: MACRO ;count, map, flags, offset
  xor a
  ld [_i], a
  ld hl, \2
  ld e, \3 ;flags
.setSprite\@
  ld a, [_i]
  ld c, a
  ld a, [hli]
  push hl
  add a, \4
  ld d, a ;tile
  call gbdk_SetSpriteTile
  ld a, [_i]
  ld c, a
  ld d, e
  call gbdk_SetSpriteProp
  pop hl
  ld a, [_i]
  inc a
  ld [_i], a
  sub a, \1
  jr nz, .setSprite\@
ENDM

UPDATE_INPUT_AND_JUMP_TO_IF_BUTTONS: MACRO ;address, buttons
  call UpdateInput
  JUMP_TO_IF_BUTTONS \1, \2
ENDM

JUMP_TO_IF_BUTTONS: MACRO ; address, buttons
  ld a, [last_button_state]
  and a
  jr nz, .skip\@
  ld a, [button_state]
  and \2
  jp nz, \1
.skip\@
ENDM

WAITPAD_UP: MACRO
.loop\@
  call gbdk_WaitVBL
  call UpdateInput
  ld a, [button_state]
  and a
  jr nz, .loop\@
ENDM