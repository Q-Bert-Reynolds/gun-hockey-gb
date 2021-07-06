SECTION "Gun Hockey", ROM0
INCLUDE "img/bg_tiles.asm"
INCLUDE "img/left_gun.asm"
INCLUDE "img/right_gun.asm"

GUN_SPEED EQU 180

BackgroundTiles:
  DB $4,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$5
  DB $1,$3,$3,$3,$3,$3,$3,$A,$A,$A,$D,$D,$D,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $1,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$2
  DB $7,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$8

GunHockey::
  di
  DISPLAY_OFF
  ld a, DMG_PAL_INVERT
  ld [rBGP], a

  ld hl, _BgTilesTiles
  ld de, _VRAM9000
  ld bc, _BG_TILES_TILE_COUNT*16
  call mem_CopyVRAM

  ld bc, BackgroundTiles
  ld de, 0
  ld hl, $1412
  call gbdk_SetBkgTiles

.leftGun
  ld hl, _LeftGunTiles
  ld de, _VRAM8800
  ld bc, _LEFT_GUN_TILE_COUNT*16
  call mem_CopyVRAM

  call UpdateLeftGun

.rightGun
  ld hl, _RightGunTiles
  ld de, _VRAM8800+64*16
  ld bc, _RIGHT_GUN_TILE_COUNT*16
  call mem_CopyVRAM

  ld bc, _RightGun6TileMap
  ld de, $1207
  ld hl, $0204
  ld a, 192
  call SetBkgTilesWithOffset

  xor a
  ld hl, PaletteGunHockey
  call GBCSetPalette

  PLAY_SONG tessie_data, 1

  DISPLAY_ON
  ei

.loop
    call UpdateInput

    ld a, [button_state]
    and a, PADF_UP
    jr nz, .upPressed

    ld a, [button_state]
    and a, PADF_DOWN
    jr nz, .downPressed

    jr .checkExit

  .upPressed
    ld a, [left_gun_angle]
    add a, GUN_SPEED
    ld [left_gun_angle], a
    jr nc, .wait
    ld a, [left_gun_angle+1]
    inc a
    ld [left_gun_angle+1], a
    call UpdateLeftGun
    jr .wait

  .downPressed
    ld a, [left_gun_angle]
    sub a, GUN_SPEED
    ld [left_gun_angle], a
    jr nc, .wait
    ld a, [left_gun_angle+1]
    dec a
    ld [left_gun_angle+1], a
    call UpdateLeftGun
    jr .wait

  .checkExit
    JUMP_TO_IF_BUTTONS .exit, PADF_A | PADF_START

  .wait
    call gbdk_WaitVBL
    jr .loop
.exit
  ret


LeftGun:
  DW _LeftGun0TileMap
  DW _LeftGun1TileMap
  DW _LeftGun2TileMap
  DW _LeftGun3TileMap
  DW _LeftGun4TileMap
  DW _LeftGun5TileMap
  DW _LeftGun6TileMap
  DW _LeftGun7TileMap
  DW _LeftGun8TileMap
  DW _LeftGun9TileMap
  DW _LeftGun10TileMap
  DW _LeftGun11TileMap
  DW _LeftGun12TileMap

UpdateLeftGun::
  ld a, [left_gun_angle+1];-90 to 90
  add a, 90 ;0 to 180
  ld h, 0
  ld l, a
  ld c, 15
  call math_Divide
  ld [_breakpoint], a
  add hl, hl
  ld bc, LeftGun
  add hl, bc
  ld a, [hli]
  ld c, a
  ld a, [hli]
  ld b, a
  ld de, $0007
  ld hl, $0204
  ld a, 128
  call SetBkgTilesWithOffset
  ret