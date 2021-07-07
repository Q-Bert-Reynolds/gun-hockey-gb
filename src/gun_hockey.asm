SECTION "Gun Hockey", ROM0
INCLUDE "img/bg_tiles.asm"
INCLUDE "img/left_gun.asm"
INCLUDE "img/right_gun.asm"
INCLUDE "img/sprites.asm"

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
  call SetupGunHockey
  call GunHockeyGameLoop
  ret 

SetupGunHockey::
  di
  DISPLAY_OFF

.backgroundTiles
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

.sprites
  ld hl, _SpritesTiles
  ld de, _VRAM8000
  ld bc, _SPRITES_TILE_COUNT*16
  call mem_CopyVRAM

.palettes
  ld a, DMG_PAL_INVERT
  ld [rBGP], a

  ld a, DMG_PAL_DLWW
  ld [rOBP0], a
  ld [rOBP1], a

  xor a
  ld hl, PaletteGunHockey
  call GBCSetBackgroundPalette

  xor a
  ld hl, PaletteGunHockeySprites
  call GBCSetSpritePalette

.music
  PLAY_SONG tessie_data, 1

  DISPLAY_ON
  ei
  ret

GunHockeyGameLoop::
.loop
    call UpdateInput
    ;TODO: exchange inputs with player 2

  .checkUp
    ld a, [button_state]
    and a, PADF_UP
    jr z, .checkDown
  .upPressed
    ld a, [left_gun_angle]
    add a, GUN_SPEED
    ld [left_gun_angle], a
    jr nc, .checkFire
    ld a, [left_gun_angle+1]
    inc a
    ld [left_gun_angle+1], a
    call UpdateLeftGun
    jr .checkFire

  .checkDown
    ld a, [button_state]
    and a, PADF_DOWN
    jr z, .checkFire
  .downPressed
    ld a, [left_gun_angle]
    sub a, GUN_SPEED
    ld [left_gun_angle], a
    jr nc, .checkFire
    ld a, [left_gun_angle+1]
    dec a
    ld [left_gun_angle+1], a
    call UpdateLeftGun

  .checkFire
    ld a, [button_state]
    and a, PADF_A | PADF_B
    jr z, .updateBullets
  .firePressed
    call FireBulletFromLeftGun

  .updateBullets
    call UpdateBullets
    call DrawBullets

  .wait
    call gbdk_WaitVBL
    jr .loop
.exit
  ret

DrawBullets::
  ld hl, bullets
  ld de, oam_buffer
  ld b, 40;max sprites
  ld c, MAX_BULLETS;TODO: skip unused bullets
.loop
    inc hl;skip vy
    ld a, [hli];Y
    ld [de], a;sprite y
    inc de
    inc hl;skip y

    inc hl;skip vx
    ld a, [hli];X
    ld [de], a;sprite x
    inc de
    inc hl;skip x

    xor a
    ld [de], a;sprite tile
    inc de

    ld [de], a;sprite flags
    inc de

    dec b
    jr nz, .loop
  ret

UpdateBullets::
  ld hl, bullets
  ld c, MAX_BULLETS
.loop
    push bc;bullets left
    ld a, [hli];vy
    push hl;Y
    call math_AddSignedByteToWord
    pop hl;Y
    inc hl;y
    inc hl;vx
    ld a, [hli];vx
    push hl;X
    call math_AddSignedByteToWord
    pop hl;X
    inc hl;x
    inc hl;next vy
    pop bc
    dec c;bullets left
    jr nz, .loop
  ret

FireBulletFromLeftGun::
  ;TODO: check ammo first
  ;TODO: find inactive bullet (ie. x|X = 0)
  ;get vx,vy from angle
  ld a, [left_gun_angle+1]
  call math_Sin127
  cpl
  inc a;flip y velo
  ld e, a;vy
  ld a, [left_gun_angle+1]
  call math_Cos127
  ld d, a;vx
  
  ld hl, bullets;vy,Y,y,vx,X,x
  ld a, e
  ld [hli], a;vy
  ld a, 70
  ld [hli], a;Y
  xor a
  ld [hli], a;y
  ld a, d
  ld [hli], a;vx
  ld a, 4
  ld [hli], a;X
  xor a
  ld [hli], a;x
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