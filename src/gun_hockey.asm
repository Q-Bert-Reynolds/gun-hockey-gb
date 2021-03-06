SECTION "Gun Hockey", ROM0
INCLUDE "img/bg_tiles.asm"
INCLUDE "img/left_gun.asm"
INCLUDE "img/right_gun.asm"
INCLUDE "img/sprites.asm"

GUN_SPEED      EQU 1
PHYS_POS_STEPS EQU 2
MAX_BULLETS    EQU 36


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

  call UpdateRightGun

.sprites
  ld hl, _SpritesTiles
  ld de, _VRAM8000
  ld bc, _SPRITES_TILE_COUNT*16
  call mem_CopyVRAM

.puck
  call ResetPuck

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
    ; jr nc, .checkFire
    ; ld a, [left_gun_angle+1]
    ; inc a
    ; ld [left_gun_angle+1], a
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
    ; jr nc, .checkFire
    ; ld a, [left_gun_angle+1]
    ; dec a
    ; ld [left_gun_angle+1], a
    call UpdateLeftGun

  .checkFire
    ld a, [last_button_state]
    and a, PADF_A | PADF_B
    jr nz, .updateObjects
    ld a, [button_state]
    and a, PADF_A | PADF_B
    jr z, .updateObjects
  .firePressed
    call FireBulletFromLeftGun

  .updateObjects
  REPT PHYS_POS_STEPS
    call MoveBullets
    call MovePuck
    call MoveBullets
  ENDR
    call CheckCollisions
    call DrawObjects

  .wait
    xor a
    ld [vbl_timer], a
    call gbdk_WaitVBL
    jp .loop
.exit
  ret

ResetPuck::
  xor a
  ld [puck.vy], a
  ld a, 84
  ld [puck.Y], a
  xor a
  ld [puck.y], a
  ld [puck.vx], a
  ld a, 84
  ld [puck.X], a
  xor a
  ld [puck.x], a
  ret

CheckCollisions::
  ld hl, moving_objects+6
  ld c, MAX_BULLETS
.loop
    call Collide
    dec c
    jr nz, .loop
  ret

Collide::;hl = obj, returns next obj in hl
.storeYVelocity
  ld a, [hli]
  ld [_w], a;y vel
.storeYPos
  ld a, [hli]
  ld [_y], a;y pos
  ld b, a
  ld a, [puck.Y]
.skipFractionalYPosition
  inc hl
.yTest
  sub a, b;puck.y - bullet.y
  cp a, 9
  jr c, .storeXVelocity
  cp a, -4
  jr nc, .storeXVelocity
.failedYTest
  inc hl
  inc hl
  inc hl
  ret
.storeXVelocity
  ld a, [hli]
  ld [_v], a;x vel
.storeXPos
  ld a, [hli]
  ld [_x], a;x pos
  ld b, a
  ld a, [puck.X]
.skipFractionalXPosition
  inc hl
.xTest
  sub a, b;puck.y - bullet.y
  cp a, 9
  jr c, .collisionFound
  cp a, -4
  jr nc, .collisionFound
.failedXTest
  ret
.collisionFound
  push hl;next obj
.removeBullet;TODO:remove this section
  dec hl
  xor a
  REPT 6
  ld [hld], a
  ENDR
.calcDPos
  ld a, [_x]
  ld b, a
  ld a, [puck.X]
  sub a, b
  ld d, a;d = dPos.x
  ld a, [_y]
  ld b, a
  ld a, [puck.Y]
  sub a, b
  ld e, a;e = dPos.y, de = dPos
.calcDVel
  ld a, [_v]
  ld b, a
  ld a, [puck.vx]
  sub a, b
  ld b, a;b = dVel.x
  ld a, [_w]
  ld c, a
  ld a, [puck.vy]
  sub a, c
  ld c, a;c = dVel.y, bc = dVel
.calcDVelDotDPos
  push de;dPos
  call math_Dot;hl = dot(dVel, dPos)
  pop de;dPos
.updateXVel
  push de;dPos
  push hl;dot(dVel, dPos)
  ld a, d;x
  ld d, h
  ld e, l
  call math_SignedMultiply;hl = dPos.x * dot(dVel, dPos)
  ld a, [puck.vx]
  sub a, h;puckVel.x -= dPos.x * 0.5 * dot(dVel, dPos)
  ld [puck.vx], a
  ld a, [_v]
  add a, h;bulletVel.x += dPos.x * 2.0 * dot(dVel, dPos)
  ld [_v], a
.updateYVel
  pop de;dot(dVel, dPos)
  pop hl;dPos
  ld a, l;de = y
  call math_SignedMultiply;hl = dPos.y * dot(dVel, dPos)
  ld a, [puck.vy]
  sub a, h;puckVel.y -= dPos.y * 0.5 * dot(dVel, dPos)
  ld [puck.vy], a
  ld a, [_w]
  add a, h;bulletVel.y += dPos.y * 2.0 * dot(dVel, dPos)
  ;TODO: set bullet velo
  pop hl;next obj
  ret


DrawObjects::
  ld hl, moving_objects
  ld de, oam_buffer
  ld b, 40;max sprites
  ld c, 0;TODO: skip unused moving_objects
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
    cp a, c
    jr nz, .setTile
    ld a, 1
  .setTile
    ld [de], a;sprite tile
    inc de
  .setProps
    xor a
    ld [de], a;sprite flags
    inc de

    inc c
    dec b
    jr nz, .loop
  ret

MovePuck::
.testYVelocity
  ld a, [puck.vy]
  cp a, 128
  ld a, [puck.Y]
  jr nc, .checkTop;if vy < 0
.checkBottom;if vy >= 0
  cp a, 146
  jr c, .updateYPosition
  jr .flipVY
.checkTop
  cp a, 18
  jr nc, .updateYPosition
.flipVY
  ld a, [puck.vy]
  cpl
  inc a
  ld [puck.vy], a
.updateYPosition
  ld a, [puck.vy]
  ld hl, puck.Y
  ADD_SIGNED_BYTE_TO_WORD;a + [hl]
.updateXPosition
  ld a, [puck.vx]
  ld hl, puck.X
  ADD_SIGNED_BYTE_TO_WORD
  ld a, [puck.X]
  cp a, 164
  ret c
  jp ResetPuck

MoveBullets::
  ld hl, bullets
  ld c, MAX_BULLETS
.loop
    push bc;bullets left
  .testYVelocity
    ld a, [hli];vy
    ld b, a;vy
    cp a, 128
    ld a, [hl];Y
    jr nc, .checkTop;if vy < 0
  .checkBottom;if vy >= 0
    cp a, 152
    jr c, .updateYPosition
    jr .flipVY
  .checkTop
    cp a, 14
    jr nc, .updateYPosition
  .flipVY
    ld a, b
    cpl
    inc a
    ld b, a
    dec hl
    ld [hli], a
  .updateYPosition
    ld a, b
    ADD_SIGNED_BYTE_TO_WORD;a + [hl]
    inc hl;y
    inc hl;vx
    ld a, [hli];vx
  .updateXPosition
    ADD_SIGNED_BYTE_TO_WORD
    ld a, [hli];X
    cp a, 164
    jr c, .next
  .offScreen
    xor a
    ld [hld], a;x
    ld [hld], a;X
    ld [hld], a;vx
    ld [hld], a;y
    ld [hld], a;Y
    ld [hld], a;vy
    ld de, 6
    add hl, de
  .next
    inc hl;next vy
    pop bc
    dec c;bullets left
    jr nz, .loop
  ret

FireBulletFromLeftGun::
  ;TODO: check ammo first

  ;get vx,vy from angle
  ld a, [left_gun_angle];+1]
  call math_Sin127
  cpl
  inc a;flip y velo
  ld e, a;vy
  ld a, [left_gun_angle];+1]
  call math_Cos127
  ld d, a;vx
  
  ;TODO: find inactive bullet (ie. y|Y = 0)
  ld hl, moving_objects+6;vy,Y,y,vx,X,x
  ld c, MAX_BULLETS
.findBulletLoop
    inc hl;skip vy
    ld a, [hli]
    ld b, a
    ld a, [hli]
    or a, b
    jr z, .foundBullet
    inc hl;skip vx
    inc hl;skip X
    inc hl;skip x
    dec c
    jr nz, .findBulletLoop
  ret;no bullet found

.foundBullet
  dec hl
  dec hl
  dec hl
  ld a, e
  ld [hli], a;vy
  ld a, 82
  ld [hli], a;Y
  xor a
  ld [hli], a;y
  ld a, d
  ld [hli], a;vx
  ld a, 2
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
  ld a, [left_gun_angle];+1];-90 to 90
  add a, 90 ;0 to 180
  ld h, 0
  ld l, a
  ld c, 15
  call math_Divide
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


RightGun:
  DW _RightGun0TileMap
  DW _RightGun1TileMap
  DW _RightGun2TileMap
  DW _RightGun3TileMap
  DW _RightGun4TileMap
  DW _RightGun5TileMap
  DW _RightGun6TileMap
  DW _RightGun7TileMap
  DW _RightGun8TileMap
  DW _RightGun9TileMap
  DW _RightGun10TileMap
  DW _RightGun11TileMap
  DW _RightGun12TileMap

UpdateRightGun::
  ld a, [right_gun_angle];+1];-90 to 90
  add a, 90 ;0 to 180
  ld h, 0
  ld l, a
  ld c, 15
  call math_Divide
  add hl, hl
  ld bc, RightGun
  add hl, bc
  ld a, [hli]
  ld c, a
  ld a, [hli]
  ld b, a
  ld de, $1207
  ld hl, $0204
  ld a, 192
  call SetBkgTilesWithOffset
  ret