INCLUDE "src/beisbol.inc"

SECTION "Simulation", ROMX, BANK[SIM_BANK]

INCLUDE "img/field.asm"
INCLUDE "img/simulation.asm"

;Sprite IDs
;0 to 25  - head and body for 9 fielders and 4 baserunners
;26 to 36 - dust particles for running
;37,38,39 - bounce fx, ball, shadow

PHYS_POS_STEPS EQU 5

ShowField:
  DISPLAY_OFF
  ld de, $8800;_VRAM+$1000+_UI_FONT_TILE_COUNT*16
  ld bc, _FIELD_TILE_COUNT*16
  ld hl, _FieldTiles
  call mem_CopyVRAM

  ld hl, $2020
  ld de, 0
  ld bc, _FieldTileMap
  ld a, _UI_FONT_TILE_COUNT
  call SetBKGTilesWithOffset

  ld de, $8000
  ld bc, _SIMULATION_TILE_COUNT*16
  ld hl, _SimulationTiles
  call mem_CopyVRAM

  ld a, 20
  ld [rSCX], a
  ld a, 84
  ld [rSCY], a
  DISPLAY_ON
  ret

CalculateLandingSpot2:
  ld d, 0
  ld a, [ball_vel_z]
  sla a
  ld e, a
  jr nc, .skip
  ld d, 1
.skip;hang time = de

  ld a, PHYS_POS_STEPS
  call math_Multiply
  ld d, h
  ld e, l

  push de;hang time * steps
  ld a, [ball_vel_x]
  call math_Multiply
  ld a, [ball_pos_x]
  ld d, a
  ld a, [ball_pos_x+1]
  ld e, a
  add hl, de;landing spot x

  pop de;hang time * steps
  push hl;landing spot x

  ld a, [ball_vel_y]
  call math_Multiply
  ld a, [ball_pos_y]
  ld d, a
  ld a, [ball_pos_y+1]
  ld e, a
  add hl, de;landing spot y
  pop bc;landing spot x

  ld d, b;x
  ld e, h;y
  ret

;TODO: this should calculated directly instead of iteratively
CalculateLandingSpot:; returns landing xy in de
  ld hl, ball_pos_x
  ld de, tile_buffer
  ld bc, ball_state - ball_pos_x
  call mem_Copy;copy ball pos,vel to tile buffer

.loopZ;until z == 0
    ld a, [tile_buffer+8];ball_vel_z
    cp 128;always apply gravity if going up
    jr nc, .applyGravity
    cp 135;don't fall faster than terminal velocity
    jr nc, .XY
.applyGravity
    sub a, 1
    ld [tile_buffer+8], a;ball_vel_z

.XY
    ld a, PHYS_POS_STEPS
.loopXY
      push af;steps left
      ld a, [tile_buffer+6];ball_vel_x
      ld hl, tile_buffer;ball_pos_x
      call math_AddSignedByteToWord

      ld a, [tile_buffer+7];ball_vel_y
      ld hl, tile_buffer+2;ball_pos_y
      call math_AddSignedByteToWord

      ld a, [tile_buffer+8];ball_vel_z
      ld hl, tile_buffer+4;ball_pos_z
      call math_AddSignedByteToWord
      cp 0
      jr z, .skip
      cp 2
      jr nz, .landed
      ld a, 255
      ld [tile_buffer+4], a;ball_pos_z
.skip
      pop af;steps left
      dec a
      jr nz, .loopXY
    jr .loopZ
.landed
  pop af;discard steps
  ld a, [tile_buffer];ball_pos_x
  ld d, a
  ld a, [tile_buffer+2];ball_pos_y
  ld e, a
  ret

UpdateBall:
  ld a, [ball_vel_z]
  cp 128;always apply gravity if going up
  jr nc, .applyGravity
  cp 135;don't fall faster than terminal velocity
  jr nc, .updatePosition
.applyGravity
  sub a, 1
  ld [ball_vel_z], a

.updatePosition
  ld a, PHYS_POS_STEPS
.updatePositionLoop
    push af;steps left
    ld a, [ball_vel_x]
    ld hl, ball_pos_x
    call math_AddSignedByteToWord

    ld a, [ball_vel_y]
    ld hl, ball_pos_y
    call math_AddSignedByteToWord

    ld a, [ball_vel_z]
    ld hl, ball_pos_z
    call math_AddSignedByteToWord
    cp 0
    jr z, .skip
    cp 2
    jr nz, .bounce
    ld a, 255
    ld [ball_pos_z], a
    jr .skip
.bounce
    ld a, [ball_pos_x]
    ld d, a
    ld a, [ball_pos_y]
    ld e, a
    ld [_breakpoint], a

    xor a
    ld [ball_pos_z], a
    ld [ball_pos_z+1], a
    ld a, [ball_vel_z]
    xor a, $FF
    ld b, a
    and a, %10000000
    srl b
    or a, b
    ld [ball_vel_z], a
    ld a, [ball_vel_x]
    ld b, a
    and a, %10000000
    srl b
    or a, b
    ld [ball_vel_x], a
    ld a, [ball_vel_y]
    ld b, a
    and a, %10000000
    srl b
    or a, b
    ld [ball_vel_y], a
.skip
    pop af;steps left
    dec a
    jr nz, .updatePositionLoop

.slowXToStop
  ld a, [ball_vel_x]
  cp -1
  jr nz, .slowYToStop
  xor a
  ld [ball_vel_x], a

.slowYToStop
  ld a, [ball_vel_y]
  cp -1
  jr nz, .updateCameraY
  xor a
  ld [ball_vel_y], a

.updateCameraY
  ld a, [ball_pos_z]
  srl a
  srl a
  srl a
  ld b, a
  ld a, [ball_pos_y]
  sub a, b
  cp 80
  jr c, .moveToTop
  cp 191
  jr nc, .moveToBottom
  sub a, 80
  ld [rSCY], a
  jr .updateCameraX
.moveToTop
  xor a
  ld [rSCY], a
  jr .updateCameraX
.moveToBottom
  ld a, 112
  ld [rSCY], a

.updateCameraX
  ld a, [ball_pos_x]
  cp 84
  jr c, .moveToLeft
  cp 179
  jr nc, .moveToRight
  sub a, 84
  ld [rSCX], a
  jr .drawBall
.moveToLeft
  xor a
  ld [rSCX], a
  jr .drawBall
.moveToRight
  ld a, 96
  ld [rSCX], a

.drawBall
  ld hl, oam_buffer+38*4;ball and shadow are second to last sprites
  ld a, [rSCY]
  ld b, a
  ld a, [ball_pos_y]
  sub a, b
  ld b, a
  ld a, [ball_pos_z]
  srl a
  ld c, a
  ld a, b
  sub a, c
  ld [hli], a;y
  ld a, [rSCX]
  ld b, a
  ld a, [ball_pos_x]
  sub a, b
  ld [hli], a;x
  ld a, [ball_pos_z]
  swap a
  and %00001111
  ld [hli], a;tile
  xor a
  ld [hli], a;prop

.drawShadow
  ld a, [rSCY]
  ld b, a
  ld a, [ball_pos_y]
  sub a, b
  inc a
  ld [hli], a
  ld a, [rSCX]
  ld b, a
  ld a, [ball_pos_x]
  sub a, b
  inc a
  ld [hli], a
  ld a, 16
  ld [hli], a
  xor a
  ld [hl], a

  ret

;sprites 
UpdateBaseRunners:
  ret 

UpdateFielders:
  call DrawFielders
  ret 

InitFielders:
  ;pitcher
  ld a, 85
  ld [SimulationFielders1.pos_x], a
  ld a, 186
  ld [SimulationFielders1.pos_y], a

  ;catcher
  ld a, 40
  ld [SimulationFielders2.pos_x], a
  ld a, 232
  ld [SimulationFielders2.pos_y], a

  ;first
  ld a, 136
  ld [SimulationFielders3.pos_x], a
  ld a, 216
  ld [SimulationFielders3.pos_y], a

  ;second
  ld a, 158
  ld [SimulationFielders4.pos_x], a
  ld a, 154
  ld [SimulationFielders4.pos_y], a

  ;third
  ld a, 61
  ld [SimulationFielders5.pos_x], a
  ld a, 136
  ld [SimulationFielders5.pos_y], a

  ;short
  ld a, 116
  ld [SimulationFielders6.pos_x], a
  ld a, 112
  ld [SimulationFielders6.pos_y], a

  ;left field
  ld a, 90
  ld [SimulationFielders7.pos_x], a
  ld a, 36
  ld [SimulationFielders7.pos_y], a

  ;center field
  ld a, 196
  ld [SimulationFielders8.pos_x], a
  ld a, 68
  ld [SimulationFielders8.pos_y], a

  ;right field
  ld a, 220
  ld [SimulationFielders9.pos_x], a
  ld a, 192
  ld [SimulationFielders9.pos_y], a

  call DrawFielders

  call CalculateLandingSpot
  push de
  call CalculateLandingSpot2
  pop bc
  ld [_breakpoint], a

.findClosest

  ret

DrawFielders:
  ld de, oam_buffer
  ld hl, SimulationFielders1.pos_y

  ld a, 9
.loop
    push af;players left

    ld a, [rSCY]
    ld b, a
    ld a, [hli];y
    sub a, b
    ld [de], a
    inc de
    inc hl
    ld a, [rSCX]
    ld b, a
    ld a, [hld];x
    sub a, b
    ld [de], a
    inc de
    dec hl
    ld a, $64;body
    ld [de], a;tile
    inc de
    xor a
    ld [de], a;prop
    inc de
    ld a, [rSCY]
    ld b, a
    ld a, [hli];y
    sub a, b
    sub a, 8
    ld [de], a
    inc de
    inc hl
    ld a, [rSCX]
    ld b, a
    ld a, [hli];x
    sub a, b
    ld [de], a
    inc de
    ld bc, SimulationFielders2.anim_state - SimulationFielders1.pos_x
    add hl, bc
    ld a, $33;head
    ld [de], a;tile
    inc de
    xor a
    ld [de], a;prop
    inc de

    pop af;players left
    dec a
    jr nz, .loop
  ret

InitBall:
  ; put ball at home plate
  ld a, 48
  ld [ball_pos_x], a
  ld a, 224
  ld [ball_pos_y], a
  ; ld a, 1
  ; ld [ball_pos_z], a

  xor a
  ld [ball_pos_z], a;starting at z = 0 makes landing calc easier
  ld [ball_pos_x+1], a
  ld [ball_pos_y+1], a
  ld [ball_pos_z+1], a
  ld [ball_vel_x+1], a
  ld [ball_vel_y+1], a
  ld [ball_vel_z+1], a
  
  ;TODO:initial velocity should be calculated from swing_diff and player batting

  ld a, [swing_diff_x]
  add a, -64
  ld [ball_vel_y], a
  ld a, [swing_diff_x]
  add a, 64
  ld [ball_vel_x], a
  ld a, [swing_diff_y]
  srl a
  ld [ball_vel_z], a

  ret
  
RunSimulation::
  HIDE_WIN
  HIDE_ALL_SPRITES
  call ShowField
  call InitBall
  call InitFielders

.loop
    call UpdateBall
    call UpdateBaseRunners
    call UpdateFielders
    call gbdk_WaitVBL

    ld a, [ball_vel_x]
    ld b, a
    ld a, [ball_vel_y]
    or a, b
    ld b, a
    ld a, [ball_vel_z]
    or a, b
    ld b, a
    ld a, [ball_pos_z]
    or a, b
    jr nz, .loop

    ; jr .loop

  DISPLAY_OFF
  xor a
  ld [rSCX], a
  ld [rSCY], a
  CLEAR_SCREEN " "
  SHOW_WIN
  DISPLAY_ON
  ret