INCLUDE "src/hardware.inc"
INCLUDE "src/memory1.asm"
INCLUDE "src/gbdk.asm"

SECTION "Start", ROMX;, BANK(1)

INCLUDE "img/title/copyrights/copyrights.asm"
INCLUDE "img/title/intro/intro.asm"
INCLUDE "img/title/intro/intro_sprites/intro_sprites.asm"

IF DEF(_HOME)
  INCLUDE "img/home_version/version_sprites/version_sprites.asm"
ELSE
  INCLUDE "img/away_version/version_sprites/version_sprites.asm"
ENDC

LightsPalSeq:
  db $E0, $E0, $E0, $E0, $E8, $E8, $E8, $E8, $E0, $E0
  db $E0, $E0, $E8, $E8, $E8, $E8, $E0, $E0, $E0, $E0
  db $E8, $E8, $E8, $E8, $E0, $E0, $E0, $E0, $E8, $E8
  db $E8, $E8, $EC, $EC, $EC, $EC, $EC, $EC, $EC, $EC

Start::
  xor a
  ld [rVBK], a
  ld [rSTAT], a
.showCopyrights
  call TurnLCDOff

  ld hl, CopyrightsTiles
  ld de, _VRAM+$1000
  ld bc, _COPYRIGHTS_TILE_COUNT*16
  call mem_CopyVRAM
  
  xor a
  ld d, a ; x
  ld e, a ; y
  ld h, _COPYRIGHT_COLUMNS ; w
  ld l, _COPYRIGHT_ROWS ; h
  ld bc, CopyrightTileMap
  call gbdk_SetXYBKG

  ld a, LCDCF_ON | LCDCF_WIN9800 | LCDCF_WINON | LCDCF_BG8800 | LCDCF_OBJ8 | LCDCF_OBJOFF | LCDCF_BGON
  ld [rLCDC], a

  ld de, 1000
  call gbdk_Delay
.showIntroSequence
  call TurnLCDOff
  ld hl, rBGP
  ld [hl], $E0

  ld hl, IntroTiles
  ld de, _VRAM+$1000
  ld bc, _INTRO_TILE_COUNT*16
  call mem_CopyVRAM

  xor a
  ld d, a ; x
  ld e, a ; y
  ld h, _INTRO_LIGHTS_COLUMNS ; w
  ld l, _INTRO_LIGHTS_ROWS ; h
  ld bc, IntroLightsTileMap
  call gbdk_SetXYBKG

  ld a, LCDCF_ON | LCDCF_WIN9800 | LCDCF_WINON | LCDCF_BG8800 | LCDCF_OBJ8 | LCDCF_OBJOFF | LCDCF_BGON
  ld [rLCDC], a

  ; set_sprite_data(0, _INTRO_SPRITES_TILE_COUNT, _intro_sprites_tiles);
  ; set_sprite_data(_INTRO_SPRITES_TILE_COUNT, _VERSION_SPRITES_TILE_COUNT, _version_sprites_tiles);
  ; set_sprite_tile(0, 0);
  ; move_sprite(0, 152,0);

.lightsSequence
  ld de, 1000
  call gbdk_Delay
  ; for (i = 0; i < 60; i++) {
  ;  if (joypad() & (J_START | J_A)) return;
  ;  update_vbl();
  ; }
  ; y = -8;
  ; for (x = 156; x > 94; x-=2) {
  ;  move_sprite(0, x, y+=3);
  ;  if (joypad() & (J_START | J_A)) return;
  ;  update_vbl();
  ; }
  
  ld d, 10 ; x
  ld e, 8  ; y
  ld h, _INTRO_LIGHT_OUT_COLUMNS ; w
  ld l, _INTRO_LIGHT_OUT_ROWS ; h
  ld bc, IntroLightOutTileMap
  call gbdk_SetXYBKG
  
  ; // start playing stars animation
  ; for (x = 0; x < 40; ++x) {
  ;  move_sprite(0, x+94, y+=4);
  ;  BGP_REG = lights_pal_seq[x];
  ;  if (joypad() & (J_START | J_A)) return;
  ;  update_vbl();
  ; }
  ; for (i = 0; i < 60; i++) {
  ;  if (joypad() & (J_START | J_A)) return;
  ;  update_vbl();
  ; }
  ld de, 1000
  call gbdk_Delay
.pitchSequence
  lcd_WaitVRAM

  ld d, 0 ; x
  ld e, 0 ; y
  ld h, _INTRO_PITCH_COLUMNS ; w
  ld l, _INTRO_PITCH_ROWS ; h
  ld bc, IntroPitchTileMap
  call gbdk_SetXYBKG

  ld hl, rBGP
  ld [hl], BG_PALETTE

  ld de, 1000
  call gbdk_Delay

  ;  for (i = 0; i < _INTRO0_COLUMNS*_INTRO0_ROWS; i++) {
  ;    set_sprite_tile(i, _intro0_map[i]+_INTRO_SPRITES_TILE_COUNT);
  ;    set_sprite_prop(i, S_PRIORITY);
  ;  }
  ;  for (k = 0; k <= 128; ++k) {
  ;    if (joypad() & (J_START | J_A)) return;
  ;    move_bkg(k+32, 0);
  ;    a = 0;
  ;    for (j = 0; j < _INTRO0_ROWS; j++) {
  ;     for (i = 0; i < _INTRO0_COLUMNS; i++) {
  ;      move_sprite(a++, k+i*8-32, j*8+80);
  ;     }
  ;    }
  ;    update();
  ;  }
  ;  for (i = 0; i < 60; i++) {
  ;    if (joypad() & (J_START | J_A)) return;
  ;    update_vbl();
  ;  }

  ;  FADE_OUT();
  ret