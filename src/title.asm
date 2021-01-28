INCLUDE "src/beisbol.inc"

SECTION "Title", ROMX, BANK[TITLE_BANK]

INCLUDE "img/title/title/title.asm"
INCLUDE "img/title/title/title_sprites/title_sprites.asm"

IF DEF(_HOME)
INCLUDE "img/home_version/version.asm"
IntroPlayerNums: 
  DB 4, 7, 1, 13, 32, 123, 25, 35, 112, 63, 92, 132, 17, 95, 77, 129
ELIF DEF(_AWAY)
INCLUDE "img/away_version/version.asm"
IntroPlayerNums: 
  DB 7, 4, 1, 56, 106, 37, 113, 142, 135, 143, 44, 60, 84, 137, 94, 26
ELSE
INCLUDE "img/demo_version/version.asm"
IntroPlayerNums: 
  DB 1, 4, 7, 25, 129, 150, 54, 65, 35, 143, 19, 95, 84, 137, 47, 151
ENDC

PLAYER_INDEX EQU _TITLE_TILE_COUNT+_VERSION_TILE_COUNT

ShowTitleLCDInterrupt::
  ld a, [rLY]
  cp 63
  jr c, .dropInTitle
  cp 255
  jr nz, .slideVersion
.dropInTitle
  ld a, 63
  ld [rLYC], a
  xor a
  ld [rSCX], a
  ld a, [_y]
  ld [rSCY], a
  ret
.slideVersion
  ld a, [rLY]
  cp 63
  jr nz, .scrollPlayers
  ld a, 72
  ld [rLYC], a
  ld a, [_x]
  ld [rSCX], a
  xor a
  ld [rSCY], a
  ret
.scrollPlayers
  ld a, [rLY]
  cp 72
  jr nz, .screenBottom
  ld a, 135
  ld [rLYC], a
  ld a, 128
  ld [rSCX], a
  xor a
  ld [rSCY], a
  ret
.screenBottom
  ld a, [rLY]
  cp 135
  ret nz
  xor a
  ld [rLYC], a
  ld [rSCX], a
  ld [rSCY], a
  ret

CyclePlayersLCDInterrupt::
  ld a, [rLY]
  and a
  jr z, .noScroll
  cp 72
  jr nz, .noScroll
  ld a,  135
  ld [rLYC], a
  ld a, [_x]
  ld [rSCX], a
  ret
.noScroll
  ld a, [rLY]
  cp 135
  ret nz
  ld a, 72
  ld [rLYC], a
  xor a
  ld [rSCX], a
  ret

ShowPlayer: ;de = player number
  DISABLE_LCD_INTERRUPT
  ld hl, IntroPlayerNums
  add hl, de
  ld a, [hl]
  push af ;player num
  ld de, PLAYER_INDEX
  call LoadPlayerBkgData; load_player_bkg_data(intro_player_nums[p], PLAYER_INDEX, TITLE_BANK);
  CLEAR_BKG_AREA 20,10,7,7,0

  pop af
  push af
  call GetPlayerImgColumns; a = 7-get_player_img_columns (intro_player_nums[p], TITLE_BANK);

  ld d, a
  ld a, 27
  sub a, d
  ld b, a;x
  ld a, 17
  sub a, d
  ld c, a;y
  pop af ;player num
  push af ;player num
  ld de, PLAYER_INDEX
  call SetPlayerBkgTiles; set_player_bkg_tiles(20+a, 10+a, intro_player_nums[p], PLAYER_INDEX, TITLE_BANK);

  pop af;player num
  call LoadPlayerBaseData
  ld hl, player_base.sgb_pal
  ld a, [hli]
  ld e, a
  ld a, [hli]
  ld d, a
  ld bc, PaletteCalvin
  ld a, [sgb_Pal23]
  call SetPalettesDirect

  ld a, 72
  ld [rLYC], a
  SET_LCD_INTERRUPT CyclePlayersLCDInterrupt
  ret

TitleDrop:
  DB 64,61,58,55,52,49,44,41,38,35,30,25,20,15,10,5,0,6,8,10,11,10,8,6,0,3,4,5,4,3,0,-1

BallToss:
  DB 16,15,15,14,14,13,13,12,12,11,11,10,10,10,9,9,9,8,8,7,7,7,6,6,6,5,5,5,5,4,4,4,4,3,3,3,3,2,2,2,2,2,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,7,7,7,8,8,9,9,9,10,10,10,11,11,12,12,13,13,14,14,15,15

TitlePalSet: PAL_SET PALETTE_TITLE_SCREEN, PALETTE_VERSION, PALETTE_CALVIN, PALETTE_GREY
SGBTitleAttrBlk:
  ATTR_BLK 5
  ATTR_BLK_PACKET %111, 1,0,0,  0, 0, 20,8 ;title & home/away
  ATTR_BLK_PACKET %001, 0,0,3,  0,10, 20,7 ;player
  ATTR_BLK_PACKET %001, 0,0,2, 13,10,  3,4 ;Calvin's head
  ATTR_BLK_PACKET %001, 0,0,2, 11,12,  5,2 ;Calvin's body
  ATTR_BLK_PACKET %001, 0,0,1, 13,14,  2,3 ;Calvin's pants
  
ShowTitle:
  DISPLAY_OFF
  CLEAR_SCREEN 0
.setDMGPalettes
  ld hl, rBGP
  ld [hl], DMG_PAL_BDLW
  ld hl, rOBP0
  ld [hl], DMG_PAL_BDLW
  ld hl, rOBP1
  ld [hl], $E0
.setColors
  ld hl, TitlePalSet
  call SetPalettesIndirect
  ld b, DRAW_FLAGS_BKG
  ld hl, SGBTitleAttrBlk
  call SetColorBlocks
.setGBCColors
  ld a, 3;player
  ld hl, tile_buffer
  ld bc, 7*7
  call mem_Set

  ld h, 7
  ld l, 7
  ld d, 20
  ld e, 10
  ld bc, tile_buffer
  call GBCSetBkgPaletteMap

.playMusic
  PLAY_SONG take_me_out_to_the_ballgame_data, 1

.drawCalvin
  ld hl, _TitleSpritesTiles
  ld de, _VRAM
  ld bc, _TITLE_SPRITES_TILE_COUNT*16
  call mem_CopyVRAM
  call UpdateAudio

  ;set upper body tiles and palette
  ld de, _CalvinTitleTileMap
  ld b, 96
  ld c, 96
  ld h, _CALVIN_TITLE_COLUMNS
  ld l, 4;_CALVIN_TITLE_ROWS
  ld a, SPRITE_FLAGS_SKIP | SPRITE_FLAGS_CLEAR_END
  ld [sprite_flags], a
  ld a, 2;GBC palette
  ld [sprite_props], a
  xor a;skip tile 0
  ld [sprite_skip_id], a
  call SetSpriteTilesXY ;bc = xy in screen space, hl = wh in tiles, de = tilemap, a = offset

  ;set lower body tiles and palette
  ld de, _CalvinTitleTileMap+4*_CALVIN_TITLE_COLUMNS
  ld b, 96
  ld c, 96+8*4
  ld h, _CALVIN_TITLE_COLUMNS
  ld l, _CALVIN_TITLE_ROWS-4
  ld a, SPRITE_FLAGS_SKIP | SPRITE_FLAGS_CLEAR_END
  ld [sprite_flags], a
  ld a, 1;GBC palette
  ld [sprite_props], a
  ld a, 16
  ld [sprite_first_tile], a
  xor a;skip tile 0
  ld [sprite_skip_id], a
  call SetSpriteTilesXY

  call UpdateAudio

.moveBaseball
  ld hl, PaletteBaseball
  ld a, 4
  call GBCSetPalette

  ld c, 5
  ld d, OAMF_PAL1 | 4
  call gbdk_SetSpriteProp
  ld c, 5
  ld d, 94
  ld e, 117
  call gbdk_MoveSprite

.drawTitle
  ld hl, _TitleTiles
  ld de, _VRAM+$1000
  ld bc, _TITLE_TILE_COUNT*16
  call mem_CopyVRAM

  ld hl, _VersionTiles
  ld de, _VRAM+$1000+_TITLE_TILE_COUNT*16
  ld bc, _VERSION_TILE_COUNT*16
  call mem_CopyVRAM
  call UpdateAudio

  xor a
  ld d, a ; x
  ld e, a ; y
  ld h, _BEISBOL_LOGO_COLUMNS ; w
  ld l, _BEISBOL_LOGO_ROWS ; h
  ld bc, _BeisbolLogoTileMap
  call gbdk_SetBkgTiles

.loadFirstPlayer
  xor a
  ld d, a
  ld e, a
  call ShowPlayer

  ld a, 64
  ld [_y], a
  ld [_x], a

  SET_LCD_INTERRUPT ShowTitleLCDInterrupt

  DISPLAY_ON
  call gbdk_WaitVBL

  ld hl, TitleDrop
.dropInTitleLoop
    call gbdk_WaitVBL
    ld a, [hli]
    cp -1
    jr z, .finishTitleDrop
    cp 0
    jr nz, .skipDropSFX
    PLAY_SFX TitleDropInSound
.skipDropSFX
    ld [_y], a
    jr .dropInTitleLoop
.finishTitleDrop

  ld d, 20
  ld e, 8
  ld h, _VERSION_COLUMNS
  ld l, _VERSION_ROWS
  ld a, _TITLE_TILE_COUNT
  ld bc, _VersionTileMap
  call SetBkgTilesWithOffset

  PLAY_SFX VersionSlideInSound

  xor a
  ld [_x], a
.slideInVersionTextLoop
  call gbdk_WaitVBL
  ld a, [_x]
  add a, 3
  ld [_x], a
  cp 104
  jr c, .slideInVersionTextLoop

  DISABLE_LCD_INTERRUPT
  CLEAR_BKG_AREA 20, 8, _VERSION_COLUMNS, _VERSION_ROWS, 0
  ld d, 7
  ld e, 8
  ld h, _VERSION_COLUMNS
  ld l, _VERSION_ROWS
  ld a, _TITLE_TILE_COUNT
  ld bc, _VersionTileMap
  call SetBkgTilesWithOffset

  ; PLAY_SONG take_me_out_to_the_ballgame_data, 1

  ld a, 72
  ld [rLYC], a
  SET_LCD_INTERRUPT CyclePlayersLCDInterrupt

  ld a, 128
  ld [_x], a
  xor a
  ld [_z], a ;current player index
CyclePlayersLoop:
  EXITABLE_DELAY .exitTitleScreen, (PADF_START | PADF_A), 255
  xor a
  ld [_j], a
.movePlayerOffScreenLoop ;for (j = 0; j <= 128; j+=4) {
  ld a, [_j]
  add a, 128
  ld [_x], a
  UPDATE_INPUT_AND_JUMP_TO_IF_BUTTONS .exitTitleScreen, (PADF_START | PADF_A)
  ld a, [_z]
  and a
  jr nz, .skipBallToss
  ld hl, BallToss
  ld a, [_j]
  ld b, 0
  ld c, a
  add hl, bc
  ld a, [hl]
  add a, 103
  ld e, a
  ld d, 94 ;x
  ld c, 5 ;sprite id of ball
  call gbdk_MoveSprite
.skipBallToss
  call gbdk_WaitVBL
  ld a, [_j]
  add a, 4
  ld [_j], a
  sub 128
  jr nz, .movePlayerOffScreenLoop

  ld a, [_z]
  inc a
  ld [_z], a
  cp 16
  jr nz, .skipMod
  xor a
  ld [_z], a
.skipMod
  xor a
  ld d, a
  ld a, [_z]
  ld e, a
  call ShowPlayer

  xor a
  ld [_x], a
.movePlayerOnScreenLoop ;for (j = 0; j <= 128; j+=4) {
  UPDATE_INPUT_AND_JUMP_TO_IF_BUTTONS .exitTitleScreen, (PADF_START | PADF_A)
  call gbdk_WaitVBL
  ld a, [_x]
  add a, 4
  ld [_x], a
  sub 128
  jr nz, .movePlayerOnScreenLoop

  jp CyclePlayersLoop
.exitTitleScreen
  ret

StartMenuPalSet: PAL_SET PALETTE_UI, PALETTE_UI, PALETTE_GREY, PALETTE_GREY
SGBStartMenuAttrBlk:
  ATTR_BLK 1
  ATTR_BLK_PACKET %111, 0,0,0, 0,0, 20,18 ;UI

NewGameOptionMenuText:
  db "NEW GAME\nOPTIONS", 0
NewGameContinueOptionMenuText:
  db "CONTINUE\nNEW GAME\nOPTIONS", 0
ShowStartMenu: ; puts choice in a ... 0 = back, >0 = choice
  DISABLE_LCD_INTERRUPT
  DISPLAY_OFF

.setPalettes
  ld hl, StartMenuPalSet               
  call SetPalettesIndirect
.setSGBColors
  ld hl, SGBStartMenuAttrBlk
  call sgb_PacketTransfer
.setGBCColors
  xor a
  ld hl, tile_buffer
  ld bc, 20*18
  call mem_Set
  ld h, 20
  ld l, 18
  ld de, 0
  ld bc, tile_buffer
  call GBCSetBkgPaletteMap

  call UpdateAudio
  CLEAR_SCREEN " "
  call LoadFontTiles
  DISPLAY_ON

  call CheckSave
  jp z, .noSaveFile

.newGameContinueLoop; while (name_buff[0] > 0) {
  ld a, 3
  ld [_c], a
  xor a
  ld hl, name_buffer
  ld [hl], a
  ld hl, NewGameContinueOptionMenuText
  ld de, str_buffer
  call str_Copy
  ld bc, 0
  ld d, 15
  ld e, 8
  ld a, DRAW_FLAGS_BKG | DRAW_FLAGS_PAD_TOP
  call ShowListMenu ;y = show_list_menu(0,0,15,8,"","CONTINUE\nNEW GAME\nOPTION",TITLE_BANK);
  cp 1 ;if (y == 1) {
  ret nz ;else return y;
  ld [_y], a
  call gbdk_WaitVBL

  ld d, 4
  ld e, 7
  ld a, DRAW_FLAGS_BKG
  call DrawSaveStats

  WAITPAD_UP
.showGameStatsLoop ;while (1) {
    UPDATE_INPUT_AND_JUMP_TO_IF_BUTTONS .returnY, PADF_A ;if (joypad() & J_A) return y;
    JUMP_TO_IF_BUTTONS .backPressed, PADF_B; else if (joypad() & J_B) {
    call gbdk_WaitVBL
    jr .showGameStatsLoop
.backPressed
  CLEAR_BKG_AREA 4,7,16,10," "
  jp .newGameContinueLoop
.returnY
  ld a, [_y]
  ret

.noSaveFile
  ld a, 2
  ld [_c], a
  xor a;title
  ld hl, name_buffer
  ld [hl], a

  ld hl, NewGameOptionMenuText ;text
  ld de, str_buffer
  call str_Copy

  xor a
  ld b, a
  ld c, a ;bc=xy
  ld d, 15 ;width
  ld e, 6 ;height
  ld a, DRAW_FLAGS_BKG | DRAW_FLAGS_PAD_TOP
  call ShowListMenu; return show_list_menu(0,0,15,6,"","NEW GAME\nOPTION",TITLE_BANK);
  ret

Title:: ; puts (c-d-1) in a
  xor a
  ld [rSCX], a

  DISABLE_LCD_INTERRUPT

  xor a
  ld [_d], a
.showTitleAndNewGameMenuLoop ; while (d == 0 || d == c)
  ld a, [_d]
  and a
  jr nz, .checkOptions
  call ShowTitle
  xor a
  ld [list_selection], a
  jr .showStartMenu
.checkOptions
  ld a, [_d]
  ld d, a
  ld a, [_c]
  and a, d
  jp z, .showStartMenu
  call ShowOptions
  xor a
  ld [_d], a
.showStartMenu
  call ShowStartMenu ;puts choice in a
  ld [_d], a;d = show_start_menu();

  ld a, [_d]
  and a ;d==0
  jr z, .showTitleAndNewGameMenuLoop
  ld b, a
  ld a, [_c]
  cp a, b ;d==c
  jr z, .showTitleAndNewGameMenuLoop

  DISABLE_LCD_INTERRUPT

; return c-d-1
  ld a, [_d]
  ld d, a
  ld a, [_c]
  sub a, d
  dec a

  ret
