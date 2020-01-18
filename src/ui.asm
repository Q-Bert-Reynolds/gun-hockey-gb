INCLUDE "src/beisbol.inc"

SECTION "UI", ROMX, BANK[UI_BANK]
INCLUDE "img/ui_font.asm"

;UILoadFontTiles
;UIRevealText - hl = text
;UIShowOptions
;UIShowTextEntry - de = title, hl = str, c = max_len
;UIShowListMenu - returns a, bc = xy, de = wh, text = [str_buffer], title = [name_buff]

UILoadFontTiles::
  ld hl, _UiFontTiles
  ld de, _VRAM+$1000
  ld bc, _UI_FONT_TILE_COUNT*16
  call mem_CopyVRAM ;doesn't loop so mem_CopyToTileData is unnecessary
  ret

FlashNextArrow: ;de = xy
  push de;xy
  ld hl, tile_buffer
  ld a, ARROW_DOWN
  ld [hl], a ;tile_buffer[0] = ARROW_DOWN;
  ld b, h
  ld c, l
  ld a, 1
  ld h, a ;w=1
  ld l, a ;h=1
  call gbdk_SetWinTiles ;set_win_tiles(x, y, 1, 1, tile_buffer);
  WAITPAD_UP
  ld a, 20
  pop de;xy
.loop1 ;for (a = 20; a > 0; --a) {
  ld [_a], a
  JUMP_TO_IF_BUTTONS .exitFlashNextArrow, PADF_A
  push de;xy
  ld de, 10
  call gbdk_Delay
  pop de ;restore xy
  ld a, [_a]
  dec a
  jp nz, .loop1
  ld hl, tile_buffer

  xor a
  ld [hl], a ;tile_buffer[0] = 0;
  ld b, h
  ld c, l
  ld a, 1
  ld h, a ;w=1
  ld l, a ;h=1
  push de ;xy
  call gbdk_SetWinTiles ;set_win_tiles(x, y, 1, 1, tile_buffer);

  pop de ;restore xy
  ld a, 20
.loop2 ;for (a = 20; a > 0; --a) {
  ld [_a], a
  JUMP_TO_IF_BUTTONS .exitFlashNextArrow, PADF_A
  push de;xy
  ld de, 10
  call gbdk_Delay
  pop de ;restore de
  ld a, [_a]
  dec a
  jp nz, .loop2
  jp FlashNextArrow
.exitFlashNextArrow
  ret

UIRevealText::
  push hl;text

  xor a
  ld b, a
  ld c, a
  ld a, 20
  ld d, a
  ld a, 6
  ld e, a
  call DrawWinUIBox; bc = xy, de = wh; draw_win_ui_box(0,0,20,6);

  ld a, 7
  ld [rWX], a
  ld a, 96
  ld [rWY], a; move_win(7,96);
  SHOW_WIN
  
  xor a
  ld [_i], a
  ld [_x], a
  ld [_y], a
  ld [_w], a
  pop hl ;text
  push hl
  call str_Length ;de = length
  ld a, e ;assumes length < 256
  ld [_l], a; l = strlen(text);
.revealTextLoop; for (i = 0; i < l; ++i) {
    pop hl;text
    push hl
.testNewLine;   if (text[i] == '\n') {
    xor a
    ld b, a
    ld a, [_i]
    ld c, a
    add hl, bc;text[i]
    ld a, [hl]
    cp "\n"
    jr nz, .drawCharacter

      ld a, [_y]
      inc a
      ld [_y], a
      sub a, 2
      jr nz, .skipFlash ;if (y == 2) {
        ld d, 18
        ld e, 4
        call FlashNextArrow ;flash_next_arrow(18,4);

        ld a, 1
        ld [_y], a

        pop hl;text
        push hl
        xor a
        ld b, a
        ld a, [_w]
        ld c, a
        add hl, bc;text+w
        ld de, str_buffer
        ld a, [_i]
        sub a, c
        ld c, a;i-w
        call mem_Copy ;memcpy(str_buff,text+w,i-w);

        ld a, [_x]
        and a
        jr z, .skipWhiteSpace
        ld bc, 17
        ld hl, str_buffer
.whiteSpaceLoop
          dec bc
          inc hl
          dec a
          jr nz, .whiteSpaceLoop
        ld a, " "
        call mem_Set
.skipWhiteSpace

        ld d, 1 ;x
        ld e, 2 ;y
        ld h, 17 ;w
        ld l, 1 ;h
        ld bc, str_buffer
        call gbdk_SetWinTiles ;set_win_tiles(1, 2, 17, 1, str_buff);

        ld bc, 17
        ld hl, str_buffer
        ld a, " "
        call mem_Set
        ld d, 1 ;x
        ld e, 4 ;y
        ld h, 17 ;w
        ld l, 1 ;h
        ld bc, str_buffer
        call gbdk_SetWinTiles ;set_win_tiles(1, 4, 17, 1, "                 ");

.skipFlash
      xor a
      ld [_x], a
      ld a, [_i]
      inc a
      ld [_w], a
      jr .delay
.drawCharacter ;else {
    pop hl; text
    push hl
    xor a
    ld b, a
    ld a, [_i]
    ld c, a
    add hl, bc
    ld b, h
    ld c, l;bc = text+i
    ld a, [_x]
    inc a
    ld [_x], a
    ld d, a ;_x+1
    ld a, [_y]
    add a, a;_y*2
    add a, 2;_y*2+2
    ld e, a ;y=_y*2+2
    ld h, 1 ;w
    ld l, 1 ;h
    call gbdk_SetWinTiles;set_win_tiles(x+1,y*2+2,1,1,text+i);

.delay
    ld de, 10;TODO: should use text speed
    call gbdk_Delay

    ld a, [_i]
    inc a
    ld [_i], a
    ld b, a
    ld a, [_l]
    sub b
    jp nz, .revealTextLoop

  ld d, 18
  ld e, 4
  call FlashNextArrow ;flash_next_arrow(18,4);
  pop hl ;text
  ret

MoveOptionsArrow: ; e = y
; tiles[0] = 0;
; tiles[1] = ARROW_RIGHT;
; tiles[2] = ARROW_RIGHT_BLANK;
; set_bkg_tiles(1,3,1,1,tiles + (a==0 ? 2 : 0) - (y==0 ? 1 : 0));
; set_bkg_tiles(7,3,1,1,tiles + (a==1 ? 2 : 0) - (y==0 ? 1 : 0));
; set_bkg_tiles(14,3,1,1,tiles + (a==2 ? 2 : 0) - (y==0 ? 1 : 0));
; set_bkg_tiles(1,8,1,1,tiles + (b==0 ? 2 : 0) - (y==1 ? 1 : 0));
; set_bkg_tiles(10,8,1,1,tiles + (b==1 ? 2 : 0) - (y==1 ? 1 : 0));
; set_bkg_tiles(1,13,1,1,tiles + (c==0 ? 2 : 0) - (y==2 ? 1 : 0));
; set_bkg_tiles(10,13,1,1,tiles + (c==1 ? 2 : 0) - (y==2 ? 1 : 0));
; set_bkg_tiles(1,16,1,1,tiles + (y==3 ? 1 : 2));
  ret

UIShowOptions::
  DISPLAY_OFF
  di
  ENABLE_RAM_MBC5
  ld a, [text_speed]
  ld [_a], a
  ld a, [animation_style]
  ld [_b], a
  ld a, [coaching_style]
  ld [_c], a
  DISABLE_RAM_MBC5
  ei

; if (a > 2) a = 0;
; if (b > 1) b = 0;
; if (c > 1) c = 0;

  xor a
  ld b, a
  ld c, a
  ld a, 20
  ld d, a
  ld a, 5
  ld e, a
  call DrawBKGUIBox; bc = xy, de = wh ; draw_bkg_ui_box(0,0,20,5);
; set_bkg_tiles(1,1,18,3,
;   "TEXT SPEED        "
;   "                  "
;   " FAST  MEDIUM SLOW"

  xor a
  ld b, a
  ld a, 5
  ld c, a
  ld e, a
  ld a, 20
  ld d, a
  call DrawBKGUIBox; bc = xy, de = wh ; draw_bkg_ui_box(0,5,20,5);
; set_bkg_tiles(1,6,18,3,
;   "AT-BAT ANIMATIONS "
;   "                  "
;   " ON       OFF     "

  xor a
  ld b, a
  ld a, 10
  ld c, a
  ld a, 20
  ld d, a
  ld a, 5
  ld e, a
  call DrawBKGUIBox; bc = xy, de = wh ; draw_bkg_ui_box(0,10,20,5);
; set_bkg_tiles(1,11,18,3,
;   "COACHING STYLE    "
;   "                  "
;   " SHIFT    SET     "


; set_bkg_tiles(2,16,6,1,
;   "CANCEL"

  DISPLAY_ON
  WAITPAD_UP

  xor a
  ld [_y], a; y = 0;
  call MoveOptionsArrow; move_options_arrow(y);

.moveOptionsArrowLoop; while (1) {
  call UpdateInput;   k = joypad();
.checkUpPressed;   if (button_state & PADF_UP && y > 0) {
  ld a, [button_state]
  and a, PADF_UP
  jr z, .checkDownPressed
  ld a, [_y]
  and a
  jp z, .checkDownPressed
  call gbdk_WaitVBL
  ld a, [_y]
  dec a
  ld [_y], a
  call MoveOptionsArrow;     move_options_arrow(--y);
  WAITPAD_UP
  jr .waitVBLAndLoop
.checkDownPressed;   else if (button_state & PADF_DOWN && y < 3) {
  ld a, [button_state]
  and a, PADF_DOWN
  jr z, .checkLeftPressed
  ld a, 3
  ld b, a
  ld a, [_y]
  cp b
  jr nc, .checkLeftPressed
  call gbdk_WaitVBL
  ld a, [_y]
  inc a
  ld [_y], a
  call MoveOptionsArrow;     move_options_arrow(++y);
  WAITPAD_UP
  jr .waitVBLAndLoop
.checkLeftPressed;   else if (button_state & PADF_LEFT && y < 3) {
  call gbdk_WaitVBL
;     if (y == 0 && a > 0) --a;
;     else if (y == 1 && b > 0) --b;
;     else if (y == 2 && c > 0) --c;
  call MoveOptionsArrow;     move_options_arrow(y);
  WAITPAD_UP
  jr .waitVBLAndLoop
.checkRightPressed;   else if (button_state & PADF_RIGHT && y < 3) {
  call gbdk_WaitVBL
;     if (y == 0 && a < 2) ++a;
;     else if (y == 1 && b < 1) ++b;
;     else if (y == 2 && c < 1) ++c;
  call MoveOptionsArrow;     move_options_arrow(y);
  WAITPAD_UP
  jr .waitVBLAndLoop
.checkStartAPressed;   if (button_state & (PADF_START | PADF_A) && y == 3) break;
  ld a, [button_state]
  and a, PADF_START | PADF_A
  jr z, .exitMoveOptionsArrowLoop
  jr .waitVBLAndLoop
.checkBPressed;   else if (button_state & PADF_B) break;
  ld a, [button_state]
  and a, PADF_B
  jr z, .exitMoveOptionsArrowLoop
.waitVBLAndLoop
  call gbdk_WaitVBL
  jp .moveOptionsArrowLoop
.exitMoveOptionsArrowLoop

  di
  ENABLE_RAM_MBC5;
  ld a, [_a]
  ld [text_speed], a
  ld a, [_b]
  ld [animation_style], a
  ld a, [_c]
  ld [coaching_style], a
  DISABLE_RAM_MBC5
  ei
  ret

MoveTextEntryArrow: ; bc = from xy, de = to xy
  push bc ;from xy
  push de ;to xy
  call gbdk_WaitVBL
  ld hl, tile_buffer
  xor a
  ld [hl], a; tiles[0] = 0;
  ld a, c
  cp 5; if (from_y == 5) {
  jr nz, .notFromLineFive
  ld e, 15
  ld a, 1
  ld d, a
  ld h, a
  ld l, a
  ld bc, tile_buffer
  call gbdk_SetWinTiles ;set_win_tiles(1,15,1,1,tile_buffer);
  jr .setArrow
.notFromLineFive; else {
  ld a, b ;from_x
  add a, a ;from_x*2
  inc a ;from_x*2+1
  ld d, a
  ld a, c ;from_y
  add a, a ;from_y*2
  add a, 5 ;from_y*2+5
  ld e, a
  ld a, 1
  ld h, a
  ld l, a
  ld bc, tile_buffer
  call gbdk_SetWinTiles ;set_win_tiles(from_x*2+1,from_y*2+5,1,1,tile_buffer);
.setArrow
  pop de ;to xy
  pop bc ;from xy  
  ld hl, tile_buffer
  ld a, ARROW_RIGHT
  ld [hl], a; tiles[0] = ARROW_RIGHT;
  ld a, e
  cp 5; if (to_y == 5) {
  jr nz, .notToLineFive
  push bc ;from xy
  push de ;to xy
  ld e, 15
  ld a, 1
  ld d, a
  ld h, a
  ld l, a
  ld bc, tile_buffer
  call gbdk_SetWinTiles ;set_win_tiles(1,15,1,1,tile_buffer);
  pop de ;to xy
  pop bc ;from xy
  jr .waitPadUp
.notToLineFive; else {
  ld a, d ;to_x
  add a, a ;to_x*2
  inc a ;to_x*2+1
  ld d, a
  ld a, e ;to_y
  add a, a ;to_y*2
  add a, 5 ;to_y*2+5
  ld e, a
  ld a, 1
  ld h, a
  ld l, a
  ld bc, tile_buffer
  call gbdk_SetWinTiles ;set_win_tiles(to_x*2+1,to_y*2+5,1,1,tile_buffer);
.waitPadUp
  WAITPAD_UP; update_waitpadup();
  ret

UpdateTextEntryDisplay: ; hl = str, d = max_len
  push de; d = max_len
  push hl; str

  ld d, 10;x
  ld e, 2;y
  pop bc ;str
  pop hl; h = max_len = width
  push hl
  push bc ;str
  ld l, 1; l = height
  call gbdk_SetWinTiles; set_win_tiles(10,2,max_len,1,str);

  pop bc ;str
  pop de ;d =max_len
  push de
  push bc ;str
  ld c, d ;c = max_len
  xor a
  ld b, a
  ld a, "-"
  ld hl, tile_buffer
  call mem_Set

  pop hl ;str
  push hl
  call str_Length; w = strlen(str);
  ld hl, tile_buffer
  add hl, de
  ld a, "^"
  ld [hl], a

  ld d, 10;x
  ld e, 3;y
  pop bc ;str
  pop hl; h = max_len = width
  ld bc, tile_buffer
  ld l, 1; l = height
  call gbdk_SetWinTiles; set_win_tiles(10,2,max_len,1,str);

  ret

LowerCase:
  db "abcdefghijklmnopqrstuvwxyz *():;[]#%-?!*+/.,↵", 0
LowerCaseTitle:
  db "lower case", 0
UpperCase:
  db "ABCDEFGHIJKLMNOPQRSTUVWXYZ *():;[]#%-?!*+/.,↵", 0
UpperCaseTitle:
  db "UPPER CASE", 0

UIShowTextEntry:: ; de = title, hl = str, c = max_len
  push bc;c = max_len
  push hl;str
  push de;title
  DISPLAY_OFF

  xor a
  ld b, a;b = 0, c = max_len
  call mem_Set; for (i = 0; i != max_len; ++i) str[i] = 0;
  CLEAR_WIN_AREA 0,0,20,4," "
  ld a, 7
  ld [rWX], a
  xor a
  ld [rWY], a; move_win(7,0);

  
  pop hl;title
  push hl
  call str_Length; l = strlen(title);
  ld a, e ;assumes len < 256
  ld [_l], a
  and a
  jp z, .skipTiles; if (l > 0) 
  pop bc;title
  push bc
  xor a
  ld d, a
  ld a, 1
  ld e, a
  ld l, a
  ld a, [_l]
  ld h, a
  call gbdk_SetWinTiles;set_win_tiles(0,1,l,1,title);
.skipTiles
  pop bc; title
  pop hl; str
  pop de; e = max_len
  push de
  push hl ;str
  ld d, e; d = max_len
  call UpdateTextEntryDisplay; update_text_entry_display(str, max_len);
  xor a
  ld b, a
  ld a, 4
  ld c, a
  ld a, 20
  ld d, a
  ld a, 11
  ld e, a
  call DrawWinUIBox; draw_win_ui_box(0,4,20,11);
  DISPLAY_ON
  pop hl ;str
  pop de; e = max_len
  push de; e = max_len
  push hl; str

  xor a
  ld [_x], a
  ld [_y], a
  ld [_c], a
  ld [_l], a
.drawTextBoxLoop; while (1) {
    ld de, str_buffer
    ld bc, 46
    ld a, [_c]
    and a
    jp nz, .shouldUseUpper
.shouldUseLower;   if (c == 0) {
    ld hl, UpperCase
    call mem_Copy;     memcpy(str_buff, upper_case, 46);
    ld bc, LowerCaseTitle;set_win_tiles(2,15,10,1,"lower case");
    jr .setCaseTiles
.shouldUseUpper;   else {
    ld hl, LowerCase
    call mem_Copy;     memcpy(str_buff, lower_case, 46);
    ld bc, UpperCaseTitle;set_win_tiles(2,15,10,1,"UPPER CASE");
.setCaseTiles
    ld d, 2
    ld e, 15
    ld h, 10
    ld l, 1
    call gbdk_SetWinTiles
    xor a
    ld [_j], a
.rowLoop;   for (j = 0; j < 5; ++j) {
      xor a
      ld [_i], a
      ld a, [_j]
      add a, a; j*2
      ld de, 18
      call Multiply; hl = 18 * j*2
      ld b, h
      ld c, l ;bc = j*2*18
      ld hl, tile_buffer
      add hl, bc ;tiles[j*2*18]
      push hl
      ld hl, str_buffer
      ld a, [_j]
      add a, a ;_j*2
      add a, a ;_j*4
      add a, a ;_j*8
      ld c, a
      ld a, [_j]
      add a, c ;_j*9
      ld c, a
      add hl, bc ;str_buff[j*9]
      ld d, h
      ld e, l
      pop hl ;tiles[j*2*18]
.collumnLoop1;     for (i = 0; i < 9; ++i) {
        ld a, [_x]
        ld b, a
        ld a, [_i]
        sub a, b
        jr nz, .notArrowTile
        ld a, [_y]
        ld b, a
        ld a, [_j]
        sub a, b
        jr nz, .notArrowTile;(x==i && y==j) ?
        ld a, ARROW_RIGHT
        ld [hli], a;tiles[j*2*18+i*2] = ARROW_RIGHT
        jr .setCharTile
.notArrowTile
        xor a
        ld [hli], a;tiles[j*2*18+i*2] = 0
.setCharTile
        ld a, [de]
        ld [hli], a ;tiles[j*2*18+i*2+1] = str_buff[j*9+i];
        inc de
      ld a, [_i]
      inc a
      ld [_i], a
      sub 9
      jr nz, .collumnLoop1

      xor a
      ld [_i], a
      ld a, [_j]
      add a, a; j*2
      inc a ;j*2+1
      ld de, 18
      call Multiply; hl = 18 * (j*2+1)
      ld b, h
      ld c, l ;bc = (j*2+1)*18
      ld hl, tile_buffer
      add hl, bc ;tiles[(j*2+1)*18]
.collumnLoop2 ;for (i = 0; i < 9; ++i) {
        xor a
        ld [hli], a ;tiles[(j*2+1)*18+i*2]   = 0;
        ld [hli], a ;tiles[(j*2+1)*18+i*2+1] = 0;
      ld a, [_i]
      inc a
      ld [_i], a
      sub 9
      jr nz, .collumnLoop2

    ld a, [_j]
    inc a
    ld [_j], a
    sub a, 5
    jr nz, .rowLoop

    ld d, 1
    ld e, 5
    ld h, 18
    ld l, 9
    ld bc, tile_buffer
    call gbdk_SetWinTiles;set_win_tiles(1,5,18,9,tile_buffer);

    WAITPAD_UP
.moveArrowLoop;   while (1) {
      call UpdateInput;k = joypad();
      ld a, [_x]
      ld b, a
      ld d, a
      ld a, [_y]
      ld c, a
      ld e, a
.moveUp;if (button_state & PADF_UP && y > 0) {
      ld a, [button_state]
      and PADF_UP
      jr z, .moveDown
      ld a, [_y]
      and a
      jr z, .moveDown
      dec e
      ld a, e
      ld [_y], a;--y;
      call MoveTextEntryArrow;  move_text_entry_arrow(x,y,x,y-1);
      jp .startOrAPressed
.moveDown;else if (button_state & PADF_DOWN && y < 5) {
      ld a, [button_state]
      and PADF_DOWN
      jr z, .moveLeft
      ld a, [_y]
      sub a, 5
      jr z, .moveLeft
      inc e
      ld a, e
      ld [_y], a;++y;
      call MoveTextEntryArrow;  move_text_entry_arrow(x,y,x,y+1);
      jp .startOrAPressed
.moveLeft;else if (button_state & PADF_LEFT && x > 0 && y < 5) {
      ld a, [button_state]
      and PADF_LEFT
      jr z, .moveRight
      ld a, [_y]
      sub a, 5
      jr z, .moveRight
      ld a, [_x]
      and a
      jr z, .moveRight
      dec d
      ld a, d
      ld [_x], a;  --x;
      call MoveTextEntryArrow;  move_text_entry_arrow(x,y,x-1,y);
      jp .startOrAPressed
.moveRight;else if (button_state & PADF_RIGHT && x < 8 && y < 5) {
      ld a, [button_state]
      and PADF_RIGHT
      jr z, .startOrAPressed
      ld a, [_y]
      sub a, 5
      jr z, .startOrAPressed
      ld a, [_x]
      sub a, 8
      jr z, .startOrAPressed
      inc d
      ld a, d
      ld [_x], a;  ++x;
      call MoveTextEntryArrow;  move_text_entry_arrow(x,y,x+1,y);
.startOrAPressed ;if (button_state & (PADF_START | PADF_A)) {
      ld a, [button_state]
      and PADF_START | PADF_A
      jp z, .bPressed
      ld a, [_y]
      sub a, 5
      jr nz, .testEnd;       if (y == 5) {
      ld a, [_c]
      ld b, a
      ld a, 1
      sub a, b
      ld [_c], a ;c = 1-c;
      jp .exitMoveArrowLoop ;break;
.testEnd ; else if (str_buff[y*9+x] == '\x1E') {
      ld hl, str_buffer
      xor a
      ld b, a
      ld a, [_y]
      add a, a;y*2
      add a, a;y*4
      add a, a;y*8
      ld c, a
      ld a, [_y]
      add a, c;y*9
      ld c, a
      ld a, [_x]
      add a, c;y*9+x
      ld c, a
      add hl, bc ;str_buff[y*9+x]
      ld a, [hl]
      cp "↵" ;0x1E
      jp nz, .testLength
      ld a, [_l]
      jp nz, .exitTextEntryLoop ; if (l > 0) return;
      jr .waitVBL
.testLength;else if (l < max_len) {
      ld a, [_l]
      pop hl ;str
      pop de ;e = max_len
      push de
      push hl
      cp e
      jr nc, .waitVBL
      pop hl ;str
      push hl
      ld c, a; _l
      inc a
      ld [_l], a;_l++
      xor a
      ld b, a
      add hl, bc;hl = str[_l]
      push hl ;str[_l]
      ld hl, str_buffer
      ld a, [_y]
      add a, a;y*2
      add a, a;y*4
      add a, a;y*8
      ld c, a
      ld a, [_y]
      add a, c;y*9
      ld c, a
      ld a, [_x]
      add a, c;y*9+x
      ld c, a
      add hl, bc ;str_buff[y*9+x]
      pop bc;str[_l]
      ld a, [hl]
      ld [bc], a ;str[l++] = str_buff[y*9+x];

      pop hl ;str
      pop de ;e = max_len
      push de
      ld d, e ;d = max_len
      xor a
      ld e, a
      push hl ;str
      call UpdateTextEntryDisplay;update_text_entry_display(str, max_len);

      WAITPAD_UP
      jr .waitVBL
.bPressed;     else if (button_state & PADF_B && l > 0) {
      ld a, [button_state]
      and PADF_B
      jr z, .waitVBL
      ld a, [_l]
      jr z, .waitVBL
      dec a
      ld [_l], a;--l
      ld c, a
      xor a
      ld b, a
      pop hl;str
      push hl
      add hl, bc;str[l]
      ld [hl], a;str[l] = 0;
      pop hl;str
      pop de ;e = max_len
      push de
      ld d, e ;d = max_len
      xor a
      ld e, a
      push hl;str
      call UpdateTextEntryDisplay ;update_text_entry_display(str, max_len);
      WAITPAD_UP
.waitVBL
    call gbdk_WaitVBL
    jp .moveArrowLoop
.exitMoveArrowLoop
    jp .drawTextBoxLoop
.exitTextEntryLoop
  pop af;str
  pop af;a = max_len
  ret

MoveMenuArrow: ;xy on stack, must stay there
  xor a
  ld [_i], a
  ld hl, tile_buffer
.tilesLoop; for (i = 0; i < c; ++i) {
  xor a
  ld [hli], a;   tiles[i*2] = 0;
  ld a, [_j]
  ld c, a
  ld a, [_i]
  sub a, c ;_i - _j
  jp nz, .setZero ;if (i == _j)
  ld a, ARROW_RIGHT ;tiles[i*2+1] = ARROW_RIGHT;
  jr .skip
.setZero
  xor a ;else tiles[i*2+1] = 0;
.skip
  ld [hli], a ;tiles[i*2+1]

  ld a, [_i]
  inc a
  ld [_i], a;++_i
  ld b, a
  ld a, [_c]
  sub a, b ;_c-_i
  jp nz, .tilesLoop

  ; pop de ;xy
  ; push de ;xy must stay on stack
  ld a, 1
  ; add a, d
  ld d, a ;x=x+1
  ld a, 1
  ; add a, e
  ld e, a ;y=y+1

  ld a, 1
  ld h, a ;w=1
  ld a, [_c]
  add a, a
  ld l, a ;h=_c*2
  
  ld bc, tile_buffer
  call gbdk_SetBKGTiles;set_bkg_tiles(x+1,y+1,1,c*2,tile_buffer);
  ret

DrawListEntry: ;set_bkg_tiles(b+2,_j,_l,1,hl);
  ;store register state
  push bc ;xy
  push de ;wh
  push hl ;text

  ;reorganize registers to use with gbdk_SetBKGTiles
  pop bc ;text
  pop hl ;wh
  pop de ;xy
  push de ;xy
  push hl ;wh
  push bc ;text

  ld a, d
  add a, 2
  ld d, a;x = x+2
  ld a, [_j]
  ld e, a;y = _j
  ld a, a
  ld a, [_l]
  ld h, a;w = _l
  ld a, 1
  ld l, a;h = 1
  ld bc, tile_buffer
  call gbdk_SetBKGTiles

  ;restore initial register state
  pop hl ;text
  pop de ;wh
  pop bc ;xy
  ret 

UIShowListMenu::; returns a, bc = xy, de = wh, text = [str_buffer], title = [name_buff]
  push bc ;xy
  push de ;wh
  call DrawBKGUIBox; draw_bkg_ui_box(x,y,w,h);
  pop de ;wh
  pop bc ;xy

  xor a
  ld [_l], a ; length of current entry
  ld [_c], a ; number of rows (used later)
  ld a, c
  add a, 2
  ld [_j], a ;y position to draw entry
  ld hl, str_buffer ; first letter of current entry (from text)
.drawListEntriesLoop
  push bc ;xy
  push de ;wh
  push hl ;text
.testNewLine; if (text[k] == '\n') {
  ld a, [hl] ;text
  cp "\n"
  jr nz, .testStringEnd
  call DrawListEntry;set_bkg_tiles(x+2,j,l,1,tile_buffer);
  xor a
  ld [_l], a
  ld a, [_j]
  add a, 2
  ld [_j], a
  ld a, [_c]
  inc a
  ld [_c], a
  jr .nextCharacter
.testStringEnd; else if (text[k] == '\0') {
  and a
  jr nz, .copyCharacterToTiles
  call DrawListEntry;set_bkg_tiles(x+2,j,l,1,tile_buffer);
  ld a, [_c]
  inc a
  ld [_c], a
  pop hl ;text
  pop de ;wh
  pop bc ;xy
  jr .exitDrawListEntriesLoop ;break;
.copyCharacterToTiles; else tiles[++l] = text[k];
  ld hl, tile_buffer
  xor a
  ld b, a
  ld a, [_l]
  ld c, a
  inc a
  ld [_l], a
  add hl, bc
  pop bc ;text
  ld a, [bc]
  ld [hl], a
  push bc ;text
.nextCharacter
  pop hl ;text
  inc hl
  pop de ;wh
  pop bc ;xy
  jp .drawListEntriesLoop
.exitDrawListEntriesLoop

  push bc ;xy
  push de ;wh

  push bc
  xor a
  ld [_j], a
  call MoveMenuArrow
  pop bc

  pop de ;wh
  pop bc ;xy
  
  push bc ;xy
  push de ;wh
.drawTitle
  push de ;wh
  ld hl, name_buffer
  call str_Length; puts length in de
  ld a, e ;assumes length is less than 256
  pop de ;wh
  ld e, a ;l = strlen(title); 
  jr z, .skipTitle;if (l > 0) {
  ld a, d ;w
  sub a, e ;w-l
  srl a;i = (w-l)/2;
  add a, b;x+i
  ld b, a
  ld d, e ;w = l
  ld a, 1
  ld e, a ;h = 1
  ;surely there's a better way to do this than rearrange registers
  push bc ;xy
  push de ;wh
  push hl ;title
  pop bc ;title
  pop hl ;wh
  pop de ;xy
  call gbdk_SetBKGTiles ;set_bkg_tiles(x+i,y,l,1,title);
.skipTitle
  pop de ;wh
  pop bc ;xy

  push bc ;xy
  WAITPAD_UP;update_waitpadup();
  xor a
  ld [_j], a ;j = 0;
.moveMenuArrowLoop ;while (1) {
  call UpdateInput
.checkMoveArrowUp ;if (button_state & PADF_UP && j > 0) {
  ld a, [button_state]
  and a, PADF_UP
  jp z, .checkMoveArrowDown
  ld a, [_j]
  or a
  jp z, .checkMoveArrowDown
  call gbdk_WaitVBL ;update_vbl(); 
  ld a, [_j]
  dec a
  ld [_j], a ;--j
  call MoveMenuArrow;move_menu_arrow(--j);
  WAITPAD_UP ;update_waitpadup();
  jr .waitVBLThenLoop
.checkMoveArrowDown ;else if (button_state & PADF_DOWN && _j < _c-1) {
  ld a, [button_state]
  and a, PADF_DOWN
  jp z, .selectMenuItem
  ld a, [_c]
  dec a
  ld b, a
  ld a, [_j]
  cp b
  jp nc, .selectMenuItem
  call gbdk_WaitVBL ;update_vbl(); 
  ld a, [_j]
  inc a
  ld [_j], a ;++j
  call MoveMenuArrow;move_menu_arrow(++j);
  WAITPAD_UP ;update_waitpadup();
  jr .waitVBLThenLoop
.selectMenuItem ;if (button_state & (PADF_START | PADF_A)) 
  ld a, [button_state]
  and a, PADF_START | PADF_A
  jr z, .back
  ld a, [_j]
  inc a ;return j+1;
  jr .exitMenu
.back ;else if (button_state & PADF_B) 
  ld a, [button_state]
  and a, PADF_B
  jr z, .waitVBLThenLoop
  xor a ;return 0;
  jr .exitMenu
.waitVBLThenLoop
  call gbdk_WaitVBL ;update_vbl();
  jp .moveMenuArrowLoop
.exitMenu
  pop bc ;xy
  ret ;return a