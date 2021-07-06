;KBHandleCharacter
;KBHandleArrowKey
;KBHandleTab
;KBHandleEnter
;KBHandleBackspace
;KBHandleDelete
;KBHandleFunctionKey
;KBHandleEscape

;simulates on screen keyboard actions
;_c = caps, _x,_y = char pos
UILowerCase: DB "abcdefghijklmnopqrstuvwxyz *():;[]#%-?!*+/.,"
UIUpperCase: DB "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
KBHandleCharacter::;a = character in ASCII
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz

.findCharXY;ASCII in b
  PUSH_VAR _c
  ld a, 1
  ld [_c], a;assume lowercase
  ld c, 0
  ld hl, UILowerCase
.lowerLoop
    ld a, [hli]
    cp a, b
    jr z, .charFound
    inc c
    ld a, 43
    cp a, c
    jr nc, .lowerLoop
  xor a
  ld [_c], a;upper case
  ld c, 0
  ld hl, UIUpperCase
.upperLoop
    ld a, [hli]
    cp a, b
    jr z, .charFound
    inc c
    ld a, 25
    cp a, c
    jr nc, .upperLoop
  POP_VAR _c
  ret

.charFound
  pop af;discard old _c
  ld h, 0
  ld l, c
  ld c, 9
  call math_Divide
  ld [_x], a
  ld a, l
  ld [_y], a
  TRAMPOLINE DrawTextEntryBox
  ld a, [button_state]
  or a, PADF_A
  ld [button_state], a

  ret

KBHandleArrowKey::;a = DPad key
  ld b, a
  ld a, [button_state]
  or a, b
  ld [button_state], a
  ret

KBHandleTab::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_BUTTONS
  ret nz
  ld a, [button_state]
  or a, PADF_SELECT
  ld [button_state], a
  ret

KBHandleEnter::
  ld a, [button_state]
  or a, PADF_START
  ld [button_state], a
  ret

KBHandleBackspace::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz
  ld a, [button_state]
  or a, PADF_B
  ld [button_state], a
  ret

KBHandleDelete::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz

  ret

KBHandleFunctionKey::;a = function key
  ;TODO: jump between scenes
  ret

KBHandleEscape::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_BUTTONS
  ret nz
  ld a, [button_state]
  or a, PADF_B
  ld [button_state], a
  ret