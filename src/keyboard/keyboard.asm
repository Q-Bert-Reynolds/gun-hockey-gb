INCLUDE "src/keyboard/usb_hid_keys.asm";USB HID key codes
INCLUDE "src/keyboard/ps2_keys.asm";PS/2 keys codes

KB_MODE_PS2     EQU 0
KB_MODE_USB_HID EQU 1

PS2_ERROR_TIMEOUT      EQU %100000
PS2_ERROR_UNKNOWN_CODE EQU %010000
PS2_ERROR_KEYBOARD     EQU %001000
PS2_ERROR_START_BIT    EQU %000100
PS2_ERROR_PARITY_BIT   EQU %000010
PS2_ERROR_FINISH_BIT   EQU %000001

KB_FLAG_RELEASE  EQU %00000001 ;$PS2 code $F0
KB_FLAG_EXTENDED EQU %00000010 ;$PS2 code $E0

KB_MOD_SCROLL_LOCK EQU PS2_LED_SCROLL_LOCK
KB_MOD_NUM_LOCK    EQU PS2_LED_NUM_LOCK
KB_MOD_CAPS_LOCK   EQU PS2_LED_CAPS_LOCK
KB_MOD_SUPER       EQU %00001000
KB_MOD_ALT         EQU %00010000
KB_MOD_CTRL        EQU %00100000
KB_MOD_SHIFT       EQU %01000000
KB_MOD_FUNCTION    EQU %10000000

SECTION "PS/2 Keyboard Vars", WRAM0
;PS/2 scan codes are 11 bits (S0123456 7PFxxxxx):
; - (S)tart (always 0)
; - Data Bits (0-7) in reverse order, 
; - Odd (P)arity Bit - if the sum of bits 1-A (data bits + parity) is even, error
; - (F)inish Bit (always 1)
kb_scan_code:: DB
kb_scan_code_buffer:: DS 8;holds last 8 scan codes
kb_error_buffer:: DS 8;holds last 8 errors codes
kb_buffer_write:: DB
kb_buffer_read:: DB;TODO: r/w only use 4 bit, could be one byte
kb_interrupt_count:: DB
kb_error:: DB;xxTUKSPF - (T)imeout, (U)nknown Scan Code, (K)eyboard $00 or $FF, (S)tart Bit, (P)arity Bit, (F)inish Bit
kb_error_count:: DB
kb_modifiers:: DB ;FSCAUPNL - (F)unction, (S)hift, (C)trl, (A)lt, S(U)per, Ca(P)s Lock, (N)um Lock, Scro(L)l Lock
kb_flags:: DB;xxxxxxER - (E)xtended key flag, (R)elease key flag
;WRAM used but defined elsewhere:
;   _x and _y for character position on screen
;   _i is the toggle between typing and debug displays

SECTION "PS/2 Keyboard Code", ROM0
INCLUDE "src/keyboard/ps2_ascii_keymaps.asm"
INCLUDE "src/keyboard/ps2_handlers.asm"
INCLUDE "src/keyboard/ps2_jump_table.asm"

PS2KeyboardInterrupt::
  ld a, [rSB]; load now before the first 3 bits get shifted out
  ld b, a;store first 8 bits of scan code (S0123456)
  
.testStartBit
  bit 7, b;test start bit, should always be 0
  jr z, .waitForLastBits
.startBitError
  ld a, PS2_ERROR_START_BIT
  ld [kb_error], a
  jp .checkAllErrors;bail early if start bit not 0

.waitForLastBits
  ld a, SCF_TRANSFER_START | SCF_CLOCK_EXTERNAL
  ld [rSC], a      ;ask for more bits using keyboard clock 
  xor a
  ld [kb_error], a ;reset current error code
  ld a, b          ;S0123456
  swap a           ;3456S012
  rrca             ;23456S01
  and a, %11111001 ;23456xxx, bit 7 and parity unknown
  or  a, %00000001 ;23456xxF, finish bit is always 1
  ld c, a          ;expected value after shift
  ld e, 255
.pollBitsLoop
    ld a, [rSB]
    ld d, a;actual value
    and a, %11111001;exit condition
    cp a, c;if bits & exit == expected, done
    jr z, .allBitsRead
    dec e
    jr z, .timeout
    jr .pollBitsLoop
.timeout
  ld a, [kb_error]
  or a, PS2_ERROR_TIMEOUT
  ld [kb_error], a
  
.allBitsRead
  ld c, d;replace incomplete expected value with actual value (234567PF)
  ld a, SCF_TRANSFER_STOP | SCF_CLOCK_EXTERNAL
  ld [rSC], a;stop asking for stuff, use external clock

.testFinishBit
  bit 0, c;test finish bit, should always be 1
  jr nz, .storeParityBit
.finishBitError
  ld a, [kb_error]
  or a, PS2_ERROR_FINISH_BIT
  ld [kb_error], a

.storeParityBit
  ld a, c
  srl a;bit 0 is parity
  and a, %00000001
  ld e, a

.storeDataBits
  sla b  ;0123456x
  ld a, c;234567PF
  srl a  ;x234567P
  srl a  ;xx234567
  or a, b;01234567
  ld d, a;data bits

; ;data (d) is 01234567, needs to be 76543210 in a
; ;we can count bits as we do so to test Parity Bit 
.reverseAndCountDataBits
  ld b, 8;bits left
  ld c, 0;bit sum
.reverseAndCountLoop
    srl d;shift data right out of d into carry
    rl a;rotate data left from carry into a
    bit 0, a
    jr z, .decrementBitsLeft
  .addBit
    inc c;if new bit is 1, increment sum
  .decrementBitsLeft
    dec b;bits left
    jr nz, .reverseAndCountLoop
.storeScanCode
  ld [kb_scan_code], a

.testParityBit
  ld a, c;bit sum
  and a, %00000001
  cp a, e;bit 0 of e is parity, and bit 0 of a is even/odd. These should never be equal.
  jr nz, .writeScanCodeToBuffer
.parityBitError
  ld a, [kb_error]
  or a, PS2_ERROR_PARITY_BIT
  ld [kb_error], a

.writeScanCodeToBuffer
  ld hl, kb_scan_code_buffer
  ld a, [kb_buffer_write]
  ld b, 0
  ld c, a
  add hl, bc
  ld a, [kb_scan_code]
  ld [hl], a

.writeErrorCodeToBuffer
  ld hl, kb_error_buffer
  ld a, [kb_buffer_write]
  ld b, 0
  ld c, a
  add hl, bc
  ld a, [kb_error]
  ld [hl], a

.incrementWriteHead
  ld a, [kb_buffer_write]
  inc a
  and a, %00000111;a%8
  ld [kb_buffer_write], a

.checkAllErrors
  ld a, [kb_error]
  and a
  jr z, .askForNextBits
  ld a, [kb_error_count]
  inc a
  ld [kb_error_count], a

.askForNextBits
  ld a, [kb_interrupt_count]
  inc a
  ld [kb_interrupt_count], a
  xor a
  ld [rSB], a;clear serial bits
  ld a, SCF_TRANSFER_START | SCF_CLOCK_EXTERNAL
  ld [rSC], a;start next transfer using external clock
  ret

KeyboardDemo::
  di
  DISPLAY_OFF
  ld a, " "
  call ClearScreen
  DISPLAY_ON
  ei
  
  ld a, 5
  ld [_y], a
  xor a
  ld [_x], a
  ld [_i], a
  ld [rSB], a
  ld a, SCF_TRANSFER_START | SCF_CLOCK_EXTERNAL
  ld [rSC], a;ask for bits using keyboard clock 
  ld [kb_scan_code], a
  ld b, 0
.loop
    call gbdk_WaitVBL
    call ProcessPS2Keys
    call DrawKeyboardDebugData
    call UpdateInput
  .testAButton
    ld a, [button_state]
    and a, PADF_A
    jr z, .loop
    ld a, [last_button_state]
    and a, PADF_A
    jr nz, .loop
  .pressedAButton;clear screen
    ld a, " "
    ld bc, 32*32+20*18
    ld hl, _SCRN0
    call mem_SetVRAM
    ld a, [_i]
    xor a, 1
    ld [_i], a
    jp .loop
  ret

ProcessPS2Keys:
  ld a, [kb_buffer_write]
  ld b, a;b = write index
  ld a, [kb_buffer_read]
  ld d, 0
  ld e, a;de = read index
  cp a, b
  ret z;if read == write, done

  ld c, 3;process maximum 3 scan codes per frame
.loop
    ld hl, kb_error_buffer
    add hl, de;[hl] = current error
    ld a, [hl];error code
    and a;check for errors
    jr nz, .incrementReadIndex;if errors found, don't process keycode

  .processKeyCode
    ld hl, kb_scan_code_buffer
    add hl, de;[hl] = current scan code
    ld a, [hli];scan code
    push de;store read index
    push bc;store write index, num left
    call PS2HandleKeycode
    pop bc;restore write index, num left
    pop de;restore read index

  .incrementReadIndex
    ld a, e
    inc a
    and a, %00000111;read%8
    ld e, a;de = read index
    ld [kb_buffer_read], a

  .checkDone
    dec c;if max scan codes read, done
    ret z
    cp a, b;if read == write, done
    ret z
    jp .loop

DrawBinaryNumber:;b = byte to draw, hl = screen location
  ld c, 8
.loop
    LCD_WAIT_VRAM
    ld a, "1"
    sla b
    jr c, .setBuffer
    ld a, "0"
  .setBuffer
    ld [hli], a
    dec c
    jr nz, .loop
  ret

HexNumbers: DB "0123456789ABCDEF"
DrawHexNumber:;a = number, bc = screen address
  push af;number to draw
  and $F0;upper nibble only
  swap a;(ie. a/16)
  ld hl, HexNumbers
  ld d, 0
  ld e, a
  add hl, de
  LCD_WAIT_VRAM
  ld a, [hl]
  ld [bc], a;upper nibble in first screen address

  inc bc;lower nibble goes in next screen address

  pop af;number to draw
  and $0F;lower nibble only (ie. a%16)
  ld hl, HexNumbers
  ld d, 0
  ld e, a
  add hl, de
  LCD_WAIT_VRAM
  ld a, [hl]
  ld [bc], a

  inc bc;increment screen address so numbers can be drawn sequentially

  ret

BlankSpace: DB "  ",0
InterruptsText: DB " int ",0
ErrorsText: DB " err",0
ClearedDashesText: DB "_____",0
ShiftText: DB "shift",0
DrawKeyboardDebugData:
.drawKeyASCII
  ld hl, PS2toASCIIKeymap
  ld a, [kb_scan_code]
  ld b, 0
  ld c, a
  add hl, bc
  LCD_WAIT_VRAM
  ld a, [hl]
  ld [_SCRN0], a

.drawScanCodeHex
  ld a, [kb_scan_code]
  ld bc, _SCRN0+5
  call DrawHexNumber

.drawScanCodeDecimal
  ld a, [kb_scan_code]
  ld h, 0
  ld l, a
  ld de, str_buffer
  call str_Number
  ld hl, BlankSpace
  ld de, str_buffer
  call str_Append

  ld a, DRAW_FLAGS_BKG
  ld hl, str_buffer
  ld de, $0800
  ld bc, 1
  call DrawText

.drawScanCodeBinary
  ld hl, _SCRN0+12
  ld a, [kb_scan_code]
  ld b, a
  call DrawBinaryNumber

.drawScanCodeBuffer
  ld a, "S"
  ld [_SCRN0+32], a
  ld hl, kb_scan_code_buffer
  ld a, 8
  ld bc, _SCRN0+32+2
.drawScanCodeBufferLoop
    push af; scan codes left to draw
    ld a, [hli]
    push hl
    call DrawHexNumber
    pop hl
    pop af
    dec a
    jr nz, .drawScanCodeBufferLoop

.drawErrorBuffer
  ld a, "E"
  ld [_SCRN0+32*2], a
  ld hl, kb_error_buffer
  ld a, 8
  ld bc, _SCRN0+32*2+2
.drawErrorBufferLoop
    push af; scan codes left to draw
    ld a, [hli]
    push hl
    call DrawHexNumber
    pop hl
    pop af
    dec a
    jr nz, .drawErrorBufferLoop

  ld a, " "
  ld hl, str_buffer
  ld bc, 20
  call mem_Set

  ld hl, str_buffer
  ld a, [kb_buffer_write]
  ld b, 0
  ld c, a
  add hl, bc
  add hl, bc
  inc hl
  inc hl
  ld a, ARROW_UP
  ld [hl], a

  ld hl, str_buffer
  ld de, _SCRN0+32*3
  ld bc, 20
  call mem_CopyVRAM

.drawInterruptCount
  ld a, [kb_interrupt_count]
  ld h, 0
  ld l, a
  ld de, str_buffer
  call str_Number
  ld hl, InterruptsText
  ld de, str_buffer
  call str_Append

.drawError
  ld a, [kb_error_count]
  ld h, 0
  ld l, a
  ld de, name_buffer
  call str_Number
  ld hl, name_buffer
  ld de, str_buffer
  call str_Append
  ld hl, ErrorsText
  ld de, str_buffer
  call str_Append

  ld a, DRAW_FLAGS_BKG
  ld hl, str_buffer
  ld de, 4
  ld bc, 1
  call DrawText

  ld hl, str_buffer
  ld a, [kb_error]
  ld b, a

.testTimeoutError
  bit 5, b
  ld a, "T"
  jr nz, .drawTimeoutError
.noTimeoutError
  ld a, "_"
.drawTimeoutError
  ld [hli], a

.testUnknownCodeError
  bit 4, b
  ld a, "U"
  jr nz, .drawUnknownCodeError
.noUnknownCodeError
  ld a, "_"
.drawUnknownCodeError
  ld [hli], a

.testKeyboardError
  bit 3, b
  ld a, "K"
  jr nz, .drawKeyboardError
.noKeyboardError
  ld a, "_"
.drawKeyboardError
  ld [hli], a

.testStartBitError
  bit 2, b
  ld a, "S"
  jr nz, .drawStartBitError
.noStartBitError
  ld a, "_"
.drawStartBitError
  ld [hli], a

.testParityBitError
  bit 1, b
  ld a, "P"
  jr nz, .drawParityBitError
.noParityBitError
  ld a, "_"
.drawParityBitError
  ld [hli], a

.testFinishBitError
  bit 0, b
  ld a, "F"
  jr nz, .drawFinishBitError
.noFinishBitError
  ld a, "_"
.drawFinishBitError
  ld [hli], a

  xor a
  ld [hl], a;end string

  ld a, DRAW_FLAGS_BKG
  ld hl, str_buffer
  ld bc, 1
  ld d, 14
  ld e, 4
  call DrawText
  ret