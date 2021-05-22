PS2_START_BIT_ERROR  EQU %100
PS2_PARITY_BIT_ERROR EQU %010
PS2_FINISH_BIT_ERROR EQU %001

SECTION "PS/2 Keyboard Vars", WRAM0
;PS/2 scan codes are 11 bits (S0123456 7PFxxxxx):
; - (S)tart (always 0)
; - Data Bits (0-7) in reverse order, 
; - Odd (P)arity Bit - if the sum of bits 1-A (data bits + parity) is even, error
; - (F)inish Bit (always 1)
ps2_scan_code:: DB
ps2_interrupt_count:: DB
ps2_interrupt_started:: DB
ps2_errors:: DB;xxxxxSPF shows (S)tart, (P)arity, and (F)inish

SECTION "PS/2 Keyboard Code", ROM0
PS2KeyboardInterrupt::
  push af
  ld a, [rSB]; do this ASAP
  push bc
  ld b, a;store first 8 bits of scan code (S0123456)
  push de
  push hl
  
  xor a
  ld [ps2_errors], a

  bit 7, b;test start bit, should always be 0
  jr z, .loadLastDataBit
.startBitError
  ld a, PS2_START_BIT_ERROR
  ld [ps2_errors], a
.loadLastDataBit
  ld a, b
  and a
  ld a, %10000001;make request with internal clock
  ld [rSC], a;stop waiting for bits
  jr nz, .loadLastDataBitLoop
  ld b, %10000000;if byte is 0, flip start bit to detect shift
.loadLastDataBitLoop
    ld a, [rSB]
    cp a, b
    jr z, .loadLastDataBitLoop
  ld h, a;store data bits
  ld b, a

  cp a, $FF
  jr nz, .loadParityBit
  ld b, %01111111;if byte is all 1s, parity bit will also be 1, so flip bit 7 to detect shift
.loadParityBit
    ld a, [rSB]
    cp a, b
    jr z, .loadParityBit
  ld l, a;bit 0 is parity
  ld b, a

  cp a, $FF
  jr nz, .loadFinishBit
  ld b, %01111111;if byte is all 1s, since stop bit is also 1, flip bit 7 to detect shift
.loadFinishBit
    ld a, [rSB]
    cp a, b
    jr z, .loadFinishBit
  and a, %00000001
  jr nz, .allBitsRead
.finishBitError
  ld a, [ps2_errors]
  or a, PS2_FINISH_BIT_ERROR
  ld [ps2_errors], a

.allBitsRead
  xor a
  ld [rSB], a
  ld a, %10000000
  ld [rSC], a;ask for bits using keyboard clock 
  ld a, [ps2_interrupt_count]
  inc a
  ld [ps2_interrupt_count], a

;data (h) is 01234567, needs to be 76543210 in a
;we can count bits as we do so to test Parity Bit 
.reverseAndCountDataBits
  ld b, 8;bits left
  ld c, 0;bit sum
.reverseAndCountLoop
    srl h;shift data right out of h into carry
    rl a;rotate data left from carry into a
    bit 0, a
    jr z, .decrementBitsLeft
  .addBit
    inc c;if new bit is 1, increment sum
  .decrementBitsLeft
    dec b;bits left
    jr nz, .reverseAndCountLoop
.storeScanCode
  ld [ps2_scan_code], a
  
.testParityBit
  bit 0, l
  jr z, .testDataBitSumPlusParity
.parityBitSet
  inc c;add parity bit to sum
.testDataBitSumPlusParity
  bit 0, c
  jr nz, .restoreRegistersAndReturn
.parityBitError
  ld a, [ps2_errors]
  or a, PS2_PARITY_BIT_ERROR
  ld [ps2_errors], a

.restoreRegistersAndReturn
  pop hl
  pop de
  pop bc
  pop af
  ret

KeyboardDemo::
  di
  DISPLAY_OFF
  ld a, " "
  call ClearScreen
  DISPLAY_ON
  ei

  xor a
  ld [rSB], a
  ld a, %10000000
  ld [rSC], a
  ld [ps2_scan_code], a
.loop
    call gbdk_WaitVBL
    call DrawKeyboardDebugData

    call UpdateInput
    ld a, [button_state]
    and a, PADF_A
    jr z, .loop

  .pressedAbutton

    jp .loop
  ret


HexNumbers: DB "0123456789ABCDEF"
BlankSpace: DB "                ",0

DrawKeyboardDebugData:

.drawScanCode
  ld hl, HexNumbers
  ld d, 0
  ld a, [ps2_scan_code]
  and $F0
  swap a
  ld e, a
  add hl, de
  ld a, [hl]
  ld [_SCRN0], a

  ld hl, HexNumbers
  ld d, 0
  ld a, [ps2_scan_code]
  and $0F
  ld e, a
  add hl, de
  ld a, [hl]
  ld [_SCRN0+1], a

.drawCount
  ld a, [ps2_interrupt_count]
  ld h, 0
  ld l, a
  ld de, str_buffer
  call str_Number
  ld hl, BlankSpace
  ld de, str_buffer
  call str_Append

  ld a, DRAW_FLAGS_BKG
  ld hl, str_buffer
  ld de, $0001
  ld bc, 1
  call DrawText

.drawErrors
  ld hl, str_buffer
  ld a, [ps2_errors]
  ld b, a

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
  ld de, 16
  call DrawText
  ret