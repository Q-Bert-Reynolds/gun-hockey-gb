;Until you meet Bill James, it says "SOMEONE's PC"
TurnedOnComputerText:          DB "%s turned on\nthe PC.", 0
AccessedMyComputerText:        DB "Accessed my PC.", 0
AccessedItemStorageSystemText: DB "Accessed Item\nStorage System.", 0
WhatDoYouWantToDoText:         DB "What do you want\nto do?", 0
ItemStorageSystemMenuText:     DB "WITHDRAW ITEM\nDEPOSIT ITEM\nTOSS ITEM\nLOG OFF", 0
WhatDoYouWantToWithdrawText:   DB "What do you want\nto withdraw?", 0
WithdrewItemText:              DB "Withdrew\n%s", 0
WhatDoYouWantToDepositText:    DB "What do you want\nto deposit?", 0
ItemWasStoredText:             DB "%s was\nstored via PC.", 0

;BILL's PC
; "Accessed BILL's PC."
; "Accessed PLAYER storage system."
; "What?" [FARM AAA]
;  CALL UP 
;     "Who's getting the call?"
;     "Who's getting sent down?"
;     "PLAYER added to the lineup."
;  RELEASE
;  CHANGE FARM
;  SEE YA!

;DOC's PC
; "Accessed DOC's PC."
; "Accessed player rating system."
; "Want to get your ROLéDEX rated?"
; "Closed link to DOC's PC."

; LEAGUE
; "Accessed HALL OF FAME List."

UsePublicComputer:
  call ShowTurnedOnComputerText
  HIDE_WIN
  WAITPAD_UP
  call ShowPlayerAvatar
  ret

UseMyComputer:
  call ShowTurnedOnComputerText
  call AccessMyComputer
  HIDE_WIN
  WAITPAD_UP
  call ShowPlayerAvatar
  ret

ShowTurnedOnComputerText:
  ld hl, TurnedOnComputerText
  ld bc, user_name
  ld de, str_buffer
  call str_Replace
  ld hl, str_buffer
  call RevealTextAndWait
  HIDE_WIN
  ret

AccessMyComputer:
  ld a, 7
  ld [rWX], a
  xor a
  ld [rWY], a
  SHOW_WIN
  HIDE_ALL_SPRITES
  xor a
  ld [list_selection], a
.showItemStorageSystemMenu
    call CopyBkgToWin
    
    ld bc, 12
    ld hl, WhatDoYouWantToDoText
    ld a, DRAW_FLAGS_PAD_TOP | DRAW_FLAGS_WIN
    call DisplayTextAtPos

    ld hl, ItemStorageSystemMenuText
    ld de, str_buffer
    call str_Copy
    xor a
    ld [name_buffer], a
    ld bc, 0
    ld de, $100A
    ld a, DRAW_FLAGS_PAD_TOP | DRAW_FLAGS_WIN
    call ShowListMenu

    cp a, 0
    ret z
    cp a, 4
    ret z

  .withdraw
    cp a, 1
    jr nz, .deposit
    ld a, INVENTORY_MODE_WITHDRAW
    ld [inventory_mode], a
    call ShowInventory
    jp .showItemStorageSystemMenu

  .deposit
    cp a, 2
    jr nz, .toss
    ld a, INVENTORY_MODE_DEPOSIT
    ld [inventory_mode], a
    call ShowInventory
    jp .showItemStorageSystemMenu

  .toss
    cp a, 3
    jp nz, .showItemStorageSystemMenu
    ld a, INVENTORY_MODE_TOSS
    ld [inventory_mode], a
    call ShowInventory
    jp .showItemStorageSystemMenu
