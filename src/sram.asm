SECTION "Save Data", SRAM, BANK[MAIN_SRAM_BANK]
s_machine_wins: DW
s_human_wins: DW

SECTION "Save/Load Code", ROM0
LoadLine::;loads line number bc into line_buffer, address in line_address
  di
  ENABLE_RAM_MBC5

  ;load user and rival names
  SWITCH_RAM_MBC5 MAIN_SRAM_BANK
  ; ld hl, sram_user_name
  ; ld de, user_name
  ; ld bc, sram_main_save_end - sram_user_name
  ; call mem_Copy

  DISABLE_RAM_MBC5
  reti