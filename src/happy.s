.include "constants.inc"
.include "header.inc"
.include "pently.inc"

DICE_ADDR = $0200
PIPS_ADDR = $0260
CURSOR_ADDR = $02A0

DELAY = 30
SUB_DELAY = 10

QUEUE_CELL = $97

.enum game_states
  before_start
  player_will_roll
  dice_rolling
  movement
  ended
  where_to
  symbols_setup
  end_turn
.endenum

.enum pently_songs
  Happy_lullaby
  A_winner_is_you
.endenum

.zeropage
.import buttons
.import last_frame_buttons
.import released_buttons
.import pressed_buttons

addr_ptr: .res 2 ; generic address pointer
player_cells: .res 8 ; array of players current cells
current_player: .res 1
game_state: .res 1
rng_seed: .res 2
current_die: .res 1
delay: .res 1
choice: .res 1
alt_choice: .res 1
choice_flick: .res 1
temp_a: .res 1
temp_b: .res 1
player_inventory: .res 8 ; array of player inventory
symbol_positions: .res 8 ; array of symbol positions
num_players: .res 1      ; number of players (4-8)
player_positions: .res 8 ; array of player positions, to be skipped
temp_c: .res 1
extra_turns: .res 8      ; extra turns per player
blue_shell: .res 1       ; indicates one player as all 8 symbols
bridge_1: .res 1         ; 0 if bridge enabled, else = countdown to enable
bridge_2: .res 1         ; idem
tvSystem: .res 1         ; Pently-detected tv system
pently_zptemp: .res 5    ; Pently scratch space
nmis: .res 1             ; NMI counter
old_nmis: .res 1

.exportzp tvSystem, pently_zptemp

.segment "CODE"

.import reset_handler
.import readjoy

.import getTVSystem

.macro print xpos, ypos, string
  LDA #<string
  STA addr_ptr
  LDA #>string
  STA addr_ptr+1
  LDX xpos
  LDY ypos
  JSR write_tiles
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  save_regs
  JSR game_state_handler

  ; Fix Scroll
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUSCROLL
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUADDR

  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  INC nmis
  restore_regs

  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

  ; write board background data
load_board_background:
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  lda #<board_nametable
  sta addr_ptr
  lda #>board_nametable
  sta addr_ptr+1

  ; Copy one page of data ($100 bytes)
copy_board_bg_page:
  ldy #0
copy_board_bg_loop:
  lda (addr_ptr), y
  sta PPUDATA
  iny
  bne copy_board_bg_loop

  ; Move on to the next page, or leave if finished
  lda addr_ptr+1
  clc
  adc #1
  sta addr_ptr+1
  cmp #(>board_nametable + 4)
  bne copy_board_bg_page

  ; clean sprite data
  LDX #$00
  LDA #$FF
clean_sprites:
  STA $0200,X
  INX
  CPX #$00
  BNE clean_sprites

  ; write sprite data
  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$A4        ; size of sprites list
  BNE load_sprites

  LDA #4
  STA num_players

  LDA #game_states::before_start
  STA game_state

  LDA #$10
  STA rng_seed
  LDA #$25
  STA rng_seed+1

  JSR pently_init
  JSR getTVSystem

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  LDA #pently_songs::Happy_lullaby
  JSR pently_start_music

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  JSR pently_update
etc:
  LDA #game_states::symbols_setup
  CMP game_state
  BNE forever
  LDA symbol_positions+7 ; checking if all symbol positions are set
  BNE forever
  JSR reset_symbol_positions ; reset symbol positions takes too long and requires a good rng seed
  JSR reset_players ; since we're here, lets reset players as well
  JMP forever
.endproc

.proc rand
  ; generate random number (0-255) in A
  ; - clobbers Y
  LDA rng_seed+1
  TAY ; store copy of high byte
  ; compute rng_seed+1 ($39>>1 = %11100)
  LSR ; shift to consume zeroes on left...
  LSR
  LSR
  STA rng_seed+1 ; now recreate the remaining bits in reverse order... %111
  LSR
  EOR rng_seed+1
  LSR
  EOR rng_seed+1
  EOR rng_seed ; recombine with original low byte
  STA rng_seed+1
  ; compute rng_seed ($39 = %111001)
  TYA ; original high byte
  STA rng_seed
  ASL
  EOR rng_seed
  ASL
  EOR rng_seed
  ASL
  ASL
  ASL
  EOR rng_seed
  STA rng_seed
  RTS
.endproc

.proc rand_die
  ; generate random number (0-5) in A
reroll:
  JSR rand
  AND #%111
  CMP #6
  BCS reroll
  RTS
.endproc

.proc reset_players
  ; set players position
  ; move pips to initial position
  LDA #0
  STA current_player
  LDX #0
iterate_players:
  LDA #0
  STA extra_turns,X
  TXA
  CLC
  ADC #QUEUE_CELL
  STA player_cells,X
  TAY
  LDA cell_position,Y
  STA player_positions,X
  TAY
  JSR move_pip
  INX
  CPX num_players
  BNE iterate_players
  RTS
.endproc

.proc rand_cell
  ; generate random cell ($02-$95) in A
reroll:
  JSR rand
  CMP #$00
  BEQ reroll
  CMP #$01
  BEQ reroll
  CMP #$96
  BCS reroll
  RTS
.endproc

.proc reset_symbol_positions
  save_regs

  LDX #0
main_loop:
  JSR rand_cell
  TAY
  LDA cell_position,Y
  STA temp_a

  TXA ; A = Y = X
  TAY
check_conflict:
  BEQ ok
  DEY
  LDA temp_a
  CMP symbol_positions,Y
  BEQ main_loop
  TYA
  JMP check_conflict

ok:
  LDA temp_a
  STA symbol_positions,X
  INX
  CPX #8
  BNE main_loop

  restore_regs
  RTS
.endproc

.proc move_pip
  ; X = pip, Y=(y,x) position
  ; X = pip | #$80 if non-moving position
  ; Move (pip)th pip sprite to (y,x) position
  ; - preserves X,Y,A

  save_regs

  TXA
  AND #$80
  BNE final_position
  LDA #$06
  STA temp_a
  JMP continue
final_position:
  LDA #$0A
  STA temp_a
continue:

  TXA
  ASL
  ASL
  ASL
  TAX ; X = 8*pip
  TYA
  AND #%11110000 ; A := (y,0)
  CLC
  ADC temp_a
  STA PIPS_ADDR,X
  ADC #$08
  STA PIPS_ADDR+4,X
  TYA
  ASL
  ASL
  ASL
  ASL ; A := (x,0)
  CLC
  ADC #$03
  STA PIPS_ADDR+3,X
  STA PIPS_ADDR+7,X

  restore_regs

  RTS
.endproc

.proc move_die
  ; X = die, Y=(y,x) position
  ; Move (die)th die sprite to (y,x) position
  ; - preserves X,Y,A
  save_regs

  TXA
  ASL
  ASL
  ASL
  ASL
  TAX ; X = 16*die
  TYA
  AND #%11110000 ; A := (y,0)
  STA DICE_ADDR,X
  STA DICE_ADDR+4,X
  ORA #%00001000 ; A := (y,8)
  STA DICE_ADDR+8,X
  STA DICE_ADDR+12,X
  TYA
  ASL
  ASL
  ASL
  ASL ; A := (x,0)
  STA DICE_ADDR+3,X
  STA DICE_ADDR+11,X
  ORA #%00001000  ; A := (x,8)
  STA DICE_ADDR+7,X
  STA DICE_ADDR+15,X

  ; color die
  LDA current_player
  STA DICE_ADDR+2,X
  STA DICE_ADDR+6,X
  STA DICE_ADDR+10,X
  STA DICE_ADDR+14,X

  restore_regs
  RTS
.endproc

.proc hide_die
  ; X = die
  ; Hides die (move to ($F0, $F0))
  ; - preserves X,Y,A
  PHA ; save Y
  LDY #$FF
  JSR move_die
  PLA
  TAY ; restore Y
  RTS
.endproc

.proc get_symbol
  ; Y=(y,x) player position
  ; Add symbol to inventory if player on it
  ; - preserves X,Y,A
  save_regs
  LDX #0
  LDA #%10000000
  STA temp_b
loop:
  TYA
  CMP symbol_positions,X
  BEQ found
  INX
  CLC
  ROR temp_b
  BCC loop
  JMP end
found:
  LDX current_player
  LDA temp_b
  ORA player_inventory,X
  STA player_inventory,X
  CMP #$FF
  BNE not_blue_shell
  LDA #$01
  STA blue_shell
not_blue_shell:
  JSR display_inventory
end:
  restore_regs
  RTS
.endproc

.proc display_inventory
  ; draw inventory for current player
  ; - preserves X,Y,A
  save_regs
  LDX current_player
  LDA player_inventory,X
  LDY #0
loop:
  LDX PPUSTATUS
  LDX #$23
  STX PPUADDR
  LDX inventory_positions,Y
  STX PPUADDR
  ROL
  LDX #$FF
  BCC next
  LDX inventory_tiles,Y
next:
  STX PPUDATA
  INY
  CPY #8
  BNE loop

  restore_regs
  RTS
.endproc

.proc draw_symbol
  ; draw board symbol index X, on Y=(y,x) cell
  ; - preserves X,Y,A
  save_regs
  TYA
  CLC
  ADC #$10
  TAY      ; (y,x) = board coordinates, screen y is off by one

  ; bg positions
  ; offset = (y*2) * $20 + (x*2)
  ; |offset        offset + 1   |
  ; |offset + $20  offset + $21 |
  ;
  ; tiles
  ; |$32 $33| + 2 * index
  ; |$42 $43|

  LDA #$20
  STA addr_ptr+1
  LDA #$00
  STA addr_ptr

  ; offset
  TYA
  CLC
  ROL
  ROL
  ROL
  AND #%11
  CLC
  ADC addr_ptr+1
  STA addr_ptr+1 ; addr_ptr = $2000 + (y*2) * $20 // first 2 bits of y

  TYA
  AND #%11110000
  ASL
  ASL
  CLC
  ADC addr_ptr
  STA addr_ptr
  LDA #0
  ADC addr_ptr+1
  STA addr_ptr+1 ; addr_ptr += (y*2) * $20 // last 6 bits of y

  TYA
  AND #%00001111
  ASL
  CLC
  ADC addr_ptr
  STA addr_ptr
  LDA #0
  ADC addr_ptr+1
  STA addr_ptr+1 ; addr_ptr += x * 2

  LDA PPUSTATUS
  LDA addr_ptr+1
  STA PPUADDR
  LDA addr_ptr
  STA PPUADDR

  CLC

  TXA
  AND #%11
  ASL
  STA temp_a ; temp_a = 2*index (bit 3 = color)

  ADC #$32
  STA PPUDATA
  LDA temp_a
  ADC #$33
  STA PPUDATA

  LDA #$20
  ADC addr_ptr
  STA addr_ptr
  LDA #$00
  ADC addr_ptr+1
  STA addr_ptr+1


  LDA addr_ptr+1
  STA PPUADDR
  LDA addr_ptr
  STA PPUADDR

  CLC

  LDA temp_a
  ADC #$42
  STA PPUDATA
  LDA temp_a
  ADC #$43
  STA PPUDATA

  restore_regs
  RTS
.endproc


.proc paint_cell
  ; X = palette, Y=(y,x) cell
  ; cobbles A, Y
  TYA
  CLC
  ADC #$10
  TAY      ; (y,x) = board coordinates, screen y is off by one

  LSR
  AND #%111
  STA temp_a       ; temp_a := x/2
  TYA
  AND #%11100000
  LSR
  LSR              ; A := y/2 * 8
  ORA temp_a       ; A := y/2 * 8 + x/2 == byte position in attribute table
  STA temp_a

  LDA #$23
  STA addr_ptr+1
  LDA #$C0
  STA addr_ptr
  CLC
  LDA temp_a
  ADC addr_ptr
  STA addr_ptr
  LDA #$00
  ADC addr_ptr+1   ; addr_ptr = address in attribute table
  STA addr_ptr+1

  LDA PPUSTATUS
  LDA addr_ptr+1
  STA PPUADDR
  LDA addr_ptr
  STA PPUADDR
  LDA PPUDATA     ; A := meta-meta-sprite attribute byte
  LDA PPUDATA     ; https://forums.nesdev.com/viewtopic.php?f=10&t=14147#p169453 (buffered reads?)
  STA temp_a

  TYA
  AND #%1
  STA temp_b    ; temp_b = x mod 2
  TYA
  LSR
  LSR
  LSR
  AND #%10
  ORA temp_b    ; A := (y mod 2, x mod 2)
  STA temp_b

  LDA temp_a
  LDY temp_b
  AND paint_masks,Y
  STA temp_a

  TXA
  ASL
  ASL
  CLC
  ADC temp_b
  TAY

  LDA temp_a
  ORA paint_palettes,Y
  TAY

  LDA PPUSTATUS
  LDA addr_ptr+1
  STA PPUADDR
  LDA addr_ptr
  STA PPUADDR
  STY PPUDATA

  RTS
.endproc

; these act like printf, displaying the corresponding digit instead
CURRENT_PLAYER_SYMBOL = $FE

DIGIT_TILES = $1B
.proc write_tiles
  ; write tiles on background
  ; addr_ptr - point to string starting point (strings end with $00)
  ; X,Y - PPU target (e.g $20, $00 = origin)
  ; When the tile is #CURRENT_PLAYER_SYMBOL, the current player number
  ; is written instead (e.g. '1' tile for current_player 0)
  LDA PPUSTATUS
  STX PPUADDR
  STY PPUADDR
  LDY #0
writing_loop:
  LDA (addr_ptr), Y
  BEQ exit
  CMP #CURRENT_PLAYER_SYMBOL
  BNE write_tile
  CLC
  LDA #DIGIT_TILES+1
  ADC current_player
write_tile:
  STA PPUDATA
  INY
  JMP writing_loop
exit:
  RTS
.endproc

.proc game_state_handler
  ; Uses RTS Trick
  LDA game_state
  ASL
  TAX
  LDA game_state_handlers+1, X
  PHA
  LDA game_state_handlers, X
  PHA
  RTS
.endproc

.proc game_state_before_start
  JSR rand ; shuffle rng seed
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ not_start

  LDA #game_states::symbols_setup
  STA game_state
  LDA #0
  STA temp_c
  JMP exit

not_start:
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ not_left
  DEC num_players
  LDA #3
  CMP num_players
  BNE refresh
  LDA #8
  STA num_players
  JMP refresh

not_left:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ exit
  INC num_players
  LDA #9
  CMP num_players
  BNE refresh
  LDA #4
  STA num_players
refresh:
  LDA PPUSTATUS
  LDA #$23
  STA PPUADDR
  LDA #$44
  STA PPUADDR
  CLC
  LDA #$1B
  ADC num_players
  STA PPUDATA
exit:
  RTS
.endproc

.proc game_state_symbols_setup
  LDA symbol_positions+7 ; checking if all symbol positions are set
  BEQ skip               ; wait until fully setup in main loop

  LDX temp_c
  LDY symbol_positions,X
  JSR draw_symbol
  LDA temp_c
  LSR
  LSR
  ORA #%10
  TAX
  JSR paint_cell
  LDX temp_c
  INX
  CPX #8
  BEQ finish_setup
  STX temp_c
  RTS
finish_setup:
  print #$23, #$22, string_clear_16
  print #$23, #$22, string_player_n
  print #$23, #$42, string_press_a_to_roll
  JSR display_inventory
  LDA #game_states::player_will_roll
  STA game_state
skip:
  RTS
.endproc

.proc game_state_player_will_roll
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ not_roll

  print #$23, #$42, string_clear_16
  LDA #game_states::dice_rolling
  STA game_state
not_roll:
  RTS
.endproc

.proc game_state_dice_rolling
  ; TODO: roll dice cosmetically, then choose number of steps and begin movement
  LDX current_die
  JSR hide_die
  JSR rand_die
  STA current_die
  TAX
  LDY #$87
  JSR move_die

  LDA #game_states::movement
  STA game_state
  LDA #DELAY
  STA delay
  LDA #0
  STA choice
  RTS
.endproc

.proc detect_bridge
  ; detect if there's a bridge available for cell (Y)
  ; returns bridge destination in X
  ; - preserves Y
  LDX #0
  TYA
loop:
  CMP bridge_cells,X
  BEQ found
  INX
  CPX #8
  BNE loop
  JMP fail_return
found:
  LDA bridge_numbers,X
  BNE when_bridge_2
  LDA bridge_1
  BNE fail_return
  LDA bridge_targets,X
  TAX
  JMP return
when_bridge_2:
  LDA bridge_2
  BNE fail_return
  LDA bridge_targets,X
  TAX
return:
  RTS
fail_return:
  LDX #0
  RTS
.endproc

.proc close_bridge_if_used
  ; X = current player cell
  ; if X is a bridge target, close the bridge (for now)
  ; - cobbles Y
  LDY #0
  TXA
loop:
  CMP bridge_targets,Y
  BEQ found
  INY
  CPY #8
  BNE loop
  JMP return
found:
  LDA bridge_numbers,Y
  BNE when_bridge_2

  LDA #4       ; TODO tweak, constantize
  STA bridge_1

  TXA
  PHA
  LDX #%01
  LDY #$62
  JSR paint_cell
  LDY #$63
  JSR paint_cell
  LDY #$64
  JSR paint_cell
  PLA
  TAX

  JMP return
when_bridge_2:

  LDA #4       ; TODO tweak, constantize
  STA bridge_2

  TXA
  PHA
  LDX #%01
  LDY #$7C
  JSR paint_cell
  LDY #$7D
  JSR paint_cell
  PLA
  TAX

return:
  RTS
.endproc

.proc restore_bridges
  save_regs

  LDA bridge_1
  BEQ when_bridge_2
  DEC bridge_1
  BNE when_bridge_2

  LDX #%00
  LDY #$62
  JSR paint_cell
  LDY #$63
  JSR paint_cell
  LDY #$64
  JSR paint_cell

  JMP return
when_bridge_2:

  LDA bridge_2
  BEQ return
  DEC bridge_2
  BNE return

  LDX #%00
  LDY #$7C
  JSR paint_cell
  LDY #$7D
  JSR paint_cell

return:
  restore_regs
  RTS
.endproc

.proc game_state_movement
  LDX choice    ; if player has choosen a target between two options, skip to it
  BNE any_path

  LDA delay
  BEQ move
  DEC delay
  RTS
move:
  LDA #DELAY
  STA delay

  LDX current_player
  LDY player_cells,X
  JSR detect_bridge ; sets X like cell_alt_target if bridge available here
  TXA
  BNE bridged
  LDX cell_alt_target,Y
  BEQ single_path ; no need to choose
bridged:
  ; choosing state
  STX alt_choice
  LDX cell_target,Y
  STX choice
  LDA #0
  STA choice_flick
  STA delay
  LDA #game_states::where_to
  STA game_state

  LDA #10
  STA delay

  print #$23, #$22, string_a_to_choose
  print #$23, #$42, string_b_to_toggle

  RTS
single_path:
  LDX cell_target,Y
any_path:
  JSR close_bridge_if_used
  LDA #0
  STA choice
  LDY current_player
  STX player_cells,Y
  LDY cell_position,X
  LDX current_player
  JSR move_pip
  JSR get_symbol

  CPY #$A7 ; origin position
  BNE continue_turn

  LDX current_player
  LDA player_inventory,X
  CMP #$FF
  BNE continue_turn
  print #$23, #$42, string_you_win

  LDA #pently_songs::A_winner_is_you
  JSR pently_start_music

  LDA #game_states::ended
  STA game_state
  RTS
continue_turn:
  ; consume die
  LDA current_die
  BEQ finish_movement
  TAX
  JSR hide_die
  DEX
  STX current_die
  LDY #$87
  JSR move_die
  RTS

finish_movement:
  LDX current_die
  JSR hide_die

  ; if over another player, don't finish yet, skip them instead
  LDX current_player
  LDY player_cells,X
  LDA cell_position,Y
  LDX #0
check_other_player_loop:
  CMP player_positions,X
  BEQ skip_player
  INX
  CPX num_players
  BNE check_other_player_loop
  JMP really_finish
skip_player:
  INC extra_turns,X ; skipped player gets extra turn
  RTS

really_finish:
  LDX current_player
  ; store new position
  STA player_positions,X
  TAY
  LDA #$80
  ORA current_player
  TAX
  JSR move_pip

  LDX current_player
  LDA extra_turns,X
  BEQ just_pass
  print #$23, #$42, string_press_a_to_roll
  JMP next_state
just_pass:
  print #$23, #$42, string_press_a_to_pass
next_state:

  LDA #game_states::end_turn
  STA game_state
  RTS
.endproc

.proc game_state_end_turn
  ; press A to end turn
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ not_passed

  LDX current_player
  LDA extra_turns,X
  BEQ next_player

  ; player has extra turn available
  DEC extra_turns,X

  print #$23, #$42, string_clear_16
  LDA #game_states::dice_rolling
  STA game_state
  JMP not_passed

next_player:
  ; change current player to next player
  INX
  CPX num_players
  BNE not_wrap_around_turn
  LDX #0
not_wrap_around_turn:
  STX current_player
  JSR restore_bridges
  ; if one player has 8 suits, the other players get one extra turn
  LDA blue_shell
  BEQ not_blue_shell
  LDA player_inventory,X
  CMP #$FF
  BEQ not_blue_shell
  INC extra_turns,X

not_blue_shell:
  JSR display_inventory
  print #$23, #$22, string_player_n
  print #$23, #$42, string_press_a_to_roll
  LDA #game_states::player_will_roll
  STA game_state

not_passed:
  RTS
.endproc

.proc game_state_ended
  ; TODO: game over, restart?
  RTS
.endproc

.proc game_state_where_to
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ not_choose

  print #$23, #$22, string_player_n
  print #$23, #$42, string_clear_16

  LDA #game_states::movement
  STA game_state
  LDA #DELAY
  STA delay
  RTS
not_choose:
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ not_toggle
  LDX choice
  LDY alt_choice
  STX alt_choice
  STY choice
  RTS
not_toggle:
  LDA delay
  BEQ move
  DEC delay
  RTS
move:
  LDA #SUB_DELAY
  STA delay
  LDA choice_flick
  BEQ flick
  DEC choice_flick

  LDY current_player
  LDX player_cells,Y
  LDY cell_position,X
  LDX current_player
  JSR move_pip
  RTS
flick:
  INC choice_flick
  LDX choice
  LDY cell_position,X
  LDX current_player
  JSR move_pip
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
; background
.byte $0C, $00, $10, $30
.byte $0C, $06, $16, $26
.byte $0C, $01, $21, $31
.byte $0C, $09, $19, $29
; sprites
.byte $0C, $14, $24, $34
.byte $0C, $06, $16, $26
.byte $0C, $01, $21, $31
.byte $0C, $09, $19, $29

sprites:
;    Y    TILE FLAG       X
; DIE-1
.byte $F0, $00, 3, $F0
.byte $F0, $01, 3, $F8
.byte $F8, $10, 3, $F0
.byte $F8, $11, 3, $F8
; DIE-2
.byte $F0, $02, 3, $F0
.byte $F0, $03, 3, $F8
.byte $F8, $12, 3, $F0
.byte $F8, $13, 3, $F8
; DIE-3
.byte $F0, $04, 3, $F0
.byte $F0, $05, 3, $F8
.byte $F8, $14, 3, $F0
.byte $F8, $15, 3, $F8
; DIE-4
.byte $F0, $06, 3, $F0
.byte $F0, $07, 3, $F8
.byte $F8, $16, 3, $F0
.byte $F8, $17, 3, $F8
; DIE-5
.byte $F0, $08, 3, $F0
.byte $F0, $09, 3, $F8
.byte $F8, $18, 3, $F0
.byte $F8, $19, 3, $F8
; DIE-6
.byte $F0, $0a, 3, $F0
.byte $F0, $0b, 3, $F8
.byte $F8, $1a, 3, $F0
.byte $F8, $1b, 3, $F8
; PIP-1
.byte $F0, $0c, 0, $F0
.byte $F8, $1c, 0, $F0
; PIP-2
.byte $F0, $0c, 1, $F0
.byte $F8, $1c, 1, $F0
; PIP-3
.byte $F0, $0c, 2, $F0
.byte $F8, $1c, 2, $F0
; PIP-4
.byte $F0, $0c, 3, $F0
.byte $F8, $1c, 3, $F0
; PIP-5
.byte $F0, $0d, 0, $F0
.byte $F8, $1d, 0, $F0
; PIP-6
.byte $F0, $0d, 1, $F0
.byte $F8, $1d, 1, $F0
; PIP-7
.byte $F0, $0d, 2, $F0
.byte $F8, $1d, 2, $F0
; PIP-8
.byte $F0, $0d, 3, $F0
.byte $F8, $1d, 3, $F0
; CURSOR
.byte $F0, $0e, 0, $F0

;; RTS Trick for game state handlers
;; must follow the order of game_state enum
game_state_handlers:
  .word game_state_before_start-1
  .word game_state_player_will_roll-1
  .word game_state_dice_rolling-1
  .word game_state_movement-1
  .word game_state_ended-1
  .word game_state_where_to-1
  .word game_state_symbols_setup-1
  .word game_state_end_turn-1


;; Board description
;;
;; On this diagram, each cell has a number; multiple cells can be at the same
;; position, depending on where the player came from.
;; For example, there are 3 cells at x,y=5A: cell 04 when coming from the right
;; (<04), cell 05 when coming from the top (v05) and cell 06 when coming from
;; the left. This will make things easier when going through intersections since
;; each intersection will have only 2 options.
;; For example, from cell 05 the player can only go to cell 07 or cell 02.
;;
;; 9F - bridge 1 - A0 and A1 - bridge 2 - A2 are bridged cells
;;
;; Note: y coordinate is off by one, because pip sprites are drawn almost one row
;;       above the actual cell
;;
;          1             2             3             4             5             6             7             8             9             A             B             C             D             E
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 1 + ^1F <20     + >21 <22     + >23 ^24 <25 + <54 >55     + <52 >53     + <50 >51     + <4E >4F     + <4C >4D     + ^4A >4B     +             +             + ^5D <5E     + >5F <60     + >61 ^62     + 1
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 2 + ^1D v1E     +             + v26 ^27     +             +             +             +             +             + ^47 v48 <49 + >56 <57     + >58 <59     + >5A v5B ^5C +             + v63 ^64     + 2
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 3 + ^1B v1C     +             + v28 <29     + >2A <2B     + >2C <2D     + >2E <2F     + >30 ^31 <32 + >43 <44     + >45 v46     +             +             + <6A v6B     + <67 >68 ^69 + v65 >66     + 3
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 4 + ^19 v1A     +             +             +             +             +             + v33 ^34     +             +             +             +             +             + v6C ^6D     +             + 4
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 5 + ^17 v18     +             +             +             +             +             + v35 ^36     +             +             +             + ^8C <8D     + >8E <8F     + v6E >6F <70 + >71 ^72     + 5
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 6 + ^15 v16 :A0 +             +             +             + <3B ^3C :9F + <39 >3A     + v37 >38     +             +             +             + ^8A v8B     +             +             + v73 ^74     + 6
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 7 + ^13 v14     +             +             +             + v3D ^3E     +             +             +             +             +             + ^88 v89 :A2 +             +             + v75 ^76 :A1 + 7
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 8 + ^11 v12     +             +             +             + v3F ^40     +             + #fila 9B    + #fila 9C    + #fila 9D    + #fila 9E    + ^86 v87     +             +             + v77 ^78     + 8
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 9 + ^0F v10     +             +             +             + v41 ^42     +             + #fila 97    + #fila 98    + #fila 99    + #fila 9A    + ^84 v85     +             +             + v79 ^7A     + 9
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; A + <0D v0E     + <0B >0C     + <09 >0A     + <07 >08     + <04 v05 >06 + <02 >03     + <00 >01     + <94 >95     + <92 >93     + <90 >91     + <81 v82 >83 + <7F >80     + <7D >7E     + v7B >7C     + A
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;          1             2             3             4             5             6             7             8             9             A             B             C             D             E
;                                                                                      [ entrada especial 96 ]

;; Cell positions
;; According to the board description above, this is a list of coordinates for each cell
;; Coordinates are represented as #%yyyyxxxx (y,x)

cell_position:
;       0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
.byte $A7, $A7, $A6, $A6, $A5, $A5, $A5, $A4, $A4, $A3, $A3, $A2, $A2, $A1, $A1, $91 ; 0
.byte $91, $81, $81, $71, $71, $61, $61, $51, $51, $41, $41, $31, $31, $21, $21, $11 ; 1
.byte $11, $12, $12, $13, $13, $13, $23, $23, $33, $33, $34, $34, $35, $35, $36, $36 ; 2
.byte $37, $37, $37, $47, $47, $57, $57, $67, $67, $66, $66, $65, $65, $75, $75, $85 ; 3
.byte $85, $95, $95, $38, $38, $39, $39, $29, $29, $29, $19, $19, $18, $18, $17, $17 ; 4
.byte $16, $16, $15, $15, $14, $14, $2A, $2A, $2B, $2B, $2C, $2C, $2C, $1C, $1C, $1D ; 5
.byte $1D, $1E, $1E, $2E, $2E, $3E, $3E, $3D, $3D, $3D, $3C, $3C, $4D, $4D, $5D, $5D ; 6
.byte $5D, $5E, $5E, $6E, $6E, $7E, $7E, $8E, $8E, $9E, $9E, $AE, $AE, $AD, $AD, $AC ; 7
.byte $AC, $AB, $AB, $AB, $9B, $9B, $8B, $8B, $7B, $7B, $6B, $6B, $5B, $5B, $5C, $5C ; 8
.byte $AA, $AA, $A9, $A9, $A8, $A8, $A7, $97, $98, $99, $9A, $87, $88, $89, $8A, $65 ; 9
.byte $61, $7E, $7B, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ; A
;; Cell target (main)
;; Where the player can go from each cell
;; (or at least the first option if there are two alternatives)

cell_target:
;       0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
.byte $02, $95, $04, $01, $07, $07, $03, $09, $06, $0B, $08, $0D, $0A, $0F, $0C, $11 ; 0
.byte $0E, $13, $10, $15, $12, $17, $14, $19, $16, $1B, $18, $1D, $1A, $1F, $1C, $21 ; 1
.byte $1E, $23, $20, $55, $55, $26, $28, $24, $2A, $27, $2C, $29, $2E, $2B, $30, $2D ; 2
.byte $43, $43, $33, $35, $31, $37, $34, $39, $36, $3B, $38, $3D, $3A, $3F, $3C, $41 ; 3
.byte $3E, $05, $40, $45, $32, $47, $44, $56, $56, $46, $4C, $48, $4E, $4B, $50, $4D ; 4
.byte $52, $4F, $54, $51, $25, $53, $58, $49, $5A, $57, $5D, $6B, $5D, $5F, $5B, $61 ; 5
.byte $5E, $63, $60, $65, $62, $67, $64, $6C, $6C, $6A, $5C, $68, $6E, $69, $71, $71 ; 6
.byte $8F, $73, $70, $75, $72, $77, $74, $79, $76, $7B, $78, $7D, $7A, $7F, $7C, $81 ; 7
.byte $7E, $90, $90, $80, $86, $82, $88, $85, $8A, $87, $8C, $89, $8E, $8B, $6F, $8D ; 8
.byte $92, $83, $94, $91, $00, $93, $02, $96, $96, $96, $96, $96, $96, $96, $96, $3A ; 9
.byte $17, $74, $87, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ; A

;; Cell alternative target (if needed)
cell_alt_target:
;       0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
.byte $00, $00, $00, $00, $42, $03, $42, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 0
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 1
.byte $00, $00, $00, $26, $22, $22, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 2
.byte $33, $2F, $2F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 3
.byte $00, $00, $00, $00, $00, $00, $00, $4A, $46, $4A, $00, $00, $00, $00, $00, $00 ; 4
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6B, $59, $59, $00, $00, $00 ; 5
.byte $00, $00, $00, $00, $00, $00, $00, $6A, $66, $66, $00, $00, $00, $00, $8F, $6D ; 6
.byte $6D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 7
.byte $00, $84, $80, $84, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 8
.byte $00, $00, $00, $00, $00, $00, $95, $00, $00, $00, $00, $00, $00, $00, $00, $3D ; 9
.byte $14, $77, $8A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; A
;; Bridge data
bridge_cells:
.byte $3B, $3C, $15, $16, $75, $76, $88, $89
bridge_targets:
.byte $A0, $A0, $9F, $9F, $A2, $A2, $A1, $A1
bridge_numbers:
.byte $00, $00, $00, $00, $01, $01, $01, $01

string_press_a_to_roll:
.byte $10, $12, $05, $13, $13, $FF, $3D, $FF, $14, $0F, $FF, $12, $0F, $0C, $0C, $00

string_player_n:
.byte $10, $0C, $01, $19, $05, $12, $FF, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00

string_a_to_choose:
.byte $3D, $FF, $14, $0F, $FF, $03, $08, $0F, $0F, $13, $05, $FF, $FF, $FF, $FF, $00

string_b_to_toggle:
.byte $4D, $FF, $14, $0F, $FF, $14, $0F, $07, $07, $0C, $05, $FF, $FF, $FF, $FF, $00

string_you_win:
.byte $19, $0F, $15, $FF, $17, $09, $0E, $2F, $2F, $2F, $FF, $FF, $FF, $FF, $FF, $00

string_clear_16:
.byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00

string_press_a_to_pass:
.byte $10, $12, $05, $13, $13, $FF, $3D, $FF, $14, $0F, $FF, $10, $01, $13, $13, $00

paint_masks:
.byte %11111100, %11110011, %11001111, %00111111

paint_palettes:
.byte %00000000, %00000000, %00000000, %00000000
.byte %00000001, %00000100, %00010000, %01000000
.byte %00000010, %00001000, %00100000, %10000000
.byte %00000011, %00001100, %00110000, %11000000

; low byte of PPU address (high byte is always $23)
inventory_positions:
.byte $18, $19, $38, $39, $1A, $1B, $3A, $3B
inventory_tiles:
.byte $5C, $5D, $6C, $6D, $5C, $5D, $6C, $6D

board_nametable:
.incbin "../assets/board.nam"

.segment "CHR"
.incbin "../assets/graphics.chr"
