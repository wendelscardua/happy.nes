.include "constants.inc"
.include "header.inc"

DICE_ADDR = $0200
PIPS_ADDR = $0260
CURSOR_ADDR = $0280

.zeropage
.import buttons
.import last_frame_buttons
.import released_buttons
.import pressed_buttons

addr_ptr: .res 2
player_position: .res 8
current_player: .res 1

.segment "CODE"

.import reset_handler
.import readjoy

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  JSR readjoy

;; XXX - debug keypress
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ not_pressed_button_A
  INC current_player
  LDA #%11
  AND current_player
  STA current_player
;; XXX - end of debug keypress


not_pressed_button_A:

  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTI
.endproc

.export main
.proc main
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
  CPX #$84        ; size of sprites list
  BNE load_sprites

  JSR reset_players


vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever
.endproc

.proc reset_players
  ; set players position
  ; move pips to initial position
  LDA #0
  STA current_player
  LDX #0
  LDY cell_position
iterate_players:
  STA player_position,X
  JSR move_pip
  INX
  CPX #4
  BNE iterate_players

  RTS
.endproc

.proc move_pip
  ; X = pip, Y=(x,y) position
  ; Move (pip)th pip sprite to (x,y) position (+dx,+dy) based on pip
  ; - preserves X,Y,A
  PHA ; save A
  TYA
  PHA ; save Y
  TXA
  PHA ; save X
  ASL
  ASL
  ASL
  TAX ; X = 8*pip
  TYA
  AND #%11110000 ; A := (x,0)
  STA PIPS_ADDR+3,X
  STA PIPS_ADDR+7,X
  TYA
  ASL
  ASL
  ASL
  ASL ; A := (y,0)
  STA PIPS_ADDR,X
  ORA #%00001000  ; A := (y,8)
  STA PIPS_ADDR+4,X

  TXA
  AND #%00001000 ; A := 8*pip & 8 == (pip&1)*8
  ORA #%00000100 ; A += 4
  TAY
  CLC
  ADC PIPS_ADDR,X
  STA PIPS_ADDR,X
  TYA
  CLC
  ADC PIPS_ADDR+4,X
  STA PIPS_ADDR+4,X

  TXA
  AND #%00010000 ; A := 8 * pip & 16 == (pip & 2) * 8
  LSR ; A := (pip & 2) * 4
  TAY
  CLC
  ADC PIPS_ADDR+3,X
  STA PIPS_ADDR+3,X
  TYA
  CLC
  ADC PIPS_ADDR+7,X
  STA PIPS_ADDR+7,X
  PLA
  TAX ; restore X
  PLA
  TAY ; restore Y
  PLA ; restore A
  RTS
.endproc

.proc move_die
  ; X = die, Y=(x,y) position
  ; Move (die)th die sprite to (x,y) position
  ; - preserves X,Y,A
  PHA ; save A
  TYA
  PHA ; save Y
  TXA
  PHA ; save X
  ASL
  ASL
  ASL
  ASL
  TAX ; X = 16*die
  TYA
  AND #%11110000 ; A := (x,0)
  STA DICE_ADDR+3,X
  STA DICE_ADDR+11,X
  ORA #%00001000 ; A := (x,8)
  STA DICE_ADDR+7,X
  STA DICE_ADDR+15,X
  TYA
  ASL
  ASL
  ASL
  ASL ; A := (y,0)
  STA DICE_ADDR,X
  STA DICE_ADDR+4,X
  ORA #%00001000  ; A := (y,8)
  STA DICE_ADDR+8,X
  STA DICE_ADDR+12,X

  PLA
  TAX ; restore X
  PLA
  TAY ; restore Y
  PLA ; restore A
  RTS
.endproc

.proc hide_die
  ; X = die
  ; Hides die (move to ($F0, $F0))
  ; - preserves X,Y,A
  PHA ; save Y
  LDX #$F0
  LDY #$F0
  JSR move_die
  PLA
  TAY ; restore Y
  RTS
.endproc

.proc write_tiles
  ; addr_ptr - point to string start
  ; X,Y - PPU target (e.g $20, $00 = origin)
  LDA PPUSTATUS
  TXA
  STA PPUADDR
  TYA
  STA PPUADDR
  LDY #0
writing_loop:
  lda (addr_ptr), Y
  BEQ reset_origin
  STA PPUDATA
  INY
  JMP writing_loop
reset_origin:
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
; background
.byte $0F, $00, $10, $30
.byte $0F, $06, $16, $26
.byte $0F, $01, $21, $31
.byte $0F, $09, $19, $29
; sprites
.byte $0F, $14, $24, $34
.byte $0F, $06, $16, $26
.byte $0F, $01, $21, $31
.byte $0F, $09, $19, $29

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
; CURSOR
.byte $F0, $0d, 0, $F0


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
; 5 + ^17 v18     +             +             +             +             +             + v35 ^36     +             +             +             +             + ^8A <8B     + v6E >6F <70 + >71 ^72     + 5
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 6 + ^15 v16     +             +             +             + <3B ^3C     + <39 >3A     + v37 >38     +             +             +             +             + ^88 v89     +             + v73 ^74     + 6
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 7 + ^13 v14     +             +             +             + v3D ^3E     +             +             +             +             +             +             + ^86 v87     +             + v75 ^76     + 7
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 8 + ^11 v12     +             +             +             + v3F ^40     +             +             +             +             +             +             + ^84 v85     +             + v77 ^78     + 8
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 9 + ^0F v10     +             +             +             + v41 ^42     +             +             +             +             +             +             + ^82 v83     +             + v79 ^7A     + 9
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; A + <0D v0E     + <0B >0C     + <09 >0A     + <07 >08     + <04 v05 >06 + <02 >03     + <00 >01     + <92 >93     + <90 >91     + <8E >8F     + <8C >8D     + <7F v80 >81 + <7D >7E     + v7B >7C     + A
;   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;          1             2             3             4             5             6             7             8             9             A             B             C             D             E

;; Cell positions
;; According to the board description above, this is a list of coordinates for each cell
;; Coordinates are represented as #%xxxxyyyy

cell_position:
;       0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
.byte $7A, $7A, $6A, $6A, $5A, $5A, $5A, $4A, $4A, $3A, $3A, $2A, $2A, $1A, $1A, $19 ; 0
.byte $19, $18, $18, $17, $17, $16, $16, $15, $15, $14, $14, $13, $13, $12, $12, $11 ; 1
.byte $11, $21, $21, $31, $31, $31, $32, $32, $33, $33, $43, $43, $53, $53, $63, $63 ; 2
.byte $73, $73, $73, $74, $74, $75, $75, $76, $76, $66, $66, $56, $56, $57, $57, $58 ; 3
.byte $58, $59, $59, $83, $83, $93, $93, $92, $92, $92, $91, $91, $81, $81, $71, $71 ; 4
.byte $61, $61, $51, $51, $41, $41, $A2, $A2, $B2, $B2, $C2, $C2, $C2, $C1, $C1, $D1 ; 5
.byte $D1, $E1, $E1, $E2, $E2, $E3, $E3, $D3, $D3, $D3, $C3, $C3, $D4, $D4, $D5, $D5 ; 6
.byte $D5, $E5, $E5, $E6, $E6, $E7, $E7, $E8, $E8, $E9, $E9, $EA, $EA, $DA, $DA, $CA ; 7
.byte $CA, $CA, $C9, $C9, $C8, $C8, $C7, $C7, $C6, $C6, $C5, $C5, $BA, $BA, $AA, $AA ; 8
.byte $9A, $9A, $8A, $8A  ; 9

;; Cell target (main)
;; Where the player can go from each cell
;; (or at least the first option if there are two alternatives)

cell_target:
;       0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
.byte $02, $93, $04, $01, $07, $07, $03, $09, $06, $0B, $08, $0D, $0A, $0F, $0C, $11 ; 0
.byte $0E, $13, $10, $15, $12, $17, $14, $19, $16, $1B, $18, $1D, $1A, $1F, $1C, $21 ; 1
.byte $1E, $23, $20, $55, $55, $26, $28, $24, $2A, $27, $2C, $29, $2E, $2B, $30, $2D ; 2
.byte $43, $43, $33, $35, $31, $37, $34, $3A, $36, $3B, $38, $3D, $3A, $3F, $3C, $41 ; 3
.byte $3E, $05, $40, $45, $32, $47, $44, $56, $56, $46, $4C, $48, $4E, $4B, $50, $4D ; 4
.byte $52, $4F, $54, $51, $25, $53, $58, $49, $5A, $57, $5D, $6B, $5D, $5F, $5B, $61 ; 5
.byte $5E, $63, $60, $65, $62, $67, $64, $6C, $6C, $6A, $5C, $68, $6E, $69, $71, $71 ; 6
.byte $8B, $73, $70, $75, $72, $77, $74, $79, $76, $7B, $78, $7D, $7A, $7F, $7C, $8C ; 7
.byte $8C, $7E, $84, $80, $86, $83, $88, $85, $8A, $87, $6F, $89, $8E, $81, $90, $8D ; 8
.byte $92, $8F, $00, $91  ; 9

;; Cell alternative target (if needed)
cell_alt_target:
;       0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
.byte $00, $00, $00, $00, $42, $03, $42, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 0
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 1
.byte $00, $00, $00, $26, $22, $22, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 2
.byte $33, $2F, $2F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 3
.byte $00, $00, $00, $00, $00, $00, $00, $4A, $46, $4A, $00, $00, $00, $00, $00, $00 ; 4
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6B, $59, $59, $00, $00, $00 ; 5
.byte $00, $00, $00, $00, $00, $00, $00, $6A, $66, $66, $00, $00, $00, $00, $8B, $6D ; 6
.byte $6D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $82 ; 7
.byte $7E, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 8
.byte $00, $00, $00, $00  ; 9

board_nametable:
.incbin "../assets/board.nam"

.segment "CHR"
.incbin "../assets/graphics.chr"
