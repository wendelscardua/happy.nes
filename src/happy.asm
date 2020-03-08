.include "constants.inc"
.include "header.inc"

DICE_ADDR = $0200
PIPS_ADDR = $0260
CURSOR_ADDR = $0280

.zeropage
.import buttons
addr_ptr: .res 2

.segment "CODE"

.import reset_handler
.import readjoy

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  JSR readjoy

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
  STX PPUADDR
  LDA #$00
  STX PPUADDR

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

board_nametable:
.incbin "../assets/board.nam"

.segment "CHR"
.incbin "../assets/graphics.chr"
