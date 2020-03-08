.include "constants.inc"
.include "header.inc"

DICE_ADDR = $0200

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
  CPX #$60        ; size of sprites list
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
.byte $0F, $00, $10, $30
.byte $0F, $06, $16, $26
.byte $0F, $01, $21, $31
.byte $0F, $09, $19, $29

sprites:
;;    Y    TILE FLAG       X
.byte $F0, $00, 1, $F0
.byte $F0, $01, 1, $F8
.byte $F8, $10, 1, $F0
.byte $F8, $11, 1, $F8

.byte $F0, $02, 1, $F0
.byte $F0, $03, 1, $F8
.byte $F8, $12, 1, $F0
.byte $F8, $13, 1, $F8

.byte $F0, $04, 1, $F0
.byte $F0, $05, 1, $F8
.byte $F8, $14, 1, $F0
.byte $F8, $15, 1, $F8

.byte $F0, $06, 1, $F0
.byte $F0, $07, 1, $F8
.byte $F8, $16, 1, $F0
.byte $F8, $17, 1, $F8

.byte $F0, $08, 1, $F0
.byte $F0, $09, 1, $F8
.byte $F8, $18, 1, $F0
.byte $F8, $19, 1, $F8

.byte $F0, $0a, 1, $F0
.byte $F0, $0b, 1, $F8
.byte $F8, $1a, 1, $F0
.byte $F8, $1b, 1, $F8

board_nametable:
.incbin "../assets/board.nam"

.segment "CHR"
.incbin "../assets/graphics.chr"
