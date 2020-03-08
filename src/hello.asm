.include "constants.inc"
.include "header.inc"

.zeropage
.import buttons

.segment "CODE"

.import reset_handler
.import readjoy

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  JSR readjoy
  JSR moving_sprite

  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTI
.endproc

.proc moving_sprite
  LDX #$00
move_main_sprite:
  LDA #%00001000
  AND buttons
  BNE move_up
  LDA #%00000100
  AND buttons
  BNE move_down
  JMP move_horiz
move_up:
  DEC $0200,X
  JMP move_horiz
move_down:
  INC $0200,X
move_horiz:
  LDA #%00000010
  AND buttons
  BNE move_left
  LDA #%00000001
  AND buttons
  BNE move_right
  JMP move_else
move_left:
  DEC $0203,X
  JMP move_else
move_right:
  INC $0203,X
move_else:
  INX
  INX
  INX
  INX
  CPX #$10
  BNE move_main_sprite
  RTS
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

  ; write sprite data
  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$10        ; size of sprites list
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
.byte $70, $05, %00000000, $80
.byte $70, $06, %00000000, $88
.byte $78, $07, %00000000, $80
.byte $78, $08, %00000000, $88

.segment "CHR"
.incbin "graphics.chr"
