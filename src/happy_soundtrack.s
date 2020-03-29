; Generated using Pently music assembler
; Music from happy.pently
; title: Lullaby
; author: Wendel Scardua
; copyright: 
;
PENTLY_NUM_SONGS=2
PENTLY_NUM_SOUNDS=0
.include "pentlyseq.inc"
.segment "RODATA"
pentlyseq_start:
pently_sfx_table:  ; 0 entries, 0 bytes
pently_instruments:  ; 2 entries, 23 bytes
PENTLY_instdef PI_Main, 2, 1, 0, 0, PIDAT_Main, 6
PENTLY_instdef PI_Bass, 2, 3, 0, 0, PIDAT_Bass, 7
PIDAT_Main:
.byte 159,157,155,152,149,147
PIDAT_Bass:
.byte 152,156,153,150,149,149,148
pently_drums:  ; 0 entries, 0 bytes
pently_patterns:  ; 4 entries, 112 bytes
PENTLY_patdef PP_Happy_lullaby_pat_2_0_0, PPDAT_Happy_lullaby_pat_2_0_0
PENTLY_patdef PP_Happy_lullaby_pat_2_1_0, PPDAT_Happy_lullaby_pat_2_1_0
PENTLY_patdef PP_A_winner_is_you_pat_2_0_0, PPDAT_A_winner_is_you_pat_2_0_0
PENTLY_patdef PP_A_winner_is_you_pat_2_1_0, PPDAT_A_winner_is_you_pat_2_1_0
PPDAT_Happy_lullaby_pat_2_0_0:
.byte PENTLY_N_C|PENTLY_D_4,PENTLY_N_C|PENTLY_D_4,PENTLY_N_E|PENTLY_D_4
.byte PENTLY_N_E|PENTLY_D_4,PENTLY_N_C|PENTLY_D_8,PENTLY_N_DS|PENTLY_D_8
.byte PENTLY_N_E|PENTLY_D_8,PENTLY_N_DS|PENTLY_D_8,PENTLY_N_C|PENTLY_D_4
.byte PENTLY_N_C|PENTLY_D_4,PENTLY_N_E|PENTLY_D_4,PENTLY_N_E|PENTLY_D_4
.byte PENTLY_N_G|PENTLY_D_4,PENTLY_N_G|PENTLY_D_4,PENTLY_N_E|PENTLY_D_8
.byte PENTLY_N_F|PENTLY_D_8,PENTLY_N_G|PENTLY_D_8,PENTLY_N_F|PENTLY_D_8
.byte PENTLY_N_E|PENTLY_D_4,PENTLY_N_E|PENTLY_D_4,PENTLY_PATEND
PPDAT_Happy_lullaby_pat_2_1_0:
.byte PENTLY_N_F|PENTLY_D_2,PENTLY_N_A|PENTLY_D_2,PENTLY_N_CH|PENTLY_D_2
.byte PENTLY_N_C|PENTLY_D_2,PENTLY_N_F|PENTLY_D_2,PENTLY_N_A|PENTLY_D_2
.byte PENTLY_N_CH|PENTLY_D_2,PENTLY_N_C|PENTLY_D_2,PENTLY_PATEND
PPDAT_A_winner_is_you_pat_2_0_0:
.byte PENTLY_N_C,PENTLY_N_DS,PENTLY_N_CH,PENTLY_REST,PENTLY_N_C,PENTLY_N_DS
.byte PENTLY_N_CH,PENTLY_REST,PENTLY_N_E,PENTLY_N_FS,PENTLY_N_EH,PENTLY_REST
.byte PENTLY_N_EH,PENTLY_N_DSH,PENTLY_N_CH,PENTLY_REST,PENTLY_N_EH,PENTLY_N_CH
.byte PENTLY_N_G,PENTLY_REST,PENTLY_N_CH,PENTLY_N_EH,PENTLY_N_FS,PENTLY_REST
.byte PENTLY_N_C,PENTLY_N_DS,PENTLY_N_G,PENTLY_N_AS,PENTLY_N_DH,PENTLY_REST
.byte PENTLY_N_C,PENTLY_REST,PENTLY_N_C,PENTLY_N_DS,PENTLY_N_CH,PENTLY_REST
.byte PENTLY_N_C,PENTLY_N_DS,PENTLY_N_CH,PENTLY_REST,PENTLY_N_E,PENTLY_N_FS
.byte PENTLY_N_EH,PENTLY_REST,PENTLY_N_EH,PENTLY_N_DSH,PENTLY_N_CH,PENTLY_REST
.byte PENTLY_N_EH,PENTLY_N_CH,PENTLY_N_G,PENTLY_REST,PENTLY_N_CH,PENTLY_N_EH
.byte PENTLY_N_FS,PENTLY_REST,PENTLY_N_C,PENTLY_N_DS,PENTLY_N_G,PENTLY_N_AS
.byte PENTLY_N_DH,PENTLY_REST,PENTLY_N_C,PENTLY_REST,PENTLY_PATEND
PPDAT_A_winner_is_you_pat_2_1_0:
.byte PENTLY_N_C|PENTLY_D_2,PENTLY_N_C|PENTLY_D_2,PENTLY_N_DS|PENTLY_D_2
.byte PENTLY_N_CH|PENTLY_D_2,PENTLY_N_C|PENTLY_D_2,PENTLY_N_C|PENTLY_D_2
.byte PENTLY_N_DS|PENTLY_D_2,PENTLY_N_CH|PENTLY_D_2,PENTLY_PATEND
pently_songs:  ; 2 entries, 48 bytes
PENTLY_songdef PS_Happy_lullaby, PSDAT_Happy_lullaby
PENTLY_songdef PS_A_winner_is_you, PSDAT_A_winner_is_you
PSDAT_Happy_lullaby:
; title: Happy lullaby
PENTLY_playPatSq1 PP_Happy_lullaby_pat_2_0_0, 27, PI_Main
PENTLY_playPatSq2 PP_Happy_lullaby_pat_2_1_0, 10, PI_Bass
PENTLY_stopPatTri
PENTLY_stopPatNoise
PENTLY_setTempo 300
PENTLY_setBeatDuration PENTLY_D_4
PENTLY_waitRows 64  ; end at 0:12.80
PENTLY_dalSegno
PSDAT_A_winner_is_you:
; title: A winner is you
PENTLY_playPatSq1 PP_A_winner_is_you_pat_2_0_0, 15, PI_Main
PENTLY_playPatSq2 PP_A_winner_is_you_pat_2_1_0, 3, PI_Bass
PENTLY_stopPatTri
PENTLY_stopPatNoise
PENTLY_setTempo 300
PENTLY_setBeatDuration PENTLY_D_4
PENTLY_waitRows 64  ; end at 0:12.80
PENTLY_dalSegno
pentlyseq_end:

; references to subsequences
pently_resume_mute = $00

; Total music data size: 183 bytes
; pently_sfx_table: 0 bytes
; pently_instruments: 23 bytes
;   PI_Main: 11 bytes
;   PI_Bass: 12 bytes
; pently_drums: 0 bytes
; pently_patterns: 112 bytes
;   PP_Happy_lullaby_pat_2_0_0: 23 bytes
;   PP_Happy_lullaby_pat_2_1_0: 11 bytes
;   PP_A_winner_is_you_pat_2_0_0: 67 bytes
;   PP_A_winner_is_you_pat_2_1_0: 11 bytes
; pently_songs: 48 bytes
;   PS_Happy_lullaby: 24 bytes
;   PS_A_winner_is_you: 24 bytes
;
; Breakdown by song
;   Shared: 23 bytes
;   Song A_winner_is_you: 102 bytes
;   Song Happy_lullaby: 58 bytes

; Period table of length 80 for ntsc: 160 bytes
periodTableLo:
.byte $f1,$7f,$13,$ad,$4d,$f3,$9d,$4c,$00,$b8,$74,$34,$f8,$bf,$89,$56,$26,$f9
.byte $ce,$a6,$80,$5c,$3a,$1a,$fb,$df,$c4,$ab,$93,$7c,$67,$52,$3f,$2d,$1c,$0c
.byte $fd,$ef,$e1,$d5,$c9,$bd,$b3,$a9,$9f,$96,$8e,$86,$7e,$77,$70,$6a,$64,$5e
.byte $59,$54,$4f,$4b,$46,$42,$3f,$3b,$38,$34,$31,$2f,$2c,$29,$27,$25,$23,$21
.byte $1f,$1d,$1b,$1a,$18,$17,$15,$14
periodTableHi:
.byte 7,7,7,6,6,5,5,5,5,4,4,4,3,3,3,3,3,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0
; Exports
; Make music data available to Pently
.export pentlyseq_start,pentlyseq_end,pently_sfx_table,pently_instruments
.export pently_drums,pently_patterns,pently_songs

; Sound effect, instrument, and song names for your program to .importzp
.exportzp PENTLY_NUM_SONGS,PENTLY_NUM_SOUNDS,pently_resume_mute,PI_Main,PI_Bass
.exportzp PS_Happy_lullaby,PS_A_winner_is_you
.export periodTableLo, periodTableHi
