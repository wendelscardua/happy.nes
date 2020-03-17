.zeropage
rng_seed: .res 2

.export rng_seed

.segment "CODE"

.export rand
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
.endproc
