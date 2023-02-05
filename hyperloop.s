; PPU control register
; 7  bit  0
; ---- ----
; VPHB SINN
; |||| ||||
; |||| ||++- Base nametable address
; |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
; |||| |+--- VRAM address increment per CPU read/write of PPUDATA
; |||| |     (0: add 1, going across; 1: add 32, going down)
; |||| +---- Sprite pattern table address for 8x8 sprites
; ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
; |||+------ Background pattern table address (0: $0000; 1: $1000)
; ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
; |+-------- PPU master/slave select
; |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
; +--------- Generate an NMI at the start of the
;            vertical blanking interval (0: off; 1: on)
PPUCTRL   = $2000

; PPU Mask register: controls the rendering of sprites
; 7  bit  0
; ---- ----
; BGRs bMmG
; |||| ||||
; |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
; |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
; |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
; |||| +---- 1: Show background
; |||+------ 1: Show sprites
; ||+------- Emphasize red (green on PAL/Dendy)
; |+-------- Emphasize green (red on PAL/Dendy)
; +--------- Emphasize blue
PPUMASK   = $2001

; PPU Status register: reflects the state of various functions inside the PPU
; 7  bit  0
; ---- ----
; VSO. ....
; |||| ||||
; |||+-++++- PPU open bus. Returns stale PPU bus contents.
; ||+------- Sprite overflow. The intent was for this flag to be set
; ||         whenever more than eight sprites appear on a scanline, but a
; ||         hardware bug causes the actual behavior to be more complicated
; ||         and generate false positives as well as false negatives; see
; ||         PPU sprite evaluation. This flag is set during sprite
; ||         evaluation and cleared at dot 1 (the second dot) of the
; ||         pre-render line.
; |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
; |          a nonzero background pixel; cleared at dot 1 of the pre-render
; |          line.  Used for raster timing.
; +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
;            Set at dot 1 of line 241 (the line *after* the post-render
;            line); cleared after reading $2002 and at dot 1 of the
;            pre-render line.
PPUSTATUS = $2002

; Object Attribute Memory address
;   : write the address of OAM to be accessed here.
OAMADDR   = $2003

; PPU scrolling position register
;   : used to change the scroll position
PPUSCROLL = $2005

; PPU address register
PPUADDR   = $2006

; PPU data port register
;   : VRAM read/write data register
PPUDATA   = $2007

; OAM data register
OAMDMA    = $4014

.segment "HEADER"
.byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks
.byte %00000001  ; Vertical mirroring, no save RAM, no mapper
.byte %00000000  ; No special-case flags set, no mapper
.byte $00        ; No PRG-RAM present
.byte $00        ; NTSC format

.segment "ZEROPAGE"
starship_x: .res 1  ; reserve 1 byte of memory
starship_y: .res 1

.segment "STARTUP"
.segment "CODE"
.proc irq_handler
    rti
.endproc

;;; NMI handler - ---------------------------------------------------------+
.proc nmi_handler
    lda #$02
    sta OAMDMA
    lda #$00
    sta OAMADDR

    jsr update_starship
    jsr draw_starship

    lda #$00
    sta $2005
    sta $2005
    rti
.endproc
;;; -----------------------------------------------------------------------+

;;; Reset handler - -------------------------------------------------------+
.proc reset_handler
    sei
    cld
    ldx #$00
    stx PPUCTRL
    stx PPUMASK
vblankwait:
    bit PPUSTATUS
    bpl vblankwait

    ; Initialize zeropage values here
    lda #$a0
    sta starship_y
    lda #$44
    sta starship_x
.endproc

;;; -----------------------------------------------------------------------+

;;; main - load palettes, enable NMIs and turn on the screen --------------+
.proc main
    ;; Prepare PPU to load a palette
    ldx PPUSTATUS
    ldx #$3f
    stx PPUADDR
    ldx #$00
    stx PPUADDR

    ;; Load all 8 4-color palettes into PPU
    ldx #$00
LoadPalette:
    lda PaletteData, x
    sta PPUDATA
    inx
    cpx #$20
    bne LoadPalette

vblankwait:       ; wait for another vblank before continuing
    bit PPUSTATUS
    bpl vblankwait

    lda #%10010000  ; turn on NMIs, sprites use first pattern table
    sta PPUCTRL
    lda #%00011110  ; turn on screen
    sta PPUMASK

forever:
    jmp forever
.endproc
;;; +----------------------------------------------------------------------+

;;; update_starship - update position of starship for movement ------------+
.proc update_starship
    ; save registers
    php
    pha
    txa
    pha
    tya
    pha

Done:
    ; restore registers
    pla
    tay
    pla
    tax
    pla
    plp
    rts
.endproc
;;; -----------------------------------------------------------------------+

;;; draw_starship - to render the starship sprite at starship_y, starship_x+
.proc draw_starship
    ; save registers
    php
    pha
    txa
    pha
    tya
    pha

    ; loop through the sprite data and render
    ldx #$00
LoadStarshipLoop:
    lda StarshipSpriteData, x
    sta $0200, x
    inx
    cpx #$24
    bne LoadStarshipLoop

    ; restore registers
    pla
    tay
    pla
    tax
    pla
    plp

    rts
.endproc
;;; +----------------------------------------------------------------------+

;;; Palette Data goes here ------------------------------------------------+
PaletteData:
    .byte $00, $00, $00, $00,   $00, $00, $00, $00,     $00, $00, $00, $00,  	$00, $00, $00, $00  ;background palettes
    .byte $0F, $20, $10, $13, 	$00, $00, $00, $00,     $00, $00, $00, $00,  	$00, $00, $00, $00 	;sprite palettes
;;; -----------------------------------------------------------------------+

;;; Sprite Data goes here -------------------------------------------------+
StarshipSpriteData: ; row, tile number, attribute, col
    .byte $40, $00, $00, $40
    .byte $40, $01, $00, $48
    .byte $40, $02, $00, $4c
    .byte $48, $10, $00, $40
    .byte $48, $11, $00, $48
    .byte $48, $12, $00, $4c
    .byte $5c, $20, $00, $40
    .byte $5c, $21, $00, $48
    .byte $5c, $22, $00, $4c
;;; -----------------------------------------------------------------------+

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHARS"
.incbin "starship.chr"