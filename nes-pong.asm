  .inesprg 2   ; 2x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  


;;;;;;;;;;;;;;;;;;
;; System constants
PPU_CTRL_REG1         = $2000
PPU_CTRL_REG2         = $2001
PPU_STATUS            = $2002
PPU_SPR_ADDR          = $2003
PPU_SPR_DATA          = $2004
PPU_SCROLL_REG        = $2005
PPU_ADDRESS           = $2006
PPU_DATA              = $2007

SND_REGISTER          = $4000
SND_SQUARE1_REG       = $4000
SND_SQUARE2_REG       = $4004
SND_TRIANGLE_REG      = $4008
SND_NOISE_REG         = $400c
SND_DELTA_REG         = $4010
SND_MASTERCTRL_REG    = $4015

SPR_DMA               = $4014
JOYPAD_PORT           = $4016
JOYPAD_PORT1          = $4016
JOYPAD_PORT2          = $4017

A_Button              = %10000000
B_Button              = %01000000
Select_Button         = %00100000
Start_Button          = %00010000
Up_Dir                = %00001000
Down_Dir              = %00000100
Left_Dir              = %00000010
Right_Dir             = %00000001

;;;;;;;;;;;;;;;;;;
;; Game constants
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
  
RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $D8
LEFTWALL       = $04
  
PADDLE1X       = $0F  ; horizontal position for paddles, doesnt move
PADDLE2X       = $E8

MAXSCORE       = $05

BUTTON_A      = 1 << 7
BUTTON_B      = 1 << 6
BUTTON_SELECT = 1 << 5
BUTTON_START  = 1 << 4
BUTTON_UP     = 1 << 3
BUTTON_DOWN   = 1 << 2
BUTTON_LEFT   = 1 << 1
BUTTON_RIGHT  = 1 << 0


;;;;;;;;;;;;;;;;;;
;; Global variables
  .rsset $0000       ; start variables at RAM location 0
bgPointerLo  .rs 1   ; pointer variables are declared in RAM
bgPointerHi  .rs 1   ; low byte first, high byte immediately after
sound_ptr     .rs 2  ; sound engine pointer
gamestate  .rs 1  
ballx      .rs 1  ; ball horizontal position
bally      .rs 1  ; ball vertical position
ballup     .rs 1  ; 1 = ball moving up
balldown   .rs 1  ; 1 = ball moving down
ballleft   .rs 1  ; 1 = ball moving left
ballright  .rs 1  ; 1 = ball moving right
ballspeedx .rs 1  ; ball horizontal speed per frame
ballspeedy .rs 1  ; ball vertical speed per frame
paddlespeed .rs 1 ; ball horizontal speed per frame
paddle1ytop    .rs 1  ; player 1 paddle top vertical position
paddle1ybot    .rs 1  ; player 1 paddle bottom vertical position
paddle2ytop    .rs 1  ; player 2 paddle top vertical position
paddle2ybot    .rs 1  ; player 2 paddle bottom vertical position
numhits        .rs 1  ; number of hits since last score
buttons1       .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2       .rs 1  ; player 2 gamepad buttons, one bit per button
last_frame_buttons1 .rs 1
last_frame_buttons2 .rs 1
released_buttons1   .rs 1
released_buttons2   .rs 1
pressed_buttons1    .rs 1
pressed_buttons2    .rs 1
score1              .rs 1  ; player 1 score, 0-15
score2              .rs 1  ; player 2 score, 0-15
scorer              .rs 1  ; # of player who just scored
binary              .rs 1  ; binary representation of score, for conversion
onesDigit           .rs 1  
tensDigit           .rs 1  
hundredsDigit       .rs 1  
drawScoreOffset     .rs 1
sleeping            .rs 1 ; ensures the main program is run just once per frame


;;;;;;;;;;;;;;;;;;
;----- first 8k bank of PRG-ROM
    .bank 0
    .org $8000  ; We have two 16k PRG banks now.  We will stick our 
                ; sound engine in the first one, which starts at $8000.
    
    .include "sound_engine.asm"

;----- second 8k bank of PRG-ROM    
    .bank 1
    .org $A000
    
;----- third 8k bank of PRG-ROM    
    .bank 2
    .org $C000

RESET:
  SEI			; disable IRQs
  CLD			; disable decimal mode
  ; LDX #$40
  ; STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX PPU_CTRL_REG1    ; disable NMI
  STX PPU_CTRL_REG2    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT PPU_STATUS
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE		  ; Initialize OAM (sprite) buffer to default sprites off screen
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT PPU_STATUS
  BPL vblankwait2

LoadPalettes:
  ; Configure the PPU for writing to the address of the palette (i.e. $3F00)
  LDA PPU_STATUS    ; read PPU status to reset the high/low latch
  LDA #$3F
  STA PPU_ADDRESS    ; write the high byte of $3F00 address
  LDA #$00
  STA PPU_ADDRESS    ; write the low byte of $3F00 address

  ; Write the palette data to the PPU 
  LDX #$00
.loop:
  LDA PaletteData, x      ; load data from address (PaletteData + the value in x)
  STA PPU_DATA               ; write to PPU
  INX                     
  CPX #$20               
  BNE .loop  

LoadBackground:
  LDA PPU_STATUS             ; read PPU status to reset the high/low latch
  LDA #$20
  STA PPU_ADDRESS             ; write the high byte of $2000 address
  LDA #$00
  STA PPU_ADDRESS             ; write the low byte of $2000 address
  LDA #$00
  STA bgPointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(BackgroundData)
  STA bgPointerHi       ; put the high byte of the address into pointer
  LDX #$00            ; start at pointer + 0
  LDY #$00
.outerloop:
.innerloop:
  LDA [bgPointerLo], y  ; copy one background byte from address in pointer plus Y
  STA PPU_DATA           ; this runs 256 * 4 times
  INY                 ; inside loop counter
  CPY #$00
  BNE .innerloop      ; run the inside loop 256 times before continuing down
  INC bgPointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  INX
  CPX #$04
  BNE .outerloop     ; run the outside loop 256 times before continuing down

;; Set initial ball stats
  LDA #$01
  STA balldown
  STA ballright
  LDA #$00
  STA ballup
  STA ballleft
  STA score1
  STA score2
  STA scorer
  STA numhits
  LDA #$50
  STA bally
  LDA #$80
  STA ballx
  LDA #$02
  STA ballspeedx
  STA ballspeedy
;; Set initial paddle positions
  JSR SetPaddleStartPositions
;; Initialize button states
  LDA #$00
  STA buttons1
  STA buttons2
;; Set starting game state
  LDA #STATETITLE
  STA gamestate

;; Enable sound channels
  JSR sound_init

;; Enable NMIs (and sprites)
  LDA #%10010000   ; enable NMI, sprites from Pattern 0, background from Pattern 1
  STA PPU_CTRL_REG1
  LDA #%00011110   ; enable sprites, enable background
  STA PPU_CTRL_REG2

Forever:
  inc sleeping
.loop:
  lda sleeping
;; Wait for NMI to clear the sleeping flag and wake us up
  bne .loop  
;; When NMI wakes us up, handle input, fill drawing buffer, etc and go back to sleep

GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying
  
GameEngineDone:  

;; set ball/paddle sprite positions
  JSR UpdateSprites  

;; get the current button data 
  JSR ReadControllers  

  jmp Forever ; go back to sleep


;;;;;;;;;;;;;;;;;;
;; Audio IRQ
IRQ: 
  RTI


;;;;;;;;;;;;;;;;;;
;; NMI handler
NMI:
;; Save registers
  PHA 
  TXA
  PHA
  TYA
  PHA

;; Configure PPU to load sprites from RAM (at $0200) via DMA
  LDA #$00
  STA PPU_SPR_ADDR  ; set the low byte (00) of the RAM address
  LDA #$02
  STA SPR_DMA  ; set the high byte (02) of the RAM address, start the transfer

;; Run drawing code
  JSR DrawStuff

;; PPU cleanup section, so rendering from next frame starts properly
  LDA #%10010000   ; enable NMI, sprites from Pattern 0, background from Pattern 1
  STA PPU_CTRL_REG1
  LDA #%00011110   ; enable sprites, enable background
  STA PPU_CTRL_REG2
  ; tell the PPU that we are not doing any scrolling at the end of NMI
  LDA #$00
  STA PPU_SCROLL_REG
  sta PPU_SCROLL_REG

;; 
;; all graphics updates are complete by here
;; 

;; Run our sound engine after all drawing is done. This ensures it is run once per frame.
  JSR sound_play_frame
  
;; Wake up the main program
  LDA #$00
  STA sleeping            

;; Restore registers
  PLA     
  TAY
  PLA
  TAX
  PLA
  RTI

 
;;;;;;;;;;;;;;;;;;
;; Game engine
EngineTitle:
;; Reset the scores
  LDA #$00
  STA score1
  STA score2

;; Reset the paddle positions
  JSR SetPaddleStartPositions

;; If start button pressed, go to playing
  LDA pressed_buttons1
  AND #BUTTON_START
  BEQ EngineTitleEnd
  LDA #STATEPLAYING
  STA gamestate

EngineTitleEnd:
  JMP GameEngineDone

 
EngineGameOver:
;; If start button pressed, go to title
  LDA pressed_buttons1
  AND #BUTTON_START
  BEQ EngineGameOverEnd
  LDA #STATETITLE
  STA gamestate

EngineGameOverEnd:
  JMP GameEngineDone

 
EnginePlaying:

MoveBallRight:
  LDA ballright
  BEQ MoveBallRightDone

  LDA ballx
  CLC
  ADC ballspeedx        ;;ballx position = ballx + ballspeedx
  STA ballx

  LDA ballx
  CMP #RIGHTWALL
  BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section

;; Give point to player 1, reset ball
  LDA #$01
  STA scorer
  JSR HandlePointScored

MoveBallRightDone:
MoveBallLeft:
  LDA ballleft
  BEQ MoveBallLeftDone   ;;if ballleft=0, skip this section

  LDA ballx
  SEC
  SBC ballspeedx        ;;ballx position = ballx - ballspeedx
  STA ballx

  LDA ballx
  CMP #LEFTWALL
  BCS MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section

;; Give point to player 2, reset ball
  LDA #$02
  STA scorer
  JSR HandlePointScored

MoveBallLeftDone:
MoveBallUp:
  LDA ballup
  BEQ MoveBallUpDone   ;;if ballup=0, skip this section

  LDA bally
  SEC
  SBC ballspeedy        ;;bally position = bally - ballspeedy
  STA bally

  LDA bally
  CMP #TOPWALL
  BCS MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
  LDA #$01
  STA balldown
  LDA #$00
  STA ballup         ;;bounce, ball now moving down
;; Play wall bounce sound
  lda #$00
  JSR sound_load

MoveBallUpDone:
MoveBallDown:
  LDA balldown
  BEQ MoveBallDownDone   ;;if ballup=0, skip this section

  LDA bally
  CLC
  ADC ballspeedy        ;;bally position = bally + ballspeedy
  STA bally

  LDA bally
  CMP #BOTTOMWALL
  BCC MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
  LDA #$00
  STA balldown
  LDA #$01
  STA ballup         ;;bounce, ball now moving down
;; Play wall bounce sound
  lda #$00
  JSR sound_load

MoveBallDownDone:
MovePaddleUp:
  LDA buttons1
  AND #Up_Dir
  BEQ MovePaddle1UpDone   ; branch if button is NOT pressed (0)

  LDA paddle1ytop
  CMP #TOPWALL
  BCC MovePaddle1UpDone    ;;if paddletop y < top wall, too high, don't move

  LDA paddle1ytop
  SEC
  SBC paddlespeed
  STA paddle1ytop
  LDA paddle1ybot
  SEC
  SBC paddlespeed
  STA paddle1ybot

MovePaddle1UpDone:
  LDA buttons2
  AND #Up_Dir
  BEQ MovePaddle2UpDone   ; branch if button is NOT pressed (0)

  LDA paddle2ytop
  CMP #TOPWALL
  BCC MovePaddle2UpDone    ;;if paddletop y < top wall, too high, don't move

  LDA paddle2ytop
  SEC
  SBC paddlespeed
  STA paddle2ytop
  LDA paddle2ybot
  SEC
  SBC paddlespeed
  STA paddle2ybot

MovePaddle2UpDone:
MovePaddleDown:
  LDA buttons1
  AND #Down_Dir
  BEQ MovePaddle1DownDone    ;;branch if button is NOT pressed (0)

  LDA paddle1ybot
  CMP #BOTTOMWALL
  BCS MovePaddle1DownDone    ;;if paddlebottom y > bottom wall, too low, don't move

  LDA paddle1ytop
  CLC
  ADC paddlespeed
  STA paddle1ytop
  LDA paddle1ybot
  CLC
  ADC paddlespeed
  STA paddle1ybot
  
MovePaddle1DownDone:

  LDA buttons2
  AND #Down_Dir
  BEQ MovePaddle2DownDone   ;;branch if button is NOT pressed (0)

  LDA paddle2ybot
  CMP #BOTTOMWALL
  BCS MovePaddle2DownDone    ;;if paddlebottom y > bottom wall, too low, don't move

  LDA paddle2ytop
  CLC
  ADC paddlespeed
  STA paddle2ytop
  LDA paddle2ybot
  CLC
  ADC paddlespeed
  STA paddle2ybot
  
MovePaddle2DownDone:
CheckPaddleCollision:
  LDA ballleft
  BEQ CheckPaddle1CollisionDone

;; Check for collision with paddle 1
  LDA ballx
  SEC
  SBC #$08
  CMP #PADDLE1X
  BCS CheckPaddle1CollisionDone

  LDA bally
  CMP paddle1ytop
  BCC CheckPaddle1CollisionDone

  LDA bally
  SEC
  SBC #$08
  CMP paddle1ybot
  BCS CheckPaddle1CollisionDone


  LDA #$01
  STA ballright
  CLC
  ADC numhits
  STA numhits
  LDA #$00
  STA ballleft         ;;bounce, ball now moving right
;; Play paddle bounce sound
  lda #$01
  JSR sound_load

CheckPaddle1CollisionDone:

  LDA ballright
  BEQ CheckPaddle2CollisionDone 

  LDA ballx
  CLC
  ADC #$08
  CMP #PADDLE2X
  BCC CheckPaddle2CollisionDone

  LDA bally
  CMP paddle2ytop
  BCC CheckPaddle2CollisionDone

  LDA bally
  SEC
  SBC #$08
  CMP paddle2ybot
  BCS CheckPaddle2CollisionDone

  LDA #$00
  STA ballright
  LDA #$01
  STA ballleft         ;;bounce, ball now moving left
  CLC
  ADC numhits
  STA numhits
;; Play paddle bounce sound
  lda #$01
  JSR sound_load

CheckPaddle2CollisionDone:
;; Update ball speed if rally has gone on long enough
  lda numhits
  CMP #$04
  BCC .done
  lda ballspeedx
  CLC
  ADC #$01
  STA ballspeedx
  LDA #$00
  STA numhits
.done:
  JMP GameEngineDone
 

;;;;;;;;;;;;;;;;;;
;; Utility functions
UpdateSprites:
;; Update all ball sprite info
  LDA bally
  STA $0200
  LDA #$75
  STA $0201
  LDA #%00000000
  STA $0202
  LDA ballx
  STA $0203
  
;; Update paddle sprites
  LDA paddle1ytop
  STA $0204
  LDA #$87
  STA $0205
  LDA #%00000001
  STA $0206
  LDA #PADDLE1X
  STA $0207
 
  LDA paddle1ybot
  STA $0208
  LDA #$87
  STA $0209
  LDA #%10000001
  STA $020A
  LDA #PADDLE1X
  STA $020B
 
  LDA paddle2ytop
  STA $020C
  LDA #$87
  STA $020D
  LDA #%01000001
  STA $020E
  LDA #PADDLE2X
  STA $020F
 
  LDA paddle2ybot
  STA $0210
  LDA #$87
  STA $0211
  LDA #%11000001
  STA $0212
  LDA #PADDLE2X
  STA $0213
 
  RTS



;; Text format: HIGH(bg location), LOW(bg location), letter1, letter2, ..., $ff
GameText:
GameTitleText:
  .db $21, $EA, ('R' - 55), ('E' - 55), ('A' - 55), ('D' - 55), ('Y' - 55), ('[' - 55), ('P' - 55), ('L' - 55), ('A' - 55), ('Y' - 55), ('E' - 55), ('R' - 55), ('S' - 55), $ff
GameOverText:
  .db $21, $EC, ('G' - 55), ('A' - 55), ('M' - 55), ('E' - 55), ('[' - 55), ('O' - 55), ('V' - 55), ('E' - 55), ('R' - 55), $ff
GameClearText:
  .db $21, $EA, ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), ('[' - 55), $ff

DrawTextMessage:
  LDA gamestate
  CMP #STATETITLE
  BEQ DrawTitle
  CMP #STATEPLAYING
  BEQ ClearText
DrawGameOver:
  LDY #(GameOverText-GameText)
  JMP DrawMessage
DrawTitle:
  LDY #(GameTitleText-GameText)
  JMP DrawMessage
ClearText:
  LDY #(GameClearText-GameText)
DrawMessage:
  LDA PPU_STATUS
  LDA GameText, y
  INY
  STA PPU_ADDRESS
  LDA GameText, y
  INY
  STA PPU_ADDRESS
.loop:
  LDA GameText, y
  CMP #$ff
  BEQ DrawTextEnd
  STA PPU_DATA
  INY
  BNE .loop
DrawTextEnd:
  RTS
  
 
DrawStuff:
  JSR DrawScore
  JSR DrawTextMessage
  RTS

DrawScore:
  LDX #$02
DrawScoreLoop:
  CPX #$00
  BEQ DrawScoreEnd
  CPX #$01
  BEQ DrawScore1
DrawScore2:
  LDA #$5C
  STA drawScoreOffset
  LDA score2
  JMP DrawScoreMain
DrawScore1:
  LDA #$41
  STA drawScoreOffset
  LDA score1
DrawScoreMain:
  STA binary
  JSR BinaryToDecimal

  LDA PPU_STATUS             ; read PPU status to reset the high/low latch
  LDA #$20
  STA PPU_ADDRESS             ; write the high byte of $2000 address
  LDA drawScoreOffset
  STA PPU_ADDRESS             ; write the low byte of $2000 address

  LDA hundredsDigit
  STA PPU_DATA
  LDA tensDigit
  STA PPU_DATA
  LDA onesDigit
  STA PPU_DATA
  DEX
  JMP DrawScoreLoop
DrawScoreEnd:
  RTS
 
 
BinaryToDecimal:
HundredsLoop:
  LDA binary
  CMP #100             ; compare binary to 100
  BCC TensLoop         ; if binary < 100, all done with hundreds digit
  LDA binary
  SEC
  SBC #100
  STA binary           ; subtract 100, store whats left
  INC hundredsDigit    ; increment the digital result
  JMP HundredsLoop     ; run the hundreds loop again
TensLoop:
  LDA binary
  CMP #10              ; compare binary to 10
  BCC OnesLoop         ; if binary < 10, all done with hundreds digit
  LDA binary
  SEC
  SBC #10
  STA binary           ; subtract 10, store whats left
  INC tensDigit        ; increment the digital result
  JMP TensLoop         ; run the tens loop again
OnesLoop:
  LDA binary
  STA onesDigit        ; result is already under 10, can copy directly to result
  RTS


;; Bit:       7     6     5     4     3     2     1     0
;; Button:    A     B   select start  up   down  left right
ReadControllers:
;; Store last frame's buttons
  LDA buttons1
  STA last_frame_buttons1
  LDA buttons2
  STA last_frame_buttons2
;; Configure controllers for reading
  LDA #$01
  STA JOYPAD_PORT
  LDA #$00
  STA JOYPAD_PORT
;; Set up loop counter
  LDX #$08
ReadControllersLoop:
;; Read controller 1
  LDA JOYPAD_PORT1
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
;; Read controller 2
  LDA JOYPAD_PORT2
  LSR A            ; bit0 -> Carry
  ROL buttons2     ; bit0 <- Carry
  DEX
  BNE ReadControllersLoop
;; Calculate new presses and releases
  JSR CalcPressesAndReleases
  RTS


CalcPressesAndReleases:
  LDX #$00
.loop
  LDA buttons1, x
  EOR #$FF
  AND last_frame_buttons1, x
  STA released_buttons1, x
  LDA last_frame_buttons1, x
  EOR #$FF
  AND buttons1, x
  STA pressed_buttons1, x
  INX
  CPX #02
  BNE .loop
  RTS


SetPaddleStartPositions:
  LDA #$03
  STA paddlespeed
  LDA #$74
  STA paddle1ytop
  STA paddle2ytop
  LDA #$7C
  STA paddle1ybot
  STA paddle2ybot
  RTS


HandlePointScored:
PlayScoreSound:
  lda #$02
  JSR sound_load
CheckScorer:
  LDA #02
  CMP scorer
  BEQ UpdateScore2
UpdateScore1:
  LDA score1
  CLC
  ADC #$01
  STA score1
  LDA #$00
  STA ballleft
  LDA #$01
  STA ballright
  JMP ScoringDone
UpdateScore2:
  LDA score2
  CLC
  ADC #$01
  STA score2
  LDA #$00
  STA ballright
  LDA #$01
  STA ballleft
ScoringDone: 
;; Reset ball location and speed
  LDA #$50
  STA bally
  LDA #$80
  STA ballx
  LDA #$02
  STA ballspeedx
;; Reset hit tracker
  LDA #$00
  STA numhits
;; Handle end of game
  LDA #MAXSCORE
  CMP score1
  BEQ EndGame
  LDA #MAXSCORE
  CMP score2
  BEQ EndGame
  JMP EOGDone
EndGame:
  LDA #STATEGAMEOVER
  STA gamestate 
EOGDone:
  RTS


;;;;;;;;;;;;;;;
;; Data
  .bank 3
  .org $E000

BackgroundData:
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 1
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;all brick tops

  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;all brick bottoms

  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;all brick bottoms

  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 2
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;all brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 14
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 15
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;row 29
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;;all brick tops

  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;row 30
  .db $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  ;;all brick bottoms


AttributeData:  ;8 x 8 = 64 bytes
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111


PaletteData:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F ; background
  .db $22,$1C,$0F,$30,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C ; sprite palette


;;;;;;;;;;;;;;;;;;
;; Vector setup
  .org $FFFA     ; first of the three vectors starts here
  .dw NMI        ; when an NMI happens (once per frame if enabled) the
                 ; processor will jump to the label NMI:
  .dw RESET      ; when the processor first turns on or is reset, it will jump
                 ; to the label RESET:
  .dw IRQ        ;external interrupt IRQ 


;;;;;;;;;;;;;;;;;;
;; CHR data
  .bank 4
  .org $0000
  .incbin "mario.chr"

