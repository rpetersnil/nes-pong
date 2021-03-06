;;;;;;;;
;; Based on sound engine from Nerdy Nights tutorials (https://nerdy-nights.nes.science/)
;;;;;;;;

  .rsset $0300 ;sound engine variables will be on the $0300 page of RAM

sound_disable_flag  .rs 1   ;a flag for whether the sound engine is disabled or not. 
sound_frame_counter .rs 1   ;a primitive counter used to time notes in this demo
sfx_playing .rs 1           ;a flag that tells us if our sound is playing or not.
sfx_index .rs 1             ;our current position in the sound data.

sound_init:
  lda #$0F
  sta SND_MASTERCTRL_REG   ;enable Square 1, Square 2, Triangle and Noise channels

  lda #$30
  sta SND_SQUARE1_REG   ;set Square 1 volume to 0
  sta SND_SQUARE2_REG   ;set Square 2 volume to 0
  sta SND_NOISE_REG   ;set Noise volume to 0
  lda #$80
  sta SND_TRIANGLE_REG   ;silence Triangle

  lda #$00
  sta sound_disable_flag  ;clear disable flag
  sta sfx_playing
  sta sfx_index
  sta sound_frame_counter

  rts

sound_disable:
  lda #$00
  sta SND_MASTERCTRL_REG   ;disable all channels
  lda #$01
  sta sound_disable_flag  ;set disable flag
  rts

sound_load:
  asl a ; multiply by 2, since we are indexing words
  tay 
  lda sfx_table, y
  sta sound_ptr
  lda sfx_table+1, y
  sta sound_ptr + 1

  lda #$01
  sta sfx_playing ;set playing flag
  lda #$00
  sta sfx_index   ;reset the index and counter
  sta sound_frame_counter
  rts

sound_play_frame:
  lda sound_disable_flag
  bne .done   ;if disable flag is set, don't advance a frame

  lda sfx_playing
  beq .done  ;if our sound isn't playing, don't advance a frame

  inc sound_frame_counter     
  lda sound_frame_counter
  cmp #$08    ;***change this compare value to make the notes play faster or slower***
  bne .done   ;only take action once every 8 frames.

  ldy sfx_index
  ;read the next byte from our sound data stream
  lda [sound_ptr], y    ; offset from our current sound effect

  cmp #$FF
  bne .note   ;if not #$FF, we have a note value
  lda #$30    ;else if #$FF, we are at the end of the sound data, so stop the sound and return
  sta SND_SQUARE1_REG
  lda #$00
  sta sfx_playing
  sta sound_frame_counter
  rts
.note:          
  asl a       ;multiply by 2, because our note table is stored as words
  tay         ;we'll use this as an index into the note table

  lda note_table, y   ;read the low byte of our period from the table
  sta SND_REGISTER+2
  lda note_table+1, y ;read the high byte of our period from the table
  sta SND_REGISTER+3
  lda #$7F    ;duty cycle 01, volume F
  sta SND_SQUARE1_REG
  lda #$08    ;set negate flag so low Square notes aren't silenced
  sta SND_REGISTER+1

  inc sfx_index   ;move our index to the next byte position in the data stream
  lda #$00
  sta sound_frame_counter ;reset frame counter so we can start counting to 8 again.    
.done:
  rts

  .include "note_table.i" ;period lookup table for notes
  .include "sound_data.i" ;holds the actual sound effects data

