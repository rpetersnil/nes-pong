
sfx_table:
    .dw sfx1_data
    .dw sfx2_data
    .dw sfx3_data

sfx1_data:  ; bounce off wall
    .byte C3, $FF     
sfx2_data:  ; hit paddle
    .byte C5, $FF  
sfx3_data:  ; miss ball / score
    .byte D2, D2, $FF 
