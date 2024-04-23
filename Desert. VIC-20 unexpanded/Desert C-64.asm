; Desert
; A text grid adventure for unexpanded VIC-20
; Commodore 64 version
; by Epic Adventures
;
;
*=$0801

        BYTE    $0B, $08, $0A, $00, $9E, $32, $30, $36, $31, $00
        BYTE    $00, $00

; Subrutines
PRTSTR = $AB1E
CHROUT = $ffd2 
CHIN = $FFCF 
CLRS = $E544 ; CLR Screen
PRTFIX = $BDCD ; Convert and pritns and integer
 
; Constanst
RETURN = $0D
SPACE = $20 ; Space


;--- Main --------------------------

Game_Begin
        ldx #$ff
        txs
        
        ; Init variables
        jsr Init
        ; show intructions of the game
        jsr Instructions
        
LocLoop
        jsr CLRS
        ; TODO check if you arrived
        jsr PrintStats
        jsr PrintLOC
        jsr Jump_LOC

MainLoop
        jsr InputCommand ; get command
        jsr EvalCommand
        jmp MainLoop
;        rts 

;---------------------------------------
Init    


        ; Initial pos
        lda #$3
        sta player_x
        lda #$3
        sta player_y

        ; Life
        lda #INITIAL_LIFE
        sta life

        ; flags to zero
        lda #$0
        sta flags 
        sta flags+1


Set_Initial_Water
        lda #INITIAL_WATER
        sta water

        ; Init flags

        rts


;----
Init_Sound_Color
        ; Backgroud yellow
        ; Border yellow
        lda #$7f
        ; Changes color background 
        ; and border
        sta $900f


        ; Set volume for sound effects
        ldx #$15
        stx $900e

        rts

;---------------------------------------
Instructions
        jsr CLRS
        lda #<str_intro
        ldy #>str_intro
        jsr PRTSTR
        jsr SR_Press_Return
        rts


;-----------------------------
PrintStats
        jsr PrintWater
        jsr PrintLife
        jsr CR
        rts


PrintWater
        lda #<str_water
        ldy #>str_water
        jsr PRTSTR

        lda #$0
        ldx water

        jsr PRTFIX 
        
        jsr CR
        rts

PrintLife
        lda #<str_life
        ldy #>str_life
        jsr PRTSTR

        lda #$0
        ldx life

        jsr PRTFIX 
        
        jsr CR
        rts

;--------------------------
; Wrong name for a subroutine
; This one prints nothing
; instead, it calculate the content of the LOC
; and stores it current_loc_type
; It is very usefull
PrintLOC
        ; check if player is in the temple
        lda player_x
        cmp #$10 ; This number is not in map
        bne @Not_in_Temple

        jsr Loc_Temple
        jmp MainLoop

@Not_in_Temple
        ; Calculate LOC number from x and y
        jsr Return_LOC_in_X

        ; I use 4 bits for LOC content
        ; So I dicie LOC / 2
        txa
        tay
        lsr

        ; Load LOC content
        tax
        lda row00,x
        tax

        ; Load again LOC number to see
        ; if it is pair or odd
        ; To see whcih part of the byte use
        tya
        and #%00000001
        bne @LOC_is_odd ; odd
        ; par
        txa
        lsr
        lsr
        lsr
        lsr

        jmp @Exit

@LOC_is_odd
        txa
        and #%00001111 

@Exit
        ; Saves current LOC type
        sta current_loc_type 
        rts

; Desert does not use this subroutine
@Print_LOC
        pha
        lda #<str_you_are
        ldy #>str_you_are
        jsr PRTSTR
        pla

        asl
        tax
        txa
        ;lda v_descriptions,x
        ;ldy v_descriptions,x+1
        jsr PRTSTR
        jsr CR
        
        ; Saltar a la rutina de la loc
        rts

;--------------
Jump_LOC
        ; Previous subroutine has to store in a
        ; the number of the LOC

        ;jsr Return_LOC_in_X
        ;txa

        lda current_loc_type
        asl
        tax
        lda v_locs,x
        sta tmp_a
        lda v_locs,x+1
        sta tmp_b
        jmp (tmp_a)


;---------------------------------------

InputCommand
        ; Read the keyboard
        ; and try to identify the verb and the name_index
        ; It expects VERB or VERB NAME

        jsr PrintPromt
        jsr Input

        jsr ProcessInput
        jsr TokenizeVerb
        ; jsr TokenizeName
        rts

;---------------
PrintPromt
        lda #RETURN
        jsr CHROUT

        ; Print '> '
        lda #<str_prompt
        ldy #>str_prompt
        jsr PRTSTR
        rts

;---------------
Input
        ; Stores 0s in the address where the
        ; code will store user's input
        LDX #0
        stx verb+1 ; Limpiar el buffer

LI
        JSR CHIN
        STA INPUT_BUFFER,X ; $200
        INX
        CMP #RETURN ; Cambiar por cte
        BNE LI
        rts

;---------------
ProcessInput
        ; Takes two characters for verb
N_C     lda INPUT_BUFFER
        sta verb
        cmp #RETURN
        beq @End
        lda INPUT_BUFFER+$1
        cmp #RETURN
        beq @End
        sta verb+$1

@End    
        ; Carriage return
        jsr CR  
        rts


;---------------
TokenizeVerb
; Token has to be in $Token cvector
; This subroutine returns the index of the token in a
; First verb is index 0; a verb_index = $FF means not found

        ldy #$FF ; This vaklue means no valid verb
        sty verb_index
        iny

@F_Loop    
        lda verb_tokens,y
        beq @F_Exit ; N
        cmp verb
        bne @Next1

        iny
        lda verb_tokens,y
        cmp verb+$1
        bne @Next2

        lda verb+$1

        tya
        lsr
        sta verb_index

@F_Exit  
        ; a = 0 not found means, other found  
        rts

@Next1   
        iny
@Next2   
        iny
        jmp @F_Loop


;---------------------------------------------
EvalCommand
        ; TODO Si no hay token, poner coando erroneo
        jsr CheckVerb
        ;jsr CheckVerbNeedsName
        jsr ExecuteCommand
        rts

;---------------
CheckVerb
        lda verb_index
        cmp #$ff ; verb not found
        bne @Exit
        lda #<str_invalid_verb
        ldy #>str_invalid_verb
        jsr PRTSTR
        jmp MainLoop
@Exit 
        rts


;-------------
ExecuteCommand
        ;lda verb_index ; already loaded
        lda player_x
        ; sta mem_mon
        cmp #$10
        bne @Jump_to_Cmd
        ; Estoy en el templo
        lda verb_index
        sec
        sbc #$4
        ; sta mem_mon+1
        bcs @Jump_to_Cmd
        ; Es verbode movimiento.
        ; Salgo del templo
        lda #TEMPLE_X
        sta player_x
        lda #TEMPLE_Y
        sta player_y
        jmp LocLoop

@Jump_to_Cmd      
        lda verb_index
        asl
        tax
        lda verb_pointers,x
        sta tmp_a
        lda verb_pointers,x+1
        sta tmp_b
        jmp (tmp_a)
        

;--- Moveent -----------------------------

North_Cmd
        ; Y has reached de limit
        ldx player_y
        cpx #$f
        beq PR_No_Exit

        inx
        stx player_y

        jmp Reduce_Water_Life

South_Cmd
        ; Y has reached de limit
        ldx player_y
        ; Cero
        beq PR_No_Exit

        dex
        stx player_y

        jmp Reduce_Water_Life

East_Cmd
        ; X has reached de limit
        ldx player_x
        cpx #$f
        beq PR_No_Exit

        inx
        stx player_x

        jmp Reduce_Water_Life
        
West_Cmd
        ; Y has reached de limit
        ldx player_x
        ; Cero
        beq PR_No_Exit

        dex
        stx player_x

        jmp Reduce_Water_Life


PR_No_Exit
        lda #<str_no_exit
        ldy #>str_no_exit
        jsr PRTSTR
        rts


;----------
Reduce_Water_Life
        ; Call this sunrotuine after moveent
        ldx water
        beq @Reduce_Life
        dex
        stx water
        jmp @Exit

@Reduce_Life
        lda life
        sec
        sbc #$1
        bcc @JMP_Player_Is_Dead
        sta life
        
@Exit
        ; Clear oveemnt flag
        ldy #F_MOVEMENT
        jsr clear_flag_y

        jmp LocLoop

@JMP_Player_Is_Dead
        jmp Player_Is_Dead


;--- Oasis commands ---------------------

Drink_Cmd
        lda current_loc_type
        cmp #OASIS_LOC
        bne @Check_Flask

        ;jsr Print_Ok
        jmp @Drink

@Check_Flask
        ldy #F_FLASK
        jsr read_flag_y
        beq JMP_Print_You_Cannot

        ldy #F_FILL_FLASK
        jsr read_flag_y
        beq JMP_Print_You_Cannot


        ldy #F_FILL_FLASK
        jsr clear_flag_y
        ;jsr Print_Ok

@Drink
        jsr Set_Initial_Water
        JSR PrintWater
        rts

JMP_Print_You_Cannot
        jmp Print_You_Cannot

;---
Fill_Cmd
        lda current_loc_type
        cmp #OASIS_LOC
        bne JMP_Print_You_Cannot

        ldy #F_FLASK
        jsr read_flag_y
        beq JMP_Print_You_Cannot

        ldy #F_FILL_FLASK
        jsr set_flag_y
        jsr Print_Ok

        rts
    

;--- Take Command ---------------
; Se puede reutilizar mucho código aquí
Take_Cmd
        ; Take Idol
        lda current_loc_type
        cmp #IDOL_LOC
        bne @Take_Flask

        ldy #F_IDOL
        jsr read_flag_y
        bne Invalid_Take

        ldy #F_IDOL
        jsr set_flag_y
        jsr Print_Ok
        
       rts

@Take_Flask
        lda current_loc_type
        cmp #FLASK_LOC
        bne @Take_Compass

        ldy #F_FLASK
        jsr read_flag_y
        bne Invalid_Take

        ldy #F_FLASK
        jsr set_flag_y
        ;jsr Print_Ok
        lda #<str_take_flask
        ldy #>str_take_flask
        jsr PRTSTR

        
        rts

@Take_Compass
        lda current_loc_type
        cmp #COMPASS_LOC
        bne @Take_Ankh

        ldy #F_COMPASS
        jsr read_flag_y
        bne Invalid_Take

        ldy #F_COMPASS
        jsr set_flag_y
        ;jsr Print_Ok

        lda #<str_take_compass
        ldy #>str_take_compass
        jsr PRTSTR

       rts

@Take_Ankh
        lda current_loc_type
        ;sta mem_mon
        ;sta mem_mon+1
        cmp #ANKH_LOC
        bne Invalid_Take ; No more items

        ldy #F_ANKH
        jsr read_flag_y
        bne Invalid_Take

        ldy #F_ANKH
        jsr set_flag_y
        jsr Print_Ok

        rts

Invalid_Take
        jsr Print_You_Cannot
        rts


;--- Guide command --------------
Guide_Cmd
        ; Check if you have compass
        ldy #F_COMPASS
        jsr read_flag_y
        beq Invalid_Take

        lda #<str_compas_point
        ldy #>str_compas_point
        jsr PRTSTR

        lda player_y
        ;sta mem_mon
        sec
        sbc #DESTINATION_Y
        ;sta mem_mon+1
        beq @Check_X
        bcc @Y_Minus
        ; X is bigger, go south
        lda #<str_south
        ldy #>str_south
        jsr PRTSTR
        jmp @Check_X

@Y_Minus
        lda #<str_north
        ldy #>str_north
        jsr PRTSTR


@Check_X
        lda player_x
        ;sta mem_mon
        sec
        sbc #DESTINATION_X
        ;sta mem_mon+1
        beq @End
        bcc @X_Minus
        ; Y is bigger, 
        lda #<str_west
        ldy #>str_west
        jsr PRTSTR
        rts

@X_Minus
        lda #<str_east
        ldy #>str_east
        jsr PRTSTR
@End
        rts

;--- Enter_Cmd ----------------
Enter_Cmd
        ; Chaek you are in temple LOC
        lda current_loc_type
        cmp #TEMPLE_LOC
        bne Print_You_Cannot ; No more items

        ; Chack you have the Ankh
        ldy #F_ANKH
        jsr read_flag_y
        beq Print_You_Cannot

        ; Move to a special loc
        lda #$10
        sta player_x
        sta player_y

        ; Temple is like an Oasis
        lda #OASIS_LOC
        sta current_loc_type
        jsr Loc_Temple

        rts

;-----------------------------
Rest_Cmd
        ; check if player is inside temple
        lda #$10
        cmp player_x
        bne Print_You_Cannot
        ; Restore life
        lda #INITIAL_LIFE
        sta life
        jsr PrintLife
        rts


;--- Helper Commands ------------

Print_You_Cannot
        lda #<str_you_cannot
        ldy #>str_you_cannot
        jsr PRTSTR
        rts


;---
Inventory_Cmd
        ldy #$4
        ;sty mem_mon

        lda #<str_carry
        ldy #>str_carry
        jsr PRTSTR

@Loop
        tya
        
        pha
        jsr read_flag_y
        beq @Next
        pla
        ;
        pha
        tax
        dex
        txa
        asl
        tax
        ;sta mem_mon,x
        
        ;ldx #$0
        ;lda v_item_names,x
        ;sta mem_mon,x
        ;lda v_item_names,x+1
        ;sta mem_mon,x+1
        
        lda v_item_names,x
        ldy v_item_names,x+1
        jsr PRTSTR
        
@Next
        pla
        tay
        dey
        bne @Loop

;@End
        jsr CR
        jsr CR        
        rts
        
        
;---
Look_Cmd
        jmp LocLoop

;---
Die_Cmd
        jmp Game_Begin

;---
Help_Cmd
        lda #<str_help
        ldy #>str_help
        jsr PRTSTR
        rts

;---- LOCS ---------------------------------


;---------
Loc_Empty
        ;pha
        lda #<str_you_are
        ldy #>str_you_are
        jsr PRTSTR
        ;pla

        lda #<str_lost_loc0
        ldy #>str_lost_loc0
        jsr PRTSTR

        jsr CR
        
        rts

;---------
Loc_Birds
        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR

        lda #<str_birds
        ldy #>str_birds
        jsr PRTSTR

        rts

;---------
Loc_Oasis
        lda #<str_you_are
        ldy #>str_you_are
        jsr PRTSTR
        ;pla

        lda #<str_oasis
        ldy #>str_oasis
        jsr PRTSTR

        rts

;----------
Loc_Oasis_East
        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR
        ;pla

        lda #<str_oasis_to_east
        ldy #>str_oasis_to_east
        jsr PRTSTR

        rts


;------------
Loc_Ruk

        jsr Loc_Empty

        ; If movement flag is set, no damage
        ldy #F_MOVEMENT
        jsr read_flag_y
        bne @Exit


  
        lda #<str_ruk_attack
        ldy #>str_ruk_attack
        jsr PRTSTR
        jsr CR

        lda life
        sec
        sbc #RUK_DAMAGE
        ; Estee s sis e desborda 
        bcc Player_Is_Dead
        sta life
        jsr PrintLife

        ; Set movement flag
        ; So no more attacks until player moves
        ldy #F_MOVEMENT
        jsr set_flag_y

@Exit
        rts


;--------
Player_Is_Dead
        lda #$0
        sta life
        
        lda #<str_you_die
        ldy #>str_you_die
        jsr PRTSTR

        jsr SR_Press_Return

        jmp Game_Begin
       

;--------------
Loc_Idol

        jsr Loc_Empty

        ; If idol flag is set, idol is in inventory
        ldy #F_IDOL
        jsr read_flag_y
        bne @Exit

        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR
        ;pla

        lda #<str_idol
        ldy #>str_idol
        jsr PRTSTR


@Exit
        rts

;-----------------
Loc_Vibrates

        jsr Loc_Empty

        ; If idol flag is not set, you do no t have idol
        ldy #F_IDOL
        jsr read_flag_y
        beq @Exit

        lda #<str_idol_vibrates
        ldy #>str_idol_vibrates
        jsr PRTSTR

@Exit
        rts

;---------------------
Loc_Flask
        jsr Loc_Empty

        ; If idol flag is set, idol is in inventory
        ldy #F_FLASK
        jsr read_flag_y
        bne @Exit

        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR
        ;pla

        lda #<str_flask
        ldy #>str_flask
        jsr PRTSTR

@Exit
        rts

;-------------
Loc_Scorpion
        jsr Loc_Empty

        ; If movement flag is set, no damage
        ldy #F_MOVEMENT
        jsr read_flag_y
        bne @Exit

      
        lda #<str_scorpion_attack
        ldy #>str_scorpion_attack
        jsr PRTSTR
        jsr CR

        lda life
        sec
        sbc #SCORPION_DAMAGE
        ; Estee s sis e desborda 
        bcc Player_Is_Dead
        sta life
        jsr PrintLife


        ; Set movement flag
        ; So no more attacks until player moves
        ldy #F_MOVEMENT
        jsr set_flag_y


        ; Destroy Flask if any
        ldy #F_FLASK
        jsr read_flag_y
        beq @Exit

        lda #<str_broken_flask
        ldy #>str_broken_flask
        jsr PRTSTR
        
        ldy #F_FLASK
        jsr clear_flag_y
        ldy #F_FILL_FLASK
        jsr clear_flag_y
        

@Exit
        rts



;----------------------------------------
; Todo el códigod e los obj debería poenrse en una
; subrutina genérica
Loc_Compass
        jsr Loc_Empty

        ; If compass flag is set, it is in inventory
        ldy #F_COMPASS
        jsr read_flag_y
        bne @Exit

        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR
        ;pla

        lda #<str_compass
        ldy #>str_compass
        jsr PRTSTR


@Exit
        rts


;--- Loc_Temple ---------------------
Loc_Found_Temple
        jsr Loc_Empty

        lda #<str_temple_found
        ldy #>str_temple_found
        jsr PRTSTR

        rts

Loc_Temple
        jsr CLRS
        jsr PrintStats

       lda #<str_in_the_temple
       ldy #>str_in_the_temple
       jsr PRTSTR

        rts

;------------------------
Loc_Ankh
        jsr Loc_Empty

        ; If ankh flag is set, it is in inventory
        ldy #F_ANKH
        jsr read_flag_y
        bne @Exit

        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR
        ;pla

        lda #<str_ankh
        ldy #>str_ankh
        jsr PRTSTR

@Exit
        rts


;--------------------------------
Loc_Final
        jsr CLRS
        lda #<str_final
        ldy #>str_final
        jsr PRTSTR
        jsr SR_Press_Return
        jmp Game_Begin
        rts


;----------------------
Loc_Enemy_North
        ; A vision of a enemy
        lda #<str_you_see
        ldy #>str_you_see
        jsr PRTSTR        


        lda #<str_enemy_north
        ldy #>str_enemy_north
        jsr PRTSTR        
        rts


;---- Helpers --------------------------------

;--------
Return_LOC_in_X
        lda player_y
        asl
        asl
        asl
        asl
        clc
        adc player_x
        tax
        rts

;--------
Print_Ok
        lda #<str_ok
        ldy #>str_ok
        jsr PRTSTR

        rts

;--------
CR
        lda #RETURN ; CR
        jsr CHROUT
        rts



;---Flag subriotines --------------------

set_x_y
        ; Innter subroutine
        ldx #$0
        tya
        and masks+$3
        beq set_rts
        inx
        tya
        sec
        sbc #$8
        tay
set_rts
        rts

set_flag_y
        jsr set_x_y
        lda flags,x
        ora masks,y
        sta flags,x
        rts ; Acumulador


clear_flag_y
        jsr set_x_y
        lda masks,y
        eor #$FF ; Invert la mask
        and flags,x
        sta flags,x
        rts ; Acumulador

read_flag_y
        ; a - bit of the flag
        ; 0, not set
        ; no 0, set
        jsr set_x_y
        lda flags,x
        and masks,y
        rts ; Acumulador

;--------

SR_Press_Return
        lda #<str_press_return
        ldy #>str_press_return
        jsr PRTSTR

        jsr Input
        
        rts


;--- Sounds --------------------------------

; Stores in tmp_a the channel you
; want to use
SR_Sound
        ; Hihh byte of mem address for souns
        ; is always $90
        lda #$90
        sta tmp_b

        ldx #$FF
@Loop1
        txa ; Saving x value
        ldx #$0
        sta (tmp_a,x)
        tax

        ; Second loop
        ldy #$ff
@Loop2
        dey
        ; A little delay
        nop
        bne @Loop2
        dex
        bne @Loop1

        ; Turn off sound
        txa ; x is 0 
        sta ($fb,x)

        rts


;--- Data ------------------------------------

;-- Constants
INITIAL_WATER = $c
INITIAL_LIFE = $c
RUK_DAMAGE = $3
SCORPION_DAMAGE = $1
DESTINATION_Y = $c ; Game end when you arrives
DESTINATION_X = $d
TEMPLE_X = $7 ; Coordinates of temple
TEMPLE_Y = $7
SOUND_1 = $0c
SOUND_2 =  $0d

; $3-$4, ADRAY1, vector to flating point / integer routine (INTIDX)
; Never used by basic
player_x = $3
player_y = $4

; $5-$6, ADRAY2, vector to flating point to integer (MAKFP)
; Never used by basic
water = $5
life = $6

; $/ Search-character for basic
; $8 Scan-quotes flag
;verb = $7 ; Uses $7 and $8
; No funciona, puede que las rutinas de inpt loc mabien

; $9 Column cursor before TAB
; $A Byte to LOAD
verb = $9

; $B  used as an index into the BASIC input text

; $C flag for array routines
verb_index = $C

; $D Type of variable: string / numeric
; no funciona

; $e
current_loc_type = $e

; Empty page 0 positions 
tmp_a = $fb
tmp_b = $fc

INPUT_BUFFER = $200 ; Explain

;mem_mon BYTE $0, $0, $0, $0, $0, $0

;--- Flags
;flags        BYTE $0, $0
flags = $76
F_MOVEMENT = $0
F_IDOL = $1
F_FLASK = $2
F_FILL_FLASK = $5 ; Indicates if flask is full (set) or empty (0)
F_COMPASS = $4
F_ANKH = $3

masks
        BYTE %00000001, %00000010, %00000100, %00001000, %00010000, %00100000, %01000000, %10000000

;--- Items ----------

str_idol_name TEXT "idol ", $0
str_flask_name TEXT "flask ", $0
sr_compass_name TEXT "compass ", $0
str_ankh_name TEXT "ankh ", $0

v_item_names    WORD str_idol_name, str_flask_name, str_ankh_name, sr_compass_name


; --- Verbs ----------
; SEarch $53, $45
; EHe (45 48)
                ; N             S       E         W       L        Inventory   DRink (44 52)  
verb_tokens     BYTE $4e, $0, $53, $0, $45, $0, $57, $0, $4C, $0, $49, $0, $44, $52
                ; DIe (44 49)
                BYTE $44, $49 
                ; TAke 54 41, FIll (46 49),GUide(47 55) ENter(45 4e), REst(52 45)
                BYTE $54, $41, $46, $49, $47, $55, $45, $4e, $52, $45
                ; HElp (48, 45)
                BYTE $48, $45
                ; Help
                BYTE $0  

verb_pointers WORD North_Cmd, South_Cmd, East_Cmd, West_Cmd, Look_Cmd, Inventory_Cmd, Drink_Cmd
              WORD Die_Cmd, Take_Cmd, Fill_Cmd, Guide_Cmd, Enter_Cmd, Rest_Cmd
              WORD Help_Cmd

;--- Map ---------------
EMPTY=%00000000

; Each loc is 4 bits
; 0001 - Birds in the sky
OASIS_LOC = $2; 0010 - Oasis
;OASIS_TO_EAST = $3 ; 0011
; Enemy 1: Ruk 0100
IDOL_LOC = $5 ; Object 1: Idol 0101
; Vibration 0110
FLASK_LOC = $7 ; Object 2: Flask 0111
; Enemy 2: Scrorpion 1000
COMPASS_LOC = $9  ; Object 3: Compass 1001
TEMPLE_LOC = $a ; Temple 1010
ANKH_LOC = $b ; Object 4: Ankh 1011
FINAL_LOC = $c ; End of game 1100
; Vision of an enemy 1101
           ;0,1                             ;6, 7
row00 BYTE EMPTY,     EMPTY,     %00000110, %01000110, EMPTY,     EMPTY,     EMPTY, EMPTY
row01 BYTE %00000111, EMPTY,     EMPTY,     %01100000, EMPTY,     %00001001, EMPTY, %00100001
row02 BYTE EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY, %01100000
row03 BYTE %00010000, EMPTY,     EMPTY,     EMPTY,     EMPTY,     %00000101, %00000110, %01000110
row04 BYTE %00100000, EMPTY,     %00000001, EMPTY,     EMPTY,     %01100000, EMPTY, %01100000
row05 BYTE %00000110, EMPTY,     %00000010, EMPTY,     %00000110, %10000110, EMPTY, EMPTY
row06 BYTE %01101000, %01100000, %00000001, EMPTY,     EMPTY,     %01100000, EMPTY, %01110000
row07 BYTE %00000110, EMPTY,     EMPTY,     EMPTY,     EMPTY,     %00000010,EMPTY, EMPTY
row08 BYTE EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY,EMPTY, EMPTY
row09 BYTE EMPTY,     %01110000, EMPTY,     EMPTY,     EMPTY,     %11010110, EMPTY,     EMPTY
row10 BYTE EMPTY,     EMPTY,     %00110011, %00001010, EMPTY,     %01100100, %01100000, %00000110
row11 BYTE %00001011, EMPTY,     EMPTY,     EMPTY,     %01100000, %00000110,EMPTY, %01100100
row12 BYTE EMPTY,     EMPTY,     EMPTY,     %00000110, %10000110, EMPTY,%00001100, %00000110
row13 BYTE %00000010, %00010001, EMPTY,     EMPTY,     %00000110, EMPTY, EMPTY, EMPTY
row14 BYTE EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY, EMPTY,%00010001, %00100000 ; 119
row15 BYTE EMPTY,     EMPTY,     EMPTY,     EMPTY,     EMPTY, EMPTY,EMPTY, EMPTY

v_locs  WORD Loc_Empty, Loc_Birds, Loc_Oasis, Loc_Oasis_East, Loc_Ruk, Loc_Idol
        WORD Loc_Vibrates, Loc_Flask, Loc_Scorpion, Loc_Compass, Loc_Found_Temple
        WORD Loc_Ankh, Loc_Final, Loc_Enemy_North

;--- String -----------

str_water           TEXT "water ", $0
str_life            TEXT "life  ", $0 ; Extra space.
str_prompt          TEXT "> ", $0
str_invalid_verb    TEXT "wrong verb", $0d, $0
str_you_cannot      TEXT "you cannot", $0d, $0
str_ok          TEXT "ok", $0d, $0
str_north       TEXT "north", RETURN, $0
str_south       TEXT "south", RETURN, $0
str_east       TEXT "east", RETURN, $0
str_west       TEXT "west", RETURN, $0
str_press_return 
        BYTE    RETURN
        TEXT "press return", $0
str_help        text "read manual", RETURN, $0
;--- LOCs -------------

str_you_are     TEXT "you are ", $0
str_no_exit     text "no exit", $0d, $0
str_carry     TEXT "you carry ", RETURN, $0

;v_descriptions  WORD str_lost_loc0 ; Borrar

str_lost_loc0   TEXT "lost in desert", $0 ; No return here
str_you_see     TEXT "you see ", $0
str_birds       TEXT "birds in the  sky", $d, $0 ; Extra space
str_oasis       TEXT "in an oasis", $d, $0
str_oasis_to_east       TEXT "an oasis to", $d, "the east", $d, $0 
str_ruk_attack  TEXT "a ruk attacks you!", $d, $0
str_idol        TEXT "an idol on thesand", $d, $0 ; No space before sand
str_idol_vibrates TEXT "the idol vibrates", $d, $0
str_flask       TEXT "a flask on thesand", $d, $0 ; No space before sand
str_take_flask  TEXT "fill it with water", $0
str_scorpion_attack TEXT "a gigant scorpion", $d, "attacks you!", $0 ; TODO Hacer comun el attacks you
str_broken_flask    TEXT "scorpion destroys your flask", RETURN, $0
str_compas_point    TEXT "compass points ", $0
str_take_compass    TEXT "you can guide yourself", $0
str_in_the_temple   TEXT "you are in the temple", RETURN, "you can rest and you", RETURN
                    TEXT "can drink", RETURN, $0
str_compass TEXT "a compass on  the sand", $d, $0 ; Extra space 
str_ankh    TEXT "a corpse with", RETURN,"an ankh" , $d, $0
str_temple_found TEXT "there is a temple hereyou may try to enter", RETURN, $0 ; No space
str_enemy_north  TEXT "an assasin at", RETURN, "the north", RETURN, $0

;--- Beginning and end ------

str_intro       TEXT "you are lost in the", RETURN
                TEXT "desert", RETURN
                TEXT "you must find the city" ; No Return here
                TEXT "of al jadur before you" ; No Return here
                TEXT "run out of water and", RETURN 
                TEXT "life", RETURN, RETURN
                TEXT "desert is full of", RETURN
                TEXT "perils but it also hasitems to help you", RETURN, RETURN
                TEXT "good luck", RETURN, RETURN, $0


str_final       TEXT "you have found the", RETURN 
                TEXT "city", RETURN
                TEXT "your family celebratesyour return", RETURN
                TEXT "you become rich",RETURN
                TEXT "guiding others across",RETURN
                ;TEXT "travellers the", RETURN
                TEXT "the desert", RETURN , RETURN
                TEXT "well done", RETURN, RETURN, $0


str_you_die     TEXT RETURN, RETURN, "you will never leave", RETURN
                TEXT "this burning desert", RETURN, $0

; ¿Por qué no guardas x ew Y juntas en el byte de LOC?
; el código que separa los tpos de loc te dirve para 
; separar estas también
; Meter todo lo único en un mismo método que compruebe núemros de LOC
; y así puedo agnar tiposd e casillas.
