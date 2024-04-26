; Pursuing Tom Ram
; A text adventure for your unexpanded VIC-20
; by epic Adventures
;
;

; Starting RAM memory for
; unexpanded VIC-20 
*=$1001

; Funciona bien
; 1216 > 1220 > 121e > 1233 > 1273 > 1278 > 12f5 > 1356 > 13E2 > 1447 > 14EE > 14e2 > 153A > 156A > 156F
; 4: 15A7 > 16CD > 1752 > 1771 > 1873 > 18d8 > 1960 > 19D0 > 1A0b > 19f5, 1A44
; 5: 1A74 > 1A69 > 1AD6 > 1C35 > 1C25 > 1D32 > 1D51 > 1D75
; Me han sobrado 139 bytes.

; Loader
; this code runs the game whe it is loaded.
; Exceutes  SYS 4109

        BYTE    $0B, $10, $0A, $00, $9E, $34, $31, $30, $39, $00
        BYTE    $00, $00

; BASIC and Kernal Subrutines

; Print a text from A and X
PRTSTR = $CB1E 
; Send a character to output (screen by default)
CHROUT = $ffd2 
; Read an input until RETURN
CHIN = $FFCF 
; Clear the Screen
CLEAR = $e55f 

; Constanst
RETURN = #$0D
SPACE = $20

; Memory addreesses
INPUT_BUFFER = $200
P0_TMP_A = $fb
P0_TMP_B = $fc
P0_TMP_BYTE = $fd



;--- Main subroutine --------------------------

Game_Begin
        ; Init variables
        jsr Init
        ; show intructions of the game
        jsr Instructions
        
LocLoop
        ; Print information
        jsr PrintContent

MainLoop
        ; Reset stack
        ldx #$ff
        txs

        jsr InputCommand; get command
        jsr EvalCommand
        jmp MainLoop
        rts 

;---- Init the game -------------------------
Init    
        lda #$1 
        sta loc ; Loc inicial

        ; Ste flags to 0
        lda #$0
        sta flags
        sta flags+1


        ; Resets objets to their original location
        ldx #$0
        lda #LOC_HIDDEN
        sta obj_status,x
        lda #%00000100
        sta obj_status,x+1
        ; Revolver necer changes its loc
         lda #LOC_HIDDEN ; LEaf
        sta obj_status,x+3
         lda #%00001101
        sta obj_status,x+4
         lda #LOC_HIDDEN ; DOll
        sta obj_status,x+5
        
        rts

;---- Intro text ------------------
Instructions
        jsr CLEAR

        lda #<str_introduction_1
        ldy #>str_introduction_1
        jsr PRTSTR

        lda #<str_introduction_2
        ldy #>str_introduction_2
        jsr PRTSTR

        jsr SR_Press_A_Key

        rts

;---------------------------------------

PrintContent
; Print information when player moves to a 
; new location (or command Look)

        ; Print description of the current loc
        jsr PrintLoc
        ; Print objects in current loc
        ; (if any)
        jsr PrintObjs
        jsr PrintSpecial

        rts

;-------------------
PrintLoc
        ; clear screen
        jsr CLEAR

        ; print 'you are in the '
        lda #<you_are
        ldy #>you_are
        JSR PRTSTR

        ; Search position ind escription vector 
        ; for current loc
        lda loc
        ; Multiply loc by 2 to obtain index
        ; x = (loc*2)
        asl
        tax
        ; Remembre.
        ; Before calling prntstr, low bye woes in a
        ; High byte goes in y.
        lda desc_list,x
        ldy desc_list,x+1
        JSR PRTSTR

        ; Descriptions are stored without carryage return
        ; So I print the carryage return character
        lda #RETURN
        jsr CHROUT
        rts

;-------------------
PrintObjs
        ; Using the Input buffer as temporal storing
        ; for a subroutine
        lda #<see_object
        sta INPUT_BUFFER
        lda #>see_object
        sta INPUT_BUFFER+1

        lda loc
        sta P0_TMP_BYTE

; Inventory command reuse this entry.
Inventory_Entry 
        ; Prepare the callback for the subroutine
        lda #<C_Print_Item
        sta P0_TMP_A
        lda #>C_Print_Item
        sta P0_TMP_B
        
        jsr F_Search_Object
        rts

; Callback
C_Print_Item
; If an object is found in the loc, 
; this subroutine prints the message in INPUT_BUFFER

        ; Item found in loc
        ; Print item name
        ; Remember, if you store memory addresses, 
        ; you multiply index by 2.
        ; x = (loc*2) 
        
        asl
        pha

        ; Print the string stored in input buffer
        ldy INPUT_BUFFER+1
        lda INPUT_BUFFER
        ; Take care, this subroutine changes the 
        ; three registers
        JSR PRTSTR 
      
        pla
        tax

        ; Prints the name of the object
        ldy obj_list,x+1
        lda obj_list,x 
        JSR PRTSTR
        
        lda #RETURN
        jsr CHROUT

        ; Jump back to the subroutine      
        jmp F_SO_Back


;-------------------
PrintSpecial
        ; diary in library
        lda loc
        cmp #LOC_LIBRARY
        bne @PS_Next_1

        lda #<str_there_is
        ldy #>str_there_is
        jsr PRTSTR

         lda #<bib_diary
        ldy #>bib_diary
        jsr PRTSTR

@PS_Next_1
        ; Bed in bedroom
        ; After reading diary
        lda loc
        cmp #LOC_BEDROOM
        bne @PS_Next_2

        jsr PS_Check_read_Diary
        beq @PS_Next_2

        lda #<str_there_is
        ldy #>str_there_is
        jsr PRTSTR
        
        lda #<str_bed
        ldy #>str_bed
        jsr PRTSTR

@PS_Next_2
        ; Mirror in the lounge
        ; After reading diary
        lda loc
        cmp #LOC_DINNING
        bne PS_Next_3

        jsr PS_Check_read_Diary
        beq PS_Next_3

        lda #<str_there_is
        ldy #>str_there_is
        jsr PRTSTR

        lda #<str_mirror
        ldy #>str_mirror
        jsr PRTSTR


PS_Next_3
        rts

;------------------------------------------------

InputCommand
        ; Read the keyboard
        ; and try to identify the verb and the name_index
        ; It expects VERB or VERB NAME

        jsr PrintPromt
        jsr Input

        ; Take 2 characters for verb and
        ; two caharcters for name
        jsr ProcessInput
        jsr TokenizeVerb
        jsr TokenizeName
        rts

;---------------
PrintPromt
        lda #RETURN
        jsr CHROUT

        ; Print '> '
        lda #<prompt
        ldy #>prompt
        jsr PRTSTR
        rts

;---------------
Input
        LDX #0

@Loop
        JSR CHIN
        STA INPUT_BUFFER,X ; $200
        INX
        CMP #RETURN 
        BNE @Loop
        rts

;---------------
ProcessInput

        ldx #0
        ldy #0

        ; Cleans verb and name
        stx verb+1 
        stx name
        stx name+1

N_C     lda INPUT_BUFFER,X
        cmp #RETURN
        beq @End
        CMP #SPACE 
        beq @Reset
        cpy #$4
        beq @Next
        sta verb,y
        iny
        cpy #$2
        beq @Disable

@Next    
        inx
        jmp N_C

@Disable
        ldy #$4
        jmp @Next

@Reset
        ldy #$2
        jmp @Next

@End    
        lda #RETURN
        jsr CHROUT 
        rts

;---------------
TokenizeVerb

        ; Set the vector of verbs as parameter
        ; of the helper 
        lda #<verb_tokens
        sta P0_TMP_A 
        lda #>verb_tokens
        sta P0_TMP_B

        lda verb
        sta token
        lda verb+1
        sta token+1

        jsr f_Tokens
        ; Token en verb_index
        sta verb_index
        ;pla 
        ;sty verb_index
        rts


;---------------
TokenizeName

        lda #<name_tokens
        sta P0_TMP_A 
        lda #>name_tokens
        sta P0_TMP_B

        lda name
        sta token
        lda name+1
        sta token+1

        jsr f_Tokens
        ; Token en verb_index
        sta name_index
        rts

;----------------------------------------------------

EvalCommand
        ; Si no hay token, poner coando erroneo
        jsr CheckVerb
        jsr CheckName
        jsr CheckVerbNeedsName
        jsr ExecuteCommand
        rts

;---------------
CheckVerb
        lda verb_index
        ; If verb_index is 0 then no valid
        ; verb was found
        bne @Exit
        lda #<invalid_verb
        ldy #>invalid_verb
        jsr PRTSTR
         ; Back to ask another input
        jmp MainLoop
@Exit 
        rts


;---------------
CheckName
        ; If thee is no name, 
        ; there is nothing to chec
        lda name
        beq CN_Exit 
        lda name_index
        bne CN_Exit

; Commands that needs a name comes here if
; there is no name
Wrong_Name
        lda #<invalid_name
        ldy #>invalid_name
        jsr PRTSTR
        jmp MainLoop
CN_Exit 
        rts


;---------------
CheckVerbNeedsName
        ;  if verb needs a name
        ; then cheks if tehre is a name

        lda verb_index
        sec
        sbc #SINGLE_VERB
        bcc @End ; Verb does not need a name

        ; Verb needs a name
        ; Checks if there is a name
        lda name_index
        beq Wrong_Name

@End
        rts


;---------------
ExecuteCommand
        ; Si no uso el 0 como no hayverbo no tengo que hacer esto
        dec verb_index ; Calcula el indice del verso
        lda verb_index
        asl
        tax
        lda verb_pointers,x
        sta $fb
        lda verb_pointers,x+1
        sta $fc
        lda loc ; Para el comando
        jmp ($fb)

;---------------
MovementCommands
        ; I have in a the current loc.
        ; I store two locs in each position of the exit vecter
        ; So I divide the loc by two to obtain the index
        lsr
        tax 
        lda exits,x 
        tay 

        ; I load the current loc again and 
        ; I check the first bit.
        ; this is the way to see if loc is pair / odd
        lda loc
        and #%00000001 
        ; If loc is odd, the exists are in 4 
        ; lower bits, so their nothing else to do
        bne Go_Back
        
        ; It is pair, so it is in the 4 ihest bits
        ; I move that bits four positions right
        tya
        lsr
        lsr
        lsr
        lsr
        tay 
Go_Back 
        rts

Norte ; North
        ; Put the exit in the first
        ; four bits of y
        jsr MovementCommands
        tya
        ; Take the fourth bits
        ; the first one indicates the north
        and #%00001000
        ; If it is 0 there is no exit
        ; to north, so it is time to print 
        ; a message
        beq NoSalida 
        ; In this map, going north
        ; is adding 3
        lda loc
        clc
        adc #$3
        ; Rememeber to store the new loc
        sta loc
        jmp End

Sur ; South
        jsr MovementCommands
        tya
        and #%00000100
        beq NoSalida ; No exit
        lda loc
        ; A substraction
        ; The opposit one than north
        sec
        sbc #$3
        sta loc
        jmp End

Este ; East
        jsr MovementCommands
        tya
        and #%00000010 
        beq NoSalida ; No exit
        lda loc
        clc
        adc #$1
        sta loc
        jmp End


Oeste ; West
        jsr MovementCommands
        tya
        and #%00000001
        beq NoSalida ; No exit
        lda loc
        sec
        sbc #$1
        sta loc
        jmp End

NoSalida ;NoExit
        ; You used a movement verb but 
        ; there is no exit
        ; Prints no exit.        
        lda #<no_exit
        ldy #>no_exit
        jsr PRTSTR
Fin    rts
        
End     
        ; Esto tiene que ejecutarse cuando hay un movimiento
        jsr PrintContent 
        rts

;----Objects commands---------------------------------------------

;-----------------
Inventory_CMD
        lda #<str_you_carry
        sta INPUT_BUFFER
        lda #>str_you_carry
        sta INPUT_BUFFER+1

        lda #LOC_INVENTORY
        sta P0_TMP_BYTE

        jsr Inventory_Entry
        rts


;---------
Search_CMD
        ; Player is in the garden'
        cmp #LOC_GARDEN
        bne @Next_Search_1

        ; LEaf is hidden
        ldx #LEAF_IND
        jsr Item_is_Hidden
        bne @Next_Search_1

        ; Move LEaf to the garden
        lda loc
        sta obj_status,x

        jsr PrintObjs
        rts

@Next_Search_1
        ; Search symbols in the lounge

        ; Player is in the lounge
        lda loc
        cmp #LOC_LOUNGE
        bne @Next_Search_2

        ; Player had the the dream
        ; Flag 1 is set
        ldy #$1
        jsr read_flag_y
        beq @Next_Search_2 ; es cero

        ; You find the symbols under a carpet
        ; You need the buried key and the magic word
        lda #<str_find_symbols
        ldy #>str_find_symbols
        jsr PRTSTR

        rts

@Next_Search_2
        ; search chandelier in garden

        ; I'm at the garden
        lda loc
        cmp #LOC_GARDEN
        bne @Next_Search_3

        ; I had the vision
        ; Flag 1 is set
        ldy #$1
        jsr read_flag_y
        beq @Next_Search_3 ; es cero

        ; Candelebra is hidden
        ldx #CHANDELIER_IND
        jsr Item_is_Hidden
        bne @Next_Search_3


         ; Move CHandelier to his loc
        lda loc
        sta obj_status,x

        jsr PrintObjs
        rts


@Next_Search_3
        ; I'm at the Dinning Room
         lda loc
        cmp #LOC_DINNING
        bne @Next_Search_4

         ; Mirror is here
        jsr PS_Check_read_Diary
        beq @Next_Search_4

        lda #<str_figure_kills
        ldy #>str_figure_kills
        jsr PRTSTR

        jmp SR_You_Die
        ;rts


@Next_Search_4
        ; I'm at the Garage
         lda loc
        cmp #LOC_GARAGE
        bne @Next_Search_5

        ; Trap not found - Flag 3
        ldy #$2
        ; Set flag
        ; a already set
        jsr set_flag_y

        ; Message
        lda #<str_there_is
        ldy #>str_there_is
        jsr PRTSTR

        lda #<str_trapdoor
        ldy #>str_trapdoor
        jsr PRTSTR

        rts

@Next_Search_5
        ; Kitchen
        lda loc
        cmp #LOC_KITCHEN
        bne @Next_Search_6

         ; Message
        lda #<str_kitchen
        ldy #>str_kitchen
        jsr PRTSTR

        rts

@Next_Search_6
        jsr P_Useless
        rts


;----------------

P_Done
        lda #<str_done
        ldy #>str_done
        jsr PRTSTR
        rts

Put_No_Carried
        jmp Ex_No_Carried


Put_CMD
        ; Check name is in the inventory
        ; of the player
        ldx name_index
        jsr Item_In_Inventory
        bne Put_No_Carried

        ; -- Put Leaf in Bed

        ; Name is LEaf
        ldx name_index
        cpx #LEAF_IND
        bne @Next_Put_1

        ; Loc is beedroom
        lda loc
        cmp #LOC_BEDROOM
        bne @Next_Put_1

        dex
        lda #LOC_DESTROYED ; LOC 3D / 61 - Destroyed
        sta obj_status,x
        ; Mensaje
        jsr P_Done
        rts

@Next_Put_1
        ; -- Put CAndelebra in lounge

        ; You had the vision
        ldy #$1
        jsr read_flag_y
        beq @Next_Put_2 ; Sie s 0 no la tienes

        ; Loc is lounge
        lda loc
        cmp #LOC_LOUNGE
        bne @Next_Put_2

        ; Name is CHandelier
        ldx name_index
        cpx #CHANDELIER_IND
        bne @Next_Put_2

        ; Remove item and write 'Done'
        ; I could use status bit for destroyed items
        dex
        lda #LOC_DESTROYED ; LOC 3D / 61 - Destroyed
        sta obj_status,x

        ; Mensaje
        jsr P_Done
        rts

@Next_Put_2 ; Place idol
        
        ; Loc is gordon
        lda loc
        cmp #LOC_GORDON
        bne @Next_Put_3

        ; Doss is not yet
        ldx #DOLL_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        beq @Next_Put_3 ; si está destruido, ya se ha pùesto.

        ldx #IDOL_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        beq @Next_Put_3 ; si está destruido, ya se ha pùesto.

        ; Name is Idol
        ldx name_index
        cpx #IDOL_IND
        bne @Name_is_DOll

@Put_back_2
        ; Remove item and write 'Done'
        dex
        lda #LOC_DESTROYED ; LOC 3D / 61 - Destroyed
        sta obj_status,x
        ; Idol is now destroyed
        ; this means is in the loc
       
        ; Mensaje
        jsr P_Done
        rts

;@Idol_is_put
        

@Name_is_DOll
        ; Name is Dool
        ldx name_index
        cpx #DOLL_IND
        bne @Next_Put_3
        jmp @Put_back_2


@Next_Put_3 ; Place doll

@Next_Put_4
        jsr P_Useless
        rts



;--- Subroutines for iobject management

;---
Name_Is_Item
        lda name_index
        tax        
        ; If name is bigger than the cosntant, 
        ; then the player cannot take it
        sec
        sbc #OBJECTS_NUMER 
        rts

;---
; Loads status of item 1
Lda_Obj_Status
        ; Check if obj is in this loc
        dex
        lda obj_status,x        
        and #%00111111 ; LLevar esta mascara a cte
        rts

;--- 
; Param x item number
Item_in_Loc
        ;ldx name_index
        jsr Lda_Obj_Status
        cmp loc
        ; If Z = 0 is current loc
        rts

;---
; Param x item number
Item_In_Inventory
        jsr Lda_Obj_Status
        cmp #LOC_INVENTORY
        ; Z = 0 it is in inventory
        rts

;--- 
; Param x item number
Item_is_Hidden
        ;ldx name_index
        jsr Lda_Obj_Status
        cmp #LOC_HIDDEN
        ; If Z = 0 is current loc
        rts


;-----------------
Take_CMD
        ; Name is an item
        jsr Name_Is_Item
        bcs Take_P_Useless ; Cambiar a P_Useless 

        ; Item is in current loc
        ;jsr Lda_Obj_Status
        ;cmp loc
        jsr Item_in_Loc
        bne Lbl_No_Item

        ;  I AM NOT SAVING SPECIAL BITS
        lda #LOC_INVENTORY
        sta obj_status,x
        ; Call inventory to confirm
        jsr Inventory_CMD
        rts

Lbl_No_Item
        lda #<no_item
        ldy #>no_item
        jsr PRTSTR
        rts 


Take_P_Useless
        jmp P_Useless


;--- Examine ------------------------------

;--------------
Ex_No_Carried
        lda #<no_carried
        ldy #>no_carried
        jsr PRTSTR

Ex_Exit
        ; Salto linea
        lda #RETURN
        jsr CHROUT
        rts  


Examine_CMD
        ; Is name is an item, shows
        ; itemd escription
        jsr Name_Is_Item
        ; If not, search for a subroutine
        bcs Ex_Diary_Library ; Mirar EXs particulares !!!

        jsr Lda_Obj_Status
        cmp #LOC_INVENTORY ; $3F

        ; It's an item
        ; But it is not in your inventory
        bne Ex_No_Carried

        txa
        ASL
        tax

        lda obj_description,x
        ldy obj_description,x+1
        jsr PRTSTR
        
        jmp Ex_Exit


;--------------
Ex_Diary_Library
        ;Name is DIary
        cpx #DIARY_IND
        bne Ex_Bed_Bedroom

        ;Loc is library
        lda loc
        cmp #LOC_LIBRARY
        bne Ex_Bed_Bedroom

        ; Print description
        lda #<desc_diary
        ldy #>desc_diary
        jsr PRTSTR
        jmp Ex_Exit
        
;--------------
Ex_Bed_Bedroom
        ;Name is BE
        cpx #BED_IND
        bne Ex_Mirror_Dinning

         ;Loc is bedroom
        lda loc
        cmp #LOC_BEDROOM
        bne Ex_Mirror_Dinning

        ; Print description
        lda #<desc_bed
        ldy #>desc_bed
        jsr PRTSTR
        jmp Ex_Exit


;--------------
Ex_Mirror_Dinning
        ;Name is MIrror
        cpx #MIRROR_IND
        bne Ex_Gordon_Dinnin

         ;Loc is dinnind room
        lda loc
        cmp #LOC_DINNING
        bne Ex_Gordon_Dinnin

        ; Print description
        lda #<desc_mirror
        ldy #>desc_mirror
        jsr PRTSTR
        jmp Ex_Exit

;--------------
; Warning, the original anatagoist was Gordon.
; It was changed to Tom to gain some estra bytes.
Ex_Gordon_Dinnin
        ;Name is TOm
        ; Althoufh I did no change the names
        ; of variables and subroutines
        cpx #GORDON_IND
        bne Lbl_No_Item

         ; Loc is Gordon's Loc
        lda loc
        cmp #LOC_GORDON
        bne Lbl_No_Item

       
        ; Print description
        lda #<desc_gordon
        ldy #>desc_gordon
        jsr PRTSTR
        jmp Ex_Exit

;--- Puzzle commands ---------------------------------

;- Read diary with classes
Read_CMD
        ; loc is desktop
        cmp #LOC_LIBRARY
        bne P_Useless

        ; Name es diary
        lda name_index
        cmp #DIARY_IND
        bne P_Useless

        ; Player has glasses in inventory
        ldx #GLASSES_IND
        jsr Item_In_Inventory        
        bne P_Understand_Nothing

        ; print message
        lda #<str_read_diary
        ldy #>str_read_diary
        jsr PRTSTR

        ; Set flag 0.
        ; The game knows if diary is read
        ; using this flag
        ldy #$0
        jsr set_flag_y

        rts

; If player is not in desktop or try to read
; a thing it is not a diary.
P_Useless
        lda #<str_useless
        ldy #>str_useless
        jsr PRTSTR
        rts

; Readin the diary witthout the glasses
; This is clue, that player needs glasses
P_Understand_Nothing
        lda #<str_understand_nothing
        ldy #>str_understand_nothing
        jsr PRTSTR
        rts


;--------
Sleep_CMD
         ; loc is bedroom
        cmp #LOC_BEDROOM
        bne P_Useless

        ; Player read the diary
        ; if flag 0 is setted
        ldy #$0 ; Flag 0
        jsr read_flag_y
        ; Player did not read the aidary
        beq P_Useless 

        ; You put the LEaf
        ; if LEaf is destroyed
        ldx #LEAF_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne P_Useless

        ; You have a vision
        lda #<str_vision
        ldy #>str_vision
        jsr PRTSTR

        ; Set flag 1
        ; Game knows player had a vision
        ldy #$1
        jsr set_flag_y

        rts



;--- Other commands -----------------------------------

;-----------------
Look_CMD
        jmp LocLoop ; ¿Resetear la pila?

;-----------------
Ehe_CMD    ; Magical word


        ; Trip to another dimension

        ; In the lounge
        ;lda loc
        cmp #LOC_LOUNGE
        bne @Ehe_End_Game

        ; Vision
        ; Here I'm in the liounge'
        ldy #$1
        jsr read_flag_y
        beq P_Useless ; si es 0 no está set

        ; Chandelier
        ldx #CHANDELIER_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne P_Useless ; CAMBIAR ESTO POR YOPU NEED THE KEY
        
        ; Sin Texto

        ; Cambiar loc
        lda #LOC_NEW_DIMENSION
        sta loc

        ; Cambiar la desc del DOll
        ldx #DOLL_IND
        dex
        txa
        asl
        tax ; x2 poque e suna dir de mem.
        lda #<desc_obj0
        sta obj_description,x
        lda #>desc_obj0
        sta obj_description,x+1

        jmp LocLoop


@Ehe_End_Game
        cmp #LOC_GORDON
        bne @Ehe_Useless

        ; Idol
        ldx #IDOL_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne @Ehe_End_Doll

        ; Msg
        lda #<str_bad_ending
        ldy #>str_bad_ending
        jsr PRTSTR

        ; You die
        jmp SR_You_Die


@Ehe_End_Doll
        ; Idol
        ldx #DOLL_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne @Ehe_Useless

        ; Msg
        lda #<str_good_ending
        ldy #>str_good_ending
        jsr PRTSTR

        ; You dont die
        jmp SR_Press_A_Key


@Ehe_Useless
        jmp P_Useless

;---------------
Shoot_CMD
        ; You always wear the revolver
       
        ; Gordon Room
        ;lda loc
        cmp #LOC_GORDON
        bne @Shoot_Next_1

        ; You shoot Gordon
        lda name_index
        cmp #GORDON_IND
        bne @Shoot_Next_1

        lda #<str_shoot_gordon
        ldy #>str_shoot_gordon
        jsr PRTSTR

        rts

@Shoot_Next_1
        ; Gordon Room
        lda loc
        cmp #LOC_MONSTERS
        bne @Shoot_Next_2

        ; You shoot Gordon
        lda name_index
        ;sta mem_mon
        cmp #MONSTER_IND
        bne @Shoot_Next_2

        lda #<str_beasts_kills
        ldy #>str_beasts_kills
        jsr PRTSTR

        jmp SR_You_Die

        rts

@Shoot_Next_2
        jmp P_Useless


;--------------------

Open_CMD
        ; Yo are in the Garage
        lda loc
        cmp #LOC_GARAGE
        bne Open_Next_1

        ; You found trap
        ; flag 2
        ldy #$2
        jsr read_flag_y
        beq Open_Next_1

        ; Text
       lda #<str_open_trapdoor
        ldy #>str_open_trapdoor
        jsr PRTSTR

        ; if doll is inventory, then end
        ; Cambiar por ime_is_hidden 
        ldx #DOLL_IND
        ;dex
        ;lda obj_status,x
        ;cmp #LOC_INVENTORY
        jsr Item_is_Hidden
        bne @Open_End_0

        ; Doll appears here
        lda #LOC_GARAGE
        ; ya le han hecho el dex
        sta obj_status,x

@Open_End_0
        rts


Open_Next_1
        jmp P_Useless



;--- Subrutinas de bajo nivel ----------------------------

SR_You_Die

        lda #<str_you_are_Dead
        ldy #>str_you_are_Dead
        jsr PRTSTR

        ; no sé cómo esperar uan puislación así que repito el input
        jsr SR_Press_A_Key
        jmp Game_Begin 

;--------

SR_Press_A_Key
        lda #<str_press_return
        ldy #>str_press_return
        jsr PRTSTR

        jsr Input
        
        rts


;------ 
; Find a token in a vector
; The token must be in $Token
; Returns the index of the token in a
; starts with 1, a = 0 means it has not been found
; 
F_tokens   
        ldy #$0

F_Loop    
        lda (P0_TMP_A),y
        beq F_Exit ; N
        cmp token
        bne Next1
        iny
        lda (P0_TMP_A),y
        cmp token+1
        bne Next2
        iny
        tya
        lsr
        
F_Exit  ; a = 0 not found, other found  
        rts

Next1   iny
Next2   iny
        jmp F_Loop


;--- Search for items 
; LOC is in P0_TMP_BYTE

F_Search_Object
        ldx #$0

F_SO_Loop
        lda obj_status,x
        beq F_SO_End
        and #%00111111 ; 2 status bit = 4 status
        cmp P0_TMP_BYTE
        bne F_SO_Next_Obj
        ; Objeto entontrado en LOC $fd
        txa ; Save x bfore callback
        pha
        ;sta mem_mon,x
        jmp $(fb) ; Callback

F_SO_Back
        pla ; Restore X
        tax

F_SO_Next_Obj
        inx
        jmp F_SO_Loop

F_SO_End
        rts


;---Flag subriotines ----

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


; No lo necesito
clear_flag_y
        jsr set_x_y
        lda masks,y
        eor #$FF ; Invert mask
        and flags,x
        sta flags,x
        rts 

read_flag_y
        ; a - bit of the flag
        ; 0, not set
        ; no 0, set
        jsr set_x_y
        lda flags,x
        and masks,y
        rts ; Acumulador

PS_Check_read_Diary
        ldy #$0
        jsr read_flag_y
        cmp #$0
        rts



;--- Data -------------------------------------------

  

;mem_mon       BYTE $0, $0, $0, $0, $0, $0, $0, $0, $0
verb            BYTE $0, $0 ; Input from keyboard
name            BYTE $0, $0 ; name for the keyboard
                            ; It must follow the verb's memory posotityions
token           BYTE $0, $0 
loc             BYTE $0 ; Location of the player
verb_index      BYTE $0 ; 0 mena no found.
name_index      BYTE $0 

; Geeral purpouse flags
; Each flag is 1 bit, so the game have 16 flags
flags
        BYTE $0, $0

; Masks to manipulate flags and exits
masks
        BYTE %00000001, %00000010, %00000100, %00001000
        BYTE %00010000, %00100000, %01000000, %10000000

;--- Data -------

OBJECTS_NUMER = $7
CHANDELIER_IND = $1
GLASSES_IND = $2
LEAF_IND = $4
IDOL_IND = $5
DOLL_IND = $6

DIARY_IND = $7
BED_IND = $8
MIRROR_IND = $9
GORDON_IND = $a
MONSTER_IND = $b

; Not used: BAll (42 41)

; Objects: CHandelier (43 48), GLasses, REvolver (52 45), LEaf (4C, 45), IDol (49 44), DOll (44, 4f)
; No Objs: DIary (44 49), BEd (42 45), MIrror (4D 49), TOm (54 4f), MOnster (4D, 4f), TRapdoor (54, 52)
; GOrdon (47 4f)
name_tokens     BYTE $43, $48, $47, $4C, $52, $45, $4C, $45, $49, $44, $44, $4f
                BYTE $44, $49, $42, $45, $4d, $49, $54, $4f, $4d, $4f, $54, $52, $0 

; Is verb token is under SINGLE_VERB it doesnt need a name
; If verb is over SINGLE_VERB a name is mandatory
; Si añades un verbo sin nombre, tienes que ponerlo al lado del úñtimo vero sin sonmbre
; E incrementar esta constante
SINGLE_VERB=11

; N,S,E,W,I (49), Look (4c), SLeep (53 4c), SEarch ($53, $45),   
verb_tokens     BYTE $4e, $0, $53, $0, $45, $0, $57, $0, $49, $0, $4C, $0, $53, $4c, $53, $45
                ; EHe (45 48), DIe (44 49)
                BYTE $45, $48, $44, $49 
                ; TAke, EXamine, REad (52 45), PUt (50 55), SHoot (53 48), OPen(4f 50)
                BYTE $54, $41, $45, $58, $52, $45, $50, $55, $53, $48, $4f, $50
                BYTE $0  

verb_pointers   WORD  Norte, Sur, Este, Oeste, Inventory_CMD, Look_CMD, Sleep_CMD, Search_CMD
                WORD  Ehe_CMD, SR_You_Die
                WORD  Take_CMD, Examine_CMD, Read_CMD, Put_CMD, Shoot_CMD, Open_CMD

you_are         TEXT "you are in the ", $0
invalid_verb    text "wrong verb", $0d, $0
invalid_name    text "wrong name", $0d, $0
no_exit         text "no exit", $0d, $0
str_done        TEXT "done", $0D, $0
str_press_return 
        BYTE    RETURN
        TEXT "press return", $0
prompt  TEXT    "> ", $0


;--- Objects --------

see_object      TEXT "you see a", $0
str_you_carry   TEXT "you carry a", $0
no_item         TEXT "no item", RETURN, $0
no_carried      TEXT "not in inventory", $0 ; ¿No son iguales=?


; No puede haber ningún objeto en loc 0 con los bits finales a 0
; 3F en inventario                              ; $BF
; Los dos primeros bits sond e status pero no los estoy usando.
; 1: 2: 3: Invetopry, 4: Hidden
; Chandelier: 00111110, Revolver, Glaasses
obj_status      BYTE %00111111, %00000100, %10111111, %00111110, %00001101, LOC_DESTROYED, $0
obj_list        WORD obj0, obj1, obj2, leaf_obj, idol_obj, doll_obj
obj0
        TEXT    " chandelier"
        BYTE    $0 
obj1
        TEXT    " glasses" ; MOVER A LA COCINA
        BYTE    $0 
obj2
        TEXT    " revolver"
        BYTE    $0
leaf_obj
        TEXT    " leaf"
        BYTE    $0
idol_obj
        TEXT    "n idol"
        BYTE    $0
doll_obj
        TEXT    " doll"
        BYTE    $0


; CHande, GLass, REvolver, LEaf, IDol, DOll has same description than Chand
obj_description WORD desc_obj0, desc_obj1, desc_obj2, desc_leaf_obj, desc_obj0, desc_doll_0

desc_obj0
        TEXT    "decorated with wicked symbols"
        BYTE    $0 
desc_obj1
        TEXT    "perfect for you"
        BYTE    $0 
desc_obj2
        TEXT    "loaded"
        BYTE    $0
desc_leaf_obj
        TEXT    "a powerful drug" ; salvia divinorum, PONER ALUCINOJENO
        BYTE    $0
desc_doll_0
        TEXT    "it listened and" ; salvia divinorum, PONER ALUCINOJENO
        BYTE    RETURN
        TEXT    "comforted the children" ; salvia divinorum, PONER ALUCINOJENO
        BYTE    $0


desc_diary
        TEXT    "written with strange characters"
        BYTE    $0
desc_bed
        TEXT    "nothing special"
        BYTE    $0
desc_mirror
        TEXT    "in the mirror"
        BYTE    RETURN
        TEXT    "the room is diferent"
        BYTE    RETURN
        TEXT    "the figure is not you"
        BYTE    RETURN
        TEXT    "figure tells you ehe"
        BYTE    RETURN, $0
desc_gordon
        TEXT    "floor has the same"
        BYTE    RETURN
        TEXT    "symbols than the lounge"
        BYTE    RETURN, $0


;--- Locations ------
LOC_LIBRARY = $0
LOC_GARAGE = $2
LOC_LOUNGE = $4
LOC_BEDROOM = $5
LOC_GARDEN = $6
LOC_DINNING = $7
LOC_KITCHEN = $8
LOC_GORDON = $9
LOC_NEW_DIMENSION = $a
LOC_MONSTERS = $b
LOC_INVENTORY=%00111111 ; LOC de los obj del inventario (3F)
LOC_HIDDEN = %00111110 ; LOC de los obj ocultos(3F)
LOC_DESTROYED = %00111101 ; LOCd e los obj que ya nos e usaran más

; N S E O              0   1      2   3      4   5      6   7      8   9      10  11     12  13
exits           BYTE %10101011, %00011110, %11110001, %01000110, %00010010, %10110001, %0000100 

desc_list WORD desc0, desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8 
          WORD desc9, desc10, desc11, desc0, desc13
; desc 12 is not used
desc0
        TEXT    "desktop"
        BYTE    $0 ; Hace un salto de linea.

desc1
        TEXT    "gateway"
        BYTE    $0 ; Hace un salto de linea.
desc2
        TEXT    "garage"
        BYTE    $0
desc3
        TEXT    "living room"
        BYTE    $0
desc4
        TEXT    "lounge"
        BYTE    $0 ; Hace un salto de linea.

desc5
        TEXT    "bedroom"
        BYTE    $0 ; Hace un salto de linea.
desc6
        TEXT    "garden"
        BYTE    $0
desc7
        TEXT    "dinning room"
        BYTE    $0
desc8
        TEXT    "kitchen"
        BYTE    $0

desc9
        TEXT    "room with a big"
        BYTE    RETURN
        TEXT    "blob of meat"
        BYTE    RETURN
        TEXT    "it is tom"
        BYTE    RETURN
        BYTE    $0

desc10
        TEXT    "same house in a diferent dimension"
        BYTE    $0

desc11
        TEXT    "nest ofe dimensional monsters"
        BYTE    $0

;desc12 ; Not used

desc13
        TEXT    "temple of madness"
        BYTE    $0


;--- Specials ---------

str_there_is
        TEXT    "there is a "
        BYTE    $0

bib_diary
        TEXT    "diary"
        BYTE    RETURN, $0

str_useless
        TEXT    "useless"
        BYTE    RETURN, $0

str_understand_nothing
        TEXT    "you understand nothing"
        BYTE    RETURN, $0

str_read_diary
        TEXT    "it talks about dreams "
        BYTE    RETURN
        TEXT    "that became into visions of other dimension"
        BYTE    RETURN, $0

str_bed
        TEXT    "bed"
        BYTE    RETURN, $0

str_mirror
        TEXT    "mirror"
        BYTE    RETURN, $0

str_trapdoor
        TEXT    "trapdoor"
        BYTE    RETURN, $0

str_vision
        ;TEXT "You see that dreams are visions of ther realities"
        ;BYTE    RETURN
        TEXT "you dream the symbols in the lounge to travel to other reality"
        BYTE    RETURN
        TEXT "you dream tom buring the key"
        BYTE    RETURN
        TEXT "you dream you need a magic word from other reality"
        BYTE    RETURN, $0

str_find_symbols
        TEXT "you find the symbols under a carpet"
        BYTE    RETURN
        TEXT "you need the buried key and the magic word"
        BYTE    RETURN, $0

str_figure_kills
        TEXT "a figure in the mirror"
        BYTE    RETURN
        TEXT "extends its arms"
        BYTE    RETURN, $0

str_you_are_Dead
        TEXT "you are dead"
        BYTE    RETURN, $0

str_shoot_gordon
        TEXT "tom screams"
        BYTE    RETURN
        TEXT "look at me"
        BYTE    RETURN
        TEXT "i am ethernal now"
        BYTE    RETURN, $0

str_beasts_kills
        TEXT "mosnters stop eating dimensions"
        BYTE    RETURN
        TEXT "they eat you"
        BYTE    RETURN, $0

str_open_trapdoor
        ; The smell makes you puke
        TEXT "you find a hole"
        BYTE    RETURN
        TEXT "where gordon locked up"
        BYTE    RETURN
        TEXT "the kidnapped children"
        BYTE    RETURN, $0

str_kitchen
        TEXT "You puke when you see the"
        BYTE    RETURN
        TEXT "leftovers of the meals of tom"
        BYTE    RETURN, $0

;--- Introductory text ---------------

str_introduction_1
        TEXT "the vanishing of tom ram"
        BYTE    RETURN
        BYTE    RETURN

        TEXT "fifteen children went missing"
        BYTE    RETURN
        TEXT "in the last ten years"
        BYTE    RETURN
        TEXT "they were poor and"
        BYTE    RETURN
        TEXT "nobody cared but you"
        BYTE    RETURN
        BYTE    $0

str_introduction_2
        TEXT "you are the detective"
        BYTE    RETURN
        TEXT "who has investigated the"
        BYTE    RETURN
        TEXT "missings of children"
        BYTE    RETURN

        TEXT "your suspect is tom ram"
        BYTE    RETURN
        TEXT "a lonely pensioner"
        BYTE    RETURN
        TEXT "you go to his house"
        BYTE    RETURN
        TEXT "to question him"
        BYTE    RETURN
        BYTE    $0

;--- Endings ----------------------

str_good_ending
        TEXT "tom shouts when he travels"
        BYTE    RETURN
        TEXT "to another dimension"
        BYTE    RETURN
        TEXT "where the children are"
        BYTE    RETURN
        TEXT "they are not defenceless"
        BYTE    RETURN
        TEXT "anymore"
        BYTE    RETURN
        TEXT "you wake up in the house"
        BYTE    RETURN
        TEXT "of tom"
        BYTE    RETURN
        TEXT "case closed"
        BYTE    RETURN
        BYTE    $0

str_bad_ending
        TEXT "you open the way to the"
        BYTE    RETURN
        TEXT "deity of madness that"
        BYTE    RETURN
        TEXT "slowly devours your world"
        BYTE    RETURN
        TEXT "while tom laughs forever"
        BYTE    RETURN
        BYTE    $0

;*=$71

