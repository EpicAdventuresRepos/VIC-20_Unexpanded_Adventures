; Pursuing Tom Ram
; A text adventure for your unexpanded VIC-20
; Commodore 64 version
; by Epic Adventures
;
;
*=$0801

        BYTE    $0B, $08, $0A, $00, $9E, $32, $30, $36, $31, $00
        BYTE    $00, $00

; Subroutines
PRINT = $AB1E
PRINT_CHAR = $FFD2 
CHIN = $FFCF 
CLEAR = $E544 ; CLR Screen
 
; Constanst
RETURN = #$0D
SPACE = $20 ; Space
FILAS = #$3 ; No se usa, usar

; Memory addreesses
INPUT_BUFFER = $200
P0_TMP_A = $fb
P0_TMP_B = $fc
P0_TMP_BYTE = $fd



;--- Main --------------------------

Game_Begin
        ; Init variables
        jsr Init
        ; show intructions of the game
        jsr Instructions
        
LocLoop
        ; Print information
        jsr PrintContent

MainLoop
        jsr InputCommand; get command
        ; is quit
        jsr EvalCommand
        ; jsr AfterCommand ; Nos e ejecuta depsués de un movimiento.
        jmp MainLoop
        rts 

;---------------------------------------
Init    
        lda #$1
        sta loc ; Loc inicial

        ; DEBUG
        ; ldy #$1
        ; jsr set_flag_y
        
        ; Hacer un buble aqui
        lda #$0
        sta flags ; Pone a 0 las ti primeras flag
        sta flags+1

        ; Resets objets to their original location
        ; Chandelier: 00111110, Revolver, Glaasses      ; REvolve ; LEaf     ; IDol
        ; obj_status      BYTE %00111111, %00000100, %10111111, %00111110, %00001101, LOC_DESTROYED, $0
        
        ;ldx #$0
        lda #LOC_HIDDEN
        sta obj_status
        sta obj_status+3
        sta obj_status+5

        lda #%00000100
        sta obj_status+1
         lda #%00001101
        sta obj_status+4
        ; Revolver necer changes its loc
        
        ; Cambiar la descripcion de DOll!!
 
        rts

;----------------------
Instructions
        jsr CLEAR

        lda #<str_introduction_1
        ldy #>str_introduction_1
        jsr PRINT

        lda #<str_introduction_2
        ldy #>str_introduction_2
        jsr PRINT

        jsr SR_Press_A_Key

        rts

;---------------------------------------

PrintContent
; Print nformation

        ; Perint description of the current loc
        jsr PrintLoc
        ; Print objects in current loc
        ; (if any)
        jsr PrintObjs
        jsr PrintSpecial
        jsr PrintPuts
        rts

;-------------------
PrintLoc
        ; clear screen
        jsr CLEAR

        ; If you ate the the leaf
        ldx #LEAF_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne @PL_NoLeaf

        ; I you didnt sleep
        ldy #F_SLEEP
        jsr read_flag_y
        bne @PL_NoLeaf

        ; desc_upsidedown
         lda #<desc_upsidedown
        ldy #>desc_upsidedown
        JSR PRINT

        jmp @PL_Return


@PL_NoLeaf
        ; print 'you are in the '
        lda #<you_are
        ldy #>you_are
        JSR PRINT

        ; Search position ind escription vector 
        ; for current loc
        lda loc
        ; x = (loc*2) + 1 
        asl
        tax
        ;inx
        ldy desc_list,x+1
        ;tay
        ;dex
        lda desc_list,x
        JSR PRINT

@PL_Return
        ; Descriptions are stored without carryage return
        ; So I print the carryage return character
        lda #RETURN
        jsr PRINT_CHAR
        rts

;-------------------
PrintObjs
        lda #<see_object
        sta INPUT_BUFFER
        lda #>see_object
        sta INPUT_BUFFER+1

        lda loc
        sta P0_TMP_BYTE

; Entrada para el comando Inventory
Inventory_Entry 
        ; Prepare the callback for the subroutin
        lda #<C_Print_Item
        ;jsr PRINT_CHAR
        sta P0_TMP_A
        lda #>C_Print_Item
        sta P0_TMP_B

        ;lda $4e
        ;jsr PRINT_CHAR

        
        jsr F_Search_Object
        rts

; Callback
C_Print_Item
; Si encuentra el objeto, 
; imprime el mensaje en el INPUT_BUFFER

        ; Item found in loc
        ; Print item name
        ; Este código es igual que el que muestra la loc
        ; x = (loc*2) 
        
        asl
        pha

        ;clc
        ;adc #$40
        ;jsr PRINT_CHAR

        ;ldy #>see_object
        ldy INPUT_BUFFER+1
        ;lda #<see_object        
        lda INPUT_BUFFER
        JSR PRINT ; Este orint me camba la X
      
        pla
        tax

        ldy obj_list,x+1
        lda obj_list,x 
        
        JSR PRINT
        
        ; Salto linea
        lda #RETURN
        jsr PRINT_CHAR
        ; Go back to the subroutine      
        jmp F_SO_Back


;-------------------
PrintSpecial
        ; diary in library
        lda loc
        cmp #LOC_LIBRARY
        bne @PS_Next_1

        lda #<str_there_is
        ldy #>str_there_is
        jsr PRINT

         lda #<bib_diary
        ldy #>bib_diary
        jsr PRINT

@PS_Next_1
        ; Bed in bedroom
        ; After reading diary
        ; Ahora se muestra siempre
        lda loc
        cmp #LOC_BEDROOM
        bne @PS_Next_2

       ; jsr PS_Check_read_Diary
       ; beq @PS_Next_2

        lda #<str_there_is
        ldy #>str_there_is
        jsr PRINT
        
        lda #<str_bed
        ldy #>str_bed
        jsr PRINT

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
        jsr PRINT

        lda #<str_mirror
        ldy #>str_mirror
        jsr PRINT


PS_Next_3
        rts


;---------------
PrintPuts
        
        
@PP_Chandelier
        lda loc
        cmp #LOC_LOUNGE
        bne @PP_DOll

        ldy #F_CHANDELIER
        jsr read_flag_y
        beq @PP_DOll ; Es Cero
        jsr Print_There_Is

        lda #<obj0
        ldy #>obj0
        jsr PRINT

        jsr Print_on_Floor

        rts

@PP_DOll
        lda loc
        cmp #LOC_GORDON
        bne PP_Exit

        ldy #F_DOLL
        jsr read_flag_y
        beq @PP_IDol ; Es Cero
        jsr Print_There_Is

        lda #<doll_obj
        ldy #>doll_obj
        jsr PRINT

        jsr Print_on_Floor

        rts

@PP_IDol
        ldy #F_IDOL
        jsr read_flag_y
        beq PP_Exit ; Es Cero
        jsr Print_There_Is

        lda #<idol_obj
        ldy #>idol_obj
        jsr PRINT

        jsr Print_on_Floor

        rts

Print_There_Is
        lda #<str_there_is
        ldy #>str_there_is
        jsr PRINT

        rts

Print_on_Floor
        lda #<str_on_floor
        ldy #>str_on_floor
        jsr PRINT

PP_Exit
        rts


;------------------------------------------------

InputCommand
        ; Read the keyboard
        ; and try to identify the verb and the name_index
        ; It expects VERB or VERB NAME

        jsr PrintPromt
        jsr Input

        ; Coger 2 caracteres para verbo y 2 para nombre
        jsr ProcessInput
        jsr TokenizeVerb
        jsr TokenizeName
        rts

;---------------
PrintPromt
        lda #RETURN
        jsr PRINT_CHAR

        ; Print '> '
        lda #<prompt
        ldy #>prompt
        jsr PRINT
        rts

;---------------
Input
        ; Stores 0s in the address where the
        ; code will store user's input
        LDX #0
        stx verb+1 ; Limpiar el buffer
        stx name
        stx name+1

LI
        JSR CHIN
        STA INPUT_BUFFER,X ; $200
        INX
        CMP #RETURN ; Cambiar por cte
        BNE LI
        rts

;---------------
ProcessInput

        ldx #0
        ldy #0

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
        ; Salto linea
        lda #RETURN
        jsr PRINT_CHAR ;$ffd2  
        rts

;---------------
TokenizeVerb

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
        ;pla 
        ;sty verb_index
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
        jsr PRINT
        jmp MainLoop
@Exit 
        rts


;---------------
CheckName
        lda name
        beq CN_Exit ; Si es 0 no se escribió nombre
        lda name_index
        bne CN_Exit

; Commands that needs a name comes here if
; there is no name
Wrong_Name
        lda #<invalid_name
        ldy #>invalid_name
        jsr PRINT
        jmp MainLoop
CN_Exit 
        rts


;---------------
CheckVerbNeedsName
        ;  if bverb needs a name
        ; trhen cheks if tehre is a name

        lda verb_index
        ;tax
        sec
        sbc #SINGLE_VERB
        ;sta mem_mon,x
        bcc @End ; No necesita name

        ; Necesita name
        ; Comprobat si tiene
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

; This subroutine takes the hals of the bytes
; Whih has the exists of the loc
MovementCommands
       ;lda loc
        lsr
        tax ; no C
        lda exits,x ; no C
        ;sta tmp_exit ; Cargo el bts de salidas y lo guardo aqui
        tay 

        lda loc
        and #%00000001 
        ; lsr y comprobar el carry. ¿Alguna de las anteriores cmabia el carry?
        ; Ya tengo en el carry si es negativo del lsr
        bne Go_Back
        ;bne Norte
        
        ; Par, cojo los 4 bits más altps
        ;lda tmp_exit
        tya
        lsr
        lsr
        lsr
        lsr
        tay ; Guardo a porque cambia al hacer rts
        ;jmp Norte
Go_Back rts


NoSalida
        ; Decir que no hay salida
        ; nop
        lda #<no_exit
        ldy #>no_exit
        jsr PRINT
        rts
        
; Entry point
Move_CMD 
        jsr MovementCommands
        
        ; Caluclate mask
        lda #$3
        sec
        sbc verb_index

        tax
        tya

        and masks,x
        beq NoSalida ; No hay salida
        

        ldx verb_index

        lda loc
        clc
        adc offset,x
        sta loc

End     
        jmp PrintContent 

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

        ; I'm at the garden'
        ;lda loc
        cmp #LOC_GARDEN
        bne @Next_Search_1

        ; LEaf is hidden
        ldx #LEAF_IND
        jsr Item_is_Hidden
        bne @Next_Search_1

        ; Move LEaf to his loc
        ;dex - Ya lo ha hecho antes
        lda loc
        sta obj_status,x

        jsr PrintObjs
        rts

@Next_Search_1
        ; Search symbols in the lounge

        ; I'm at the lounge
        lda loc
        cmp #LOC_LOUNGE
        bne @Next_Search_2

        ; I das the dream
        ; Flag 1 is set
        ldy #$1
        jsr read_flag_y
        beq @Next_Search_2 ; es cero

        ; You find the symbols under a carpet
        ; You need the buried key and the magic word
        lda #<str_find_symbols
        ldy #>str_find_symbols
        jsr PRINT

        ; USAR UNA BANDERA PARA QUE S EMUESTREN LOS IMBOLOS
        ; SI YA LOS HAS ENCONTRADO.
        rts

@Next_Search_2
        ; search CAndelebra in garden

        ; I'm at the garden
        lda loc
        cmp #LOC_GARDEN
        bne @Next_Search_3

        ; I had the vision
        ; Flag 1 is set
        ldy #$1
        jsr read_flag_y
        beq @Next_Search_3 ; es cero

        ; Chandelier is hidden
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
        jsr PRINT

        jmp SR_You_Die
        ;rts


@Next_Search_4
        ; I'm at the Garage
         lda loc
        cmp #LOC_GARAGE
        bne @Next_Search_5

        ; Trap not found - Flag 3
        ldy #$2
        ;jsr read_flag_y
        ;bne @Next_Search_5
        ; No pasanada si la buscas ma´s d euna vez

        ; Set flag
        ; a already set
        jsr set_flag_y

        ; Message
        lda #<str_there_is
        ldy #>str_there_is
        jsr PRINT

        lda #<str_trapdoor
        ldy #>str_trapdoor
        jsr PRINT

        rts

@Next_Search_5
        ; Kitchen
        lda loc
        cmp #LOC_KITCHEN
        bne @Next_Search_6

         ; Message
        lda #<str_kitchen
        ldy #>str_kitchen
        jsr PRINT

        rts

@Next_Search_6
        jsr P_Useless
        rts


;----------------

P_Done
        lda #<str_done
        ldy #>str_done
        jsr PRINT
        rts

Put_No_Carried
        jmp Ex_No_Carried


Put_CMD
        ; Comprobar que el name esta en tu inventario
        ; Sea cual sea
        ldx name_index
        jsr Item_In_Inventory
        bne Put_No_Carried

        ; -- Put Leaf in Bed

 
@Next_Put_1
        ; -- Put Chandelier in lounge

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
        ;dex
        ldx #CHANDELIER_IND-1
        lda #LOC_DESTROYED ; LOC 3D / 61 - Destroyed
        sta obj_status,x
        ; SI CHANDELIER ESTA DESTRUIDO QUE SE DIGA EN EL SALON

        ; flag
        ldy #F_CHANDELIER
        jsr set_flag_y

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

        ; No es necesario porque ya he comprobado que
        ; lo tengo en el inventario
        ;ldx #IDOL_IND
        ;jsr Lda_Obj_Status
        ;cmp #LOC_DESTROYED
        ;beq @Next_Put_3 ; si está destruido, ya se ha pùesto.

;@Put_back_1
        ; Name is Idol
        ldx name_index
        cpx #IDOL_IND
        bne @Name_is_DOll

        ldy #F_IDOL
        jsr set_flag_y

@Put_back_2
        ; Remove item and write 'Done'
        ldx name_index
        dex

        ;stx mem_mon

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

        ldy #F_DOLL
        jsr set_flag_y

        jmp @Put_back_2


@Next_Put_3 
        jsr P_Useless
        rts



;--- Subroutines for iobject management

;---
Name_Is_Item
        lda name_index
        tax
        
        ; El indice del objeto es su name
        ; si name > numerod e obejtos, entonces mal 
        ; Este chek no está probado
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
        bne Print_No_Item

        ;  I AM NOT SAVING SPECIAL BITS
        lda #LOC_INVENTORY
        sta obj_status,x
        ; Call inventory to confirm
        jsr Inventory_CMD
        rts

Print_No_Item
        lda #<no_item
        ldy #>no_item
        jsr PRINT
        rts 


Take_P_Useless
        jmp P_Useless


;--- Examine ------------------------------

;--------------
Ex_No_Carried
        lda #<no_carried
        ldy #>no_carried
        jsr PRINT

Ex_Exit
        ; Salto linea
        lda #RETURN
        jsr PRINT_CHAR
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
        jsr PRINT
        
        jmp Ex_Exit

        ;
;--------------
Ex_Diary_Library
        ;Name is DIary
        cpx #DIARY_IND
        bne Ex_Next_1

        ;Loc is library
        lda loc
        cmp #LOC_LIBRARY
        bne Ex_Next_1

        ; Print description
        lda #<desc_diary
        ldy #>desc_diary
        jsr PRINT
        jmp Ex_Exit
        
;--------------
Ex_Next_1
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
        jsr PRINT
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

        ; I read de diary
        ; Ro Do or not

        ; Print description
        lda #<desc_mirror
        ldy #>desc_mirror
        jsr PRINT
        jmp Ex_Exit

;--------------
Ex_Gordon_Dinnin
;Name is MIrror
        cpx #GORDON_IND
        bne Ex_Paints

         ;Loc is dinnind room
        lda loc
        cmp #LOC_GORDON
        bne Ex_Paints

       
        ; Print description
        lda #<desc_gordon
        ldy #>desc_gordon
        jsr PRINT
        jmp Ex_Exit


;----
Ex_Paints
        cpx #PAINT_IND
        bne Ex_No_Item

         ;Loc is dinnind room
        lda loc
        cmp #LOC_PAINTS
        bne Ex_No_Item

       
        ; Print description
        lda #<desc_paint
        ldy #>desc_paint
        jsr PRINT
        jmp Ex_Exit

Ex_No_Item
        jmp Print_No_Item


;--- Puzzle commands ---------------------------------

;- Read diary with classes
Read_CMD
        ; loc is librería
        ;lda loc
        cmp #LOC_LIBRARY
        bne P_Useless

        ; Name es diary
        lda name_index
        ;sta mem_mon
        cmp #DIARY_IND
        bne P_Useless

        ; Llevo las gafas en el inve tario
        ldx #GLASSES_IND
        jsr Item_In_Inventory        
        bne P_Understand_Nothing

        ; Imprimir mensaje
        lda #<str_read_diary
        ldy #>str_read_diary
        jsr PRINT

        ; Poner bandera o a 1.
        ; Si lo hago con el bit de status oculto / no 
        ; No necesito banderas
        ldy #$0
        jsr set_flag_y

        rts
        
P_Useless
        lda #<str_useless
        ldy #>str_useless
        jsr PRINT
        rts

P_Understand_Nothing
        lda #<str_understand_nothing
        ldy #>str_understand_nothing
        jsr PRINT
        rts


;--------
Sleep_CMD
         ; loc is bedroom
        ;lda loc
        cmp #LOC_BEDROOM
        bne P_Useless

        ; You read the diary
        ldy #$0 ; Flag 0
        jsr read_flag_y
        beq P_Useless ; Yo dint read the aidary

        ; You put the leaf
        ; Leaf is destroyed
        ldx #LEAF_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne P_Useless

        ; Textod e la vision
        lda #<str_vision
        ldy #>str_vision
        jsr PRINT

        ; Set flag 1
        ldy #F_SLEEP
        jsr set_flag_y

        rts


;-------------------
Eat_CMD
        ldx name_index
        cpx #LEAF_IND
        bne P_Useless

        ; COMPROBar que esta en el inventario
        ;ldx #LEAF_IND ;-1
        jsr Item_In_Inventory
        bne Ex_No_Item

        ; I read the diary
        ; jsr PS_Check_read_Diary
       ; beq @PS_Next_2

        ;ldy #0
        ;jsr read_flag_y
        jsr PS_Check_read_Diary
        beq P_Useless

         ; Remove item and write 'Done'
        ; I could use status bit for destroyed items
        ldx #LEAF_IND-1
        ; x is right
        lda #LOC_DESTROYED ; LOC 3D / 61 - Destroyed
        sta obj_status,x

        ; flag
        ; No se usa porque se mira si LEAF esta destroyed
        ;ldy #F_LEAF
        ;jsr set_flag_y

        ; Mensaje
        jsr P_Done
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
        jsr PRINT

        jmp @Ehe_Restart


@Ehe_End_Doll
        ; Idol
        ldx #DOLL_IND
        jsr Lda_Obj_Status
        cmp #LOC_DESTROYED
        bne @Ehe_Useless

        ; Msg
        lda #<str_good_ending
        ldy #>str_good_ending
        jsr PRINT

        ; You dont die
        ;jmp SR_Press_A_Key

@Ehe_Restart
        jsr SR_Press_A_Key
        jmp Game_Begin 


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
        jsr PRINT

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
        jsr PRINT

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
        jsr PRINT

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
        jsr PRINT

        ; no sé cómo esperar uan puislación así que repito el input
        jsr SR_Press_A_Key
        ; JSR CHIN

        ;brk
        jmp Game_Begin 

;--------

SR_Press_A_Key
        lda #<str_press_return
        ldy #>str_press_return
        jsr PRINT

        jsr Input
        
        rts


;--- Busca un token en una lista
; El token tiene que estar en $Token
; Devuelve el indice del token en a
; empieza por 1, a = 0 es que no lo ha encontrado
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
        ;tay
        ;pha
        
F_Exit  ; a = 0 not found, other found  
        ;tay
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
        ; Solo uso las 8 p`rimeras banderas
        ldx #$0
;        tya
;        and masks+$3
;        beq set_rts
;        inx
;        tya
;        sec
;        sbc #$8
;        tay
set_rts
        rts

set_flag_y
        jsr set_x_y
        lda flags,x
        ora masks,y
        sta flags,x
        rts ; Acumulador


; No lo necesito
;clear_flag_y
;        jsr set_x_y
;        lda masks,y
;        eor #$FF ; Invierto la mask
;        and flags,x
;        sta flags,x
;        rts ; Acumulador

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


;-------------------------
;Debug_BRK
;        brk

;--- Data -------------------------------------------

;--- Storage -------
;*=$fb ; Si lo pongo aqui me convierte la pantalla en @

; For debug 
;mem_mon       BYTE $0, $0, $0, $0, $0, $0, $0

;verb            
; 47 - 48 Variable address
verb = $47

;name            
; 49 - 4a Variable pinter for FOR
name = $49


;token           
; $45 - 46 Current variable name
token = $45

; $71, 72 Cassete buff len

; $73 - 8a CHRGET subroutine
;loc     BYTE    $0
loc = $73

;verb_index      
verb_index = $74

;name_index       
name_index = $75

;flags        
flags = $76
F_SLEEP = $1
F_LEAF = $3
F_CHANDELIER = $4
F_DOLL = $5
F_IDOL = $6

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
PAINT_IND = $d

; Not used: BAll (42 41)
; CHandelier (43 48)
; Objects: CAndelebra (43 41), GLasses, REvolver (52 45), LEaf (4C, 45), IDol (49 44), DOll (44, 4f)
; No Objs: DIary (44 49), BEd (42 45), MIrror (4D 49), TOm (54 4f), MOnster (4D, 4f), TRapdoor (54, 52)
; PAints ($50, $41) - Como revolver
; GOrdon (47 4f)
name_tokens     BYTE $43, $41, $47, $4C, $52, $45, $4C, $45, $49, $44, $44, $4f
                BYTE $44, $49, $42, $45, $4d, $49, $54, $4f, $4d, $4f, $54, $52
                BYTE $50, $41, $0 

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
                ; EAt (45, 41)
                BYTE $45, $41
                BYTE $0  

verb_pointers   WORD  Move_CMD, Move_CMD, Move_CMD, Move_CMD, Inventory_CMD, Look_CMD, Sleep_CMD, Search_CMD
                WORD  Ehe_CMD, SR_You_Die
                WORD  Take_CMD, Examine_CMD, Read_CMD, Put_CMD, Shoot_CMD, Open_CMD
                WORD  Eat_CMD

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
str_in_bed      TEXT " in bed", RETURN, $0
str_on_floor    TEXT RETURN,"on the floor", RETURN, $0

; No puede haber ningún objeto en loc 0 con los bits finales a 0
; 3F en inventario                              ; $BF
; Los dos primeros bits sond e status pero no los estoy usando.
; 1: 2: 3: Invetopry, 4: Hidden
;                    Chandelier: Glaasses, Revolver,  LEaf,      IDol,          Doll
obj_status      BYTE %00111111, %00000100, %10111111, %00111110, %00001101, LOC_DESTROYED, $0
obj_list        WORD obj0, obj1, obj2, leaf_obj, idol_obj, doll_obj
obj0
        TEXT    " candelebra"
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
        TEXT    "you see everything"
        BYTE    RETURN
        text    "fuzzy"
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
        TEXT    "written in fuzzy"
        BYTE    RETURN
        text    "characters"
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
        BYTE    $0
desc_gordon      
        TEXT    "floor has the same"
        BYTE    RETURN
        TEXT    "symbols than the", RETURN
        text    "lounge"
        BYTE    $0
desc_paint
        TEXT    "figures are the keys"
        BYTE    RETURN
        TEXT    "to many dimensions"
        BYTE    $0


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
LOC_PAINTS = $c
LOC_INVENTORY=%00111111 ; LOC de los obj del inventario (3F)
LOC_HIDDEN = %00111110 ; LOC de los obj ocultos(3F)
LOC_DESTROYED = %00111101 ; LOCd e los obj que ya nos e usaran más


desc_upsidedown TEXT "everything is upside  down", $0 ; Extra space

; N S E O              0   1      2   3      4   5      6   7      8   9      10  11     12  13
exits           BYTE %10101011, %00011110, %11110001, %01000110, %00010010, %10110001, %00100101 
;; oofsets for movement: N, S, E, W
offset          BYTE $3, %11111101, $1, %11111111
desc_list WORD desc0, desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8 
          WORD desc9, desc10, desc11, desc12, desc13
; desc 12 is not used
desc0
        TEXT    "desktop"
        BYTE    $0 ; Hace un salto de linea.

desc1   ; gateway
        TEXT    "foyer"
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
        TEXT    "dining room"
        BYTE    $0
desc8
        TEXT    "kitchen"
        BYTE    $0

desc9
        TEXT    "room"
        BYTE    RETURN
        TEXT    "with a big blob of"
        BYTE    RETURN
        text    "meat"
        BYTE    RETURN
        TEXT    "it is tom"
        BYTE    $0

desc10
        TEXT    "same"
        BYTE    RETURN
        text "house in a diferent"
        BYTE    RETURN
        text "dimension"

        BYTE    $0

desc11
        TEXT    "nest ofdimensional" ; Dont fix the space
        BYTE    RETURN
        TEXT    "monsters"
        BYTE    $0

desc12 
        TEXT "half ofpaints" ; Dont fix the space
        BYTE    $0

desc13
        TEXT    "temple of madness"
        BYTE    $0


;--- Specials ---------

str_there_is
        TEXT    "there is a"
        BYTE    $0

bib_diary
        TEXT    " diary"
        BYTE    RETURN, $0

str_useless
        TEXT    "useless"
        BYTE    RETURN, $0

str_understand_nothing
        TEXT    "you understand nothing"
        BYTE    RETURN, $0

str_read_diary
        TEXT    "it talks about dreams"
        BYTE    RETURN
        TEXT    "that became into"
        BYTE    RETURN
        text    "visions of many"
        BYTE    RETURN
        text "dimensions"
        BYTE    RETURN, $0

str_bed
        TEXT    " bed"
        BYTE    RETURN, $0

str_mirror
        TEXT    " mirror"
        BYTE    RETURN, $0

str_trapdoor
        TEXT    " trapdoor"
        BYTE    RETURN, $0

; Reescribir
str_vision
        ;TEXT "You see that dreams are visions of ther realities"
        ;BYTE    RETURN
        TEXT "you dream the symbols"
        BYTE    RETURN
        TEXT  "in the lounge to go"
        BYTE    RETURN
        text "to other dimension"
        BYTE    RETURN
        TEXT "you dream tom buring"
        BYTE    RETURN
        text "the key"
        BYTE    RETURN
        TEXT "you dream you need a" 
        BYTE    RETURN
        TEXT "magic word from other"
        BYTE    RETURN
        TEXT "dimension"
        BYTE    RETURN, $0

str_find_symbols
        TEXT "you find the symbols"
        BYTE    RETURN
        TEXT "under a carpet"
        BYTE    RETURN
        TEXT "you need the buried"
        BYTE    RETURN
        text"key and the magic word"
        BYTE   $0 ; No need return

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
        TEXT "mosnters stop eating"
        BYTE    RETURN
        TEXT "dimensions"
        BYTE    RETURN
        TEXT "they eat you"
        BYTE    RETURN, $0

str_open_trapdoor
        ; The smell makes you puke
        TEXT "you find a hole"
        BYTE    RETURN
        TEXT "where tom locked up"
        BYTE    RETURN
        TEXT "the kidnapped children"
        BYTE    RETURN, $0

str_kitchen
        TEXT "you puke when you see"
        BYTE    RETURN
        TEXT "the leftovers of the"
        BYTE    RETURN
        TEXT "meals of tom"
        BYTE    RETURN, $0

;--- Introductory text ---------------

str_introduction_1
        ; pursuing, chasing
        TEXT "pursuing tom ram"
        BYTE    RETURN
        BYTE    RETURN

        TEXT "fifteen children were"
        BYTE    RETURN
        TEXT "missing in the last"
        BYTE    RETURN
        TEXT "ten years."
        BYTE    RETURN
        TEXT "they were poor and"
        BYTE    RETURN
        TEXT "nobody cared but you"
        BYTE    RETURN
        BYTE    RETURN
        BYTE    $0

str_introduction_2
        TEXT "you are the detective"
        BYTE    RETURN
        TEXT "who investigate the"
        BYTE    RETURN
        TEXT "missings"
        ;" of children."
        BYTE    RETURN

        TEXT "you follow tom ram"
        BYTE    RETURN
        TEXT "a lonely retired."
        BYTE    RETURN
        TEXT "you go to his house"
        BYTE    RETURN
        TEXT "to question him"
        BYTE    RETURN
        BYTE    $0

;--- Endings ----------------------

str_good_ending
        TEXT "tom shouts when he"
        BYTE    RETURN
        TEXT "travels to another"
        BYTE    RETURN
        TEXT "dimension where the"
        BYTE    RETURN
        TEXT " childrens are now"
        BYTE    RETURN
        TEXT "they are not defence-"
        BYTE    RETURN
        TEXT "less anymore"
        BYTE    RETURN
        TEXT "you wake up in the"
        BYTE    RETURN
        TEXT "house of tom"
        BYTE    RETURN, RETURN
        TEXT "case closed"
        BYTE    RETURN
        BYTE    $0

str_bad_ending
        TEXT "you open the path to"
        BYTE    RETURN
        TEXT "the deity of madness"
        BYTE    RETURN
        TEXT "that slowly devours"
        BYTE    RETURN
        TEXT "your world while tom"
        BYTE    RETURN
        TEXT "will laugh forever"
        BYTE    RETURN
        BYTE    $0

;*=$71

;*=$73
;        rts