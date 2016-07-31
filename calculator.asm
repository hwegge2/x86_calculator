; multi-segment executable file template.

data segment
    SUM DB 4 DUP(0)
    NOTSUM db 4 dup(0)
    NOTDIFF db 4 dup(0)
    DIFF1 DB 4 DUP(0)
    MULT db 4 dup(0)
    NUMBER DB 12 DUP(0)
    DECIMAL_IN_ASCII db 11 dup(0)
    A DB 4 DUP(0)
    usenegmult db 0
    usenotsum db 0
    usenotdiff db 0
    Aneg db 4 dup(0)
    result db 11 dup(0)
    asign db 0
    bsign db 0
    B DB 4 DUP(0)
    Bneg db 4 dup(0)
    enterx db "  X: $"
    entery db "  Y: $"
    xplusy db "  X + Y = $"        ;changed from a,b to x,y
    xminusy db "  X - Y = $"       ;to reflect new program
    xtimesy db "  X * Y = $"
    blanks db 21 dup(' '), '$'
    currentcmd db 00h            
    NL	DB	10,13,'$'
    SCREEN	DB	'          MENU          ',10,13   
	DB	'   1 - Enter Values',10,13
	DB	'   2 - Add',10,13
	DB	'   3 - Subtract',10,13
	DB	'   4 - Multiply',10,13
	DB	'   Q - QUIT',10,13
	DB	13,10,10,'  Enter choice of operation:$'
ends

stack segment
    dw   128  dup(0)
ends

code segment
main proc far
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax ;
    
    mov ah, 0h
    mov al, 3h   
    call menu
    
    CALL NEW_LINE              ;moved "BEGIN" to be main program
    MOV DX,OFFSET SCREEN
    CALL MESSAGE
    
    REDO:
    mov ah, 2
    mov bh, 0
    mov dh, 10       ;set cursor to where user enters choice
    mov dl, 29
    mov bh, 0
    int 10h
       
    CALL GET_CHAR
    CMP AL,'1'
    JNE REDO1
    
    mov asign, 0
    mov bsign, 0
    mov usenotsum, 0
    mov usenotdiff, 0
    mov usenegmult, 0
    
    call CLR_PRV_OUTPUTS
    call NEW_LINE
    call NEW_LINE
    mov ah, 9h
    mov dx, offset enterx
    int 21h
    mov ch, 2
    CALL INPUT_NUMBER
    call CONV_ASC2BIN
    
    call NEW_LINE
    mov ah, 9h
    mov dx, offset entery
    int 21h
    call INPUT_NUMBER
    mov ch, 1
    call CONV_ASC2BIN
    JMP REDO
    
    REDO1:
    CMP ah,10h
    JE QUIT
    call EVALUATE
    JMP REDO
    
    QUIT:
    MOV AH,4CH
    INT 21H                      
    
    mov ax, 4c00h ; exit to operating system.
    int 21
    ret
main endp   
    
;-------------------------------------------------  

PRODUCT_A_B proc near
    push cx
    push dx
    push bx
    push ax
    mov cx, 1
    mov dx, 19
    
    mov SI,offset A 
    mov BX,offset B
    mov DI,offset MULT

    mulagain:
    mov bx, offset B
    mov ax, [bx]
    AND ax, cx
    jz skip
 
    mov si, offset A
    mov bx, offset mult
    call ACCUM_MUL

    skip:
    shl cx, 1
    call SHIFT_A
    dec dx
    jnz mulagain
    pop ax
    pop bx
    pop dx
    pop cx 
    ret
PRODUCT_A_B endp

SHIFT_A proc near
    push si
    push ax
    push cx
    mov si, offset A
    mov ax, [si]
    shl ax, 1
    mov [si], ax
    mov ax, [si+2]
    rcl ax, 1
    mov [si+2], ax
    pop cx
    pop ax
    pop si
    ret
SHIFT_A endp

ACCUM_MUL proc near
    push cx                     ;A into si
    push ax                     ;B into bx, out into bx
    push si                    
    push bx
    push dx
    mov cx, 4
    mov ax, 0
    clc
    mov al, [si]
    adc al, [bx]
    mov dl, al
    inc si
    inc bx
    dec cx
    
    mov al, [si]
    adc al, [bx]
    mov dh, al
    inc si
    inc bx 
    dec cx
    
    push dx
    mov dx, 0
    
    mov al, [si]
    adc al, [bx]
    mov dl, al
    inc si
    inc bx 
    dec cx
    
    mov al, [si]
    adc al, [bx]
    mov dh, al
    inc si
    inc bx
    dec cx
    
    mov bx, offset mult
    mov [bx+2], dx
    pop dx
    mov [bx], dx
    
    pop dx
    pop bx
    pop si
    pop ax
    pop cx
    ret 
ACCUM_MUL endp
  
EVALUATE proc near
    cmp currentcmd, '4'
    je multiply
    
    cmp asign, 0
    je testB
    lea di, A
    lea si, Aneg 
    call COMPLEMENT
    
    testB:
    cmp bsign, 0
    je addsubtract
    lea di, B
    lea si, Bneg
    call COMPLEMENT
    
    addsubtract:
    cmp currentcmd, '3'
    je subtract

    cmp asign, 1
    je chos_aneg
    mov si, offset A
    jmp chosb
    chos_aneg:
    mov si, offset Aneg
    
    chosb:
    cmp bsign, 1
    je chos_bneg
    mov bx, offset B
    jmp addandsubtract
    chos_bneg:
    mov bx, offset Bneg
    
    addandsubtract:
    cmp bsign, 1
    je subtractnotadd
    mov di, offset SUM
    call ADD_A_B
    jmp skipother
    
    subtractnotadd:
    mov si, offset Aneg
    cmp asign, 1
    je contin
    mov si, offset A
    contin:
    mov bx, offset B
    mov di, offset SUM
    call SUB_A_B
    
    skipother:
    lea di, sum
    cmp [di+3], 0FFh
    jne conv2dec:
    lea si, NOTSUM
    call COMPLEMENT
    lea si, usenotsum
    mov [si], 1
    
    conv2dec:
    cmp usenotsum, 0
    je sumpositive
    lea si, notsum
    jmp conv2asc
    sumpositive:
    lea si, sum
    
    conv2asc:
    cmp currentcmd, '3'
    je subtract    
    call BIN2DEC2ASC
    call DISPLAY_ANS
    jmp done_eval
    
    subtract:
    cmp bsign, 0
    je subtractlikenormal
    mov bx, offset B
    mov di, offset diff1
    mov si, offset A
    cmp asign, 0
    je addd
    mov si, offset Aneg
    addd: 
    call ADD_A_B
    jmp donesubtracting
    
    subtractlikenormal:
    mov bx, offset B
    mov si, offset A
    cmp asign, 0
    je subtractposA
    mov si, offset Aneg
    
    subtractposA:
    mov di, offset DIFF1
    call SUB_A_B
    
    donesubtracting:
    lea di, diff1
    cmp [di+3], 0FFh
    jne convB2dec
    lea si, NOTDIFF
    call COMPLEMENT
    lea si, usenotdiff
    mov [si], 1
    
    convB2dec:
    cmp usenotdiff, 0
    je diffpositive
    lea si, notdiff
    jmp convB2asc
    diffpositive:
    lea si, diff1
    
    convB2asc:
    call BIN2DEC2ASC
    CALL DISPLAY_ANS
    jmp done_eval
    
    multiply:
    call PRODUCT_A_B
    mov al, asign
    add al, bsign   
    cmp al, 0
    je positivemult
    cmp al, 2
    je positivemult    
    mov usenegmult, 1
    
    positivemult:
    lea si, MULT
    call BIN2DEC2ASC
    call DISPLAY_ANS
    jmp done_eval    

    done_eval:
    ret    
EVALUATE endp

DISPLAY_ANS proc near
    mov ah, 2
    mov bh, 0
    mov dh, 15       ;set cursor to correct area
    mov dl, 0
    mov bh, 0
    int 10h
    
    mov ah, 9
    mov dx, offset blanks  ;clear line
    int 21h
    
    mov ah, 2
    mov bh, 0
    mov dh, 15       ;set cursor back to correct area
    mov dl, 0
    mov bh, 0
    int 10h
    
    cmp currentcmd, '2'
    jne notadding
    mov ah, 9
    mov dx, offset xplusy
    int 21h
      
    cmp usenotsum, 0
    je displaysum
    mov ah, 2
    mov dl, '-'                ;display '-' for negative sum
    int 21h
    displaysum:
    mov ah, 9
    mov dx, offset DECIMAL_IN_ASCII
    int 21h
    jmp done_display
    
    notadding:
    cmp currentcmd, '3'
    jne notsubtracting    
    mov ah, 9
    mov dx, offset xminusy
    int 21h
    
    cmp usenotdiff, 0
    je displayminus2
    mov ah, 2
    mov dl, '-'                ;display '-' for negative diff
    int 21h
    
    displayminus2:
    mov ah, 9
    mov dx, offset DECIMAL_IN_ASCII
    int 21h
    jmp done_display
    
    notsubtracting:
    mov ah, 9
    mov dx, offset xtimesy
    int 21h
    
    cmp usenegmult, 0
    je skip3
    mov ah, 2
    mov dl, '-'                ;display '-' for negative diff
    int 21h
    
    skip3: 
    mov ah, 9
    mov dx, offset DECIMAL_IN_ASCII
    int 21h

    done_display:
    ret
DISPLAY_ANS endp    

MENU proc near
        push    ds      
        push    di      

        mov     ax, 0040h
        mov     ds, ax  ; for getting screen parameters.
        mov     ah, 06h ; scroll up function id.
        mov     al, 0   ; scroll all lines
        mov     bh, 1001_1110b  ; attributes blue bg & yellow font
        mov     ch, 0   ; upper row.
        mov     cl, 0   ; upper col.
        mov     di, 84h ; rows on screen -1,
        mov     dh, [di] ; lower row (byte).
        mov     di, 4ah ; columns on screen,
        mov     dl, [di]
        dec     dl      ; lower col.
        int     10h

        ; set cursor position to top
        ; of the screen:
        mov     bh, 0   ; current page.
        mov     dl, 0   ; col.
        mov     dh, 0   ; row.
        mov     ah, 02
        int     10h

        pop     di      ; re-store registers...
        pop     ds      ;
        
        MOV AX,0     ;reset registers
	    MOV BX,0
	    MOV CX,0
	    MOV DX,0
	    ret           
MENU endp

NEW_LINE PROC NEAR
    PUSH AX
    PUSH DX
    MOV DX,OFFSET NL
    MOV AH,9
    INT 21H
    POP DX
    POP AX
    RET
NEW_LINE ENDP

MESSAGE PROC NEAR
    PUSH AX
    CALL NEW_LINE
    MOV AH,9
    INT 21H
    POP AX
    RET
MESSAGE ENDP


GET_CHAR PROC NEAR
	MOV AH,0          ;changed to scan code
	INT 16H
	mov currentcmd, al           ;clears input
	RET
GET_CHAR ENDP
                              

ADD_A_B proc near
    push cx                     ;A into si
    push ax                     ;B into bx
    push si                     ;output into di
    push bx
    mov cx, 4
    mov ax, 0
    clc
    adding: mov al, [si]
    adc al, [bx]
    mov [di], al
    inc si
    inc bx
    inc di 
    dec cx
    jnz adding
    pop bx
    pop si
    pop ax
    pop cx
    ret 
ADD_A_B endp 

            
SUB_A_B proc near
    push cx
    push ax
    push si
    push bx
    mov cx, 4
    mov ax, 0
    clc
    subtracting: mov al, [si]
    sbb al, [bx]
    mov [di], al
    inc si
    inc bx
    inc di 
    dec cx
    jnz subtracting
    pop bx
    pop si
    pop ax
    pop cx
    ret            
SUB_A_B endp

INPUT_NUMBER proc near
    mov di, offset NUMBER
    mov [di], 10
    mov ah, 0Ah
    mov dx, offset NUMBER
    int 21h
    ret        
INPUT_NUMBER endp
   
CONV_ASC2BIN proc near
    mov si, offset NUMBER
    mov cl, [si+1]
    
    cmp [si+2], '-'
    jne positive        ;jump past -, dec length
    inc si
    dec cl
    cmp ch, 1
    jg anegative
    mov bsign, 1
    jmp positive
    anegative:
    mov asign, 1
    
    positive:
    cmp cl, 1
    jnz continue     ;fixed inputs with only 1 digit
    push [si+2]
    mov [si+2], 30h
    pop dx
    mov [si+3], dx  
    mov cl, 2
    mov dx, 0
     
    Continue:
    dec cl      ;end fix
    add si, 2
    mov ax, 0
    mov bp, 10
    clc
    mov dx, 0
    conv: mov bl, [si]
    sub bl, 30h
    
    add ax, bx
    adc dx, 0
    
    push ax
    mov ax, dx
    mul bp
    
    mov di, ax
    pop ax
    mul bp
    add dx, di
        
    inc si
    dec cl
    jnz conv
    clc
    dec ch
    jz Bnum
    mov bl, [si]
    sub bl, 30h
    add al, bl
    adc ah, 0
    adc dx, 0
    mov si, offset A
    mov [si], ax
    mov [si+2], dx
    ret
    Bnum: mov bl, [si]
    sub bl, 30h
    add al, bl
    adc ah, 0
    adc dx, 0
    mov si, offset B
    mov [si], ax
    mov [si+2], dx 
    ret
CONV_ASC2BIN endp

CLC_SCREEN proc near
    push ax
    mov ax, 0003h
    int 10h
    pop ax
    ret
CLC_SCREEN endp 

COMPLEMENT proc near
    mov ax, [di]
    mov dx, [di+2]            ;original # at DI

    mov bx, 0FFFFh
    sub bx, dx
    mov cx, 0FFFFh
    sub cx, ax

    add cx, 1
    adc bx, 0
          
    mov [si], cx           ;result in SI
    mov [si+2], bx
    ret
COMPLEMENT endp

BIN2DEC2ASC proc near
    mov bp, 000Ah            ;use si as input
    lea di, result
    
    againb2d:
    mov dx, 0
    mov ax, [si+2]
    div bp
    
    mov [si+2], ax
    mov ax, [si]
    div bp
    mov [si], ax
    
    mov [di], dx
    add [di], 30h
    
    cmp ax, 0
    je nextb2d
    inc di
    jne againb2d
    
    nextb2d:
    lea si, result
    lea bx, DECIMAL_IN_ASCII
    switchb2d:
    mov al, [di]
    mov [bx], al
    inc bx
    dec di
    cmp di, si
    jnl switchb2d
    mov [bx], '$'
    ret
BIN2DEC2ASC endp

CLR_PRV_OUTPUTS proc near
    push dx
    mov ah, 2
    mov bh, 0
    mov dh, 15    
    mov dl, 0
    mov bh, 0         ;set cursor to last output 
    int 10h
    
    mov ah, 9
    mov dx, offset blanks
    int 21h                 ;write spaces
    
    mov ah, 2
    mov bh, 0
    mov dh, 13    
    mov dl, 0
    mov bh, 0         
    int 10h
    
    mov ah, 9
    mov dx, offset blanks
    int 21h
    
    mov ah, 2
    mov bh, 0
    mov dh, 12    
    mov dl, 0
    mov bh, 0         
    int 10h
    
    mov ah, 9
    mov dx, offset blanks
    int 21h
 
    mov ah, 2
    mov bh, 0
    mov dh, 10       ;set cursor to where user enters choice
    mov dl, 29
    mov bh, 0
    int 10h
    pop dx
    ret
CLR_PRV_OUTPUTS endp
ends

end main ; set entry point and stop the assembler.
