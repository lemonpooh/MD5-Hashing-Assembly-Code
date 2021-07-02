; Program template

.386
.model flat,stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword
DumpRegs PROTO

;include Irvine32.inc    ; from Irvine CDROM
 
.data 
;----------------------------------------------------------------------------------------------
org 0000H
cr EQU 0dh
lf EQU 0ah
usermsg     byte "Enter username: ",0
passmsg     byte "Enter password: ",0
cpassmsg    byte "Enter Confirm password: ",0
notmatchmsg    byte "                        Password not match!",cr, lf,0 
menu1       byte "    1.Login",cr, lf,
                 "    2.Reguster",cr, lf, cr, lf,
                 "    Choose option: ",0
menu2	    byte "-----------menu------------",cr, lf, cr, lf,
             "1.menu1",cr, lf,
             "2.menu2",cr, lf,
             "3.menu3",cr, lf,
             "4.menu4",cr, lf,
             "5.menu5",cr, lf,
             "6.menu6",cr, lf, cr, lf,
             "Choose your option : ", 0 
invalidmsg  byte "Option invalid!",0
regsuccess  byte "                          register success~",0
loginsuccess byte "                         login success~",0
regg  byte "~~~~~~~~~~~~~~~~~~~~~~~Register~~~~~~~~~~~~~~~~~~~~~~~",0 
logg  byte "~~~~~~~~~~~~~~~~~~~~~~~Login~~~~~~~~~~~~~~~~~~~~~~~~~~",0
 
TABLE_T dword  d76aa478h, e8c7b756h, 242070dbh, c1bdceeeh
        dword  0f57c0fafh, 04787c62ah, 0a8304613h, 0fd469501h, 0698098d8h
        dword  08b44f7afh, 0ffff5bb1h, 0895cd7beh, 06b901122h, 0fd987193h
        dword  0a679438eh, 049b40821h, 0f61e2562h, 0c040b340h, 0265e5a51h
        dword  0e9b6c7aah, 0d62f105dh, 002441453h, 0d8a1e681h, 0e7d3fbc8h
        dword  021e1cde6h, 0c33707d6h, 0f4d50d87h, 0455a14edh, 0a9e3e905h
        dword  0fcefa3f8h, 0676f02d9h, 8d2a4c8ah, fffa3942h, 08771f681h
        dword  06d9d6122h, 0fde5380ch, 0a4beea44h, 04bdecfa9h, 0f6bb4b60h
        dword  0bebfbc70h, 0289b7ec6h, 0eaa127fah, 0d4ef3085h, 004881d05h
        dword  0d9d4d039h, 0e6db99e5h, 01fa27cf8h, c4ac5665h, 0f4292244h
        dword  0432aff97h, 0ab9423a7h, 0fc93a039h, 655b59c3h, 08f0ccc92h
        dword  ffeff47dh, 085845dd1h, 06fa87e4fh, 0fe2ce6e0h, 0a3014314h
        dword  04e0811a1h, 0f7537e82h, 0bd3af235h, 02ad7d2bbh, 0eb86d391h

SHIFT_AMTS  dword  7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22
            dword  5,9,14,20,  5,9,14,20,  5,9,14,20,  5,9,14,20
            dword  4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23 
            dword  6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21
A_ EQU 067452301H
B_ EQU 0efcdab89H
C_ EQU 098badcfeH
D_ EQU 010325476H
cleareax TEXTEQU <xor eax, eax>
clearebx TEXTEQU <xor ebx, ebx>
clearecx TEXTEQU <xor ecx, ecx>
clearedx TEXTEQU <xor edx, edx>
clearesi TEXTEQU <xor esi, esi> 
clearedi TEXTEQU <xor edi, edi>
clearebp TEXTEQU <xor ebp, ebp> 

atemp  dword ?
btemp  dword ?
ctemp  dword ?
dtemp  dword ?
x_ dword ?
str16  byte "0123456789abcdef",0 
MM  dword 64 DUP(0)
TTT byte "T[]: ", 0
MMM byte "MMM[]", 0

strByte dword 32 dup(0)
strlenn byte ?
f1 dword ?
g1 byte ?
addnum dword ?
five byte 5
three byte 3
sixteen byte 16
seven byte 7
shiftvarp dword ?
tempe dword ?


pass1_r           byte "imsohandsome",0
cpass             byte "123412341234",0 
pass3_hex_str     byte 33 dup(0)

pass4_hexcheck_I byte 33 dup(0)
pass4_hexcheck_II byte 33 dup(0) 

hashmsg          byte "Password MD5hash:  ", 0
strr1            word  ?
strr             byte 8 dup(?)
username         byte  100 dup(?)        
username2        byte  100 dup(?) 
count            byte 7
countpass        byte 2


;----------------------------------------------------------------------------------------------------------

FF macro  b,c,d,f1
mov ESI, b
mov EDI, b
not ESI
and EDI, c
and ESI, d
or EDI, ESI
MOV f1, EDI
 endm FF

GG macro  b,c,d,f1 
mov esi, b
and esi, d
mov edi, d
not edi
and edi, c
or edi, esi
mov f1, edi
 endm GG


HH macro  b,c,d,f1 
mov edi, b
xor edi, c
xor edi, d
mov f1, edi
 endm HH


II macro  b,c,d,f1 
mov edi, d
not edi
or edi, b
xor edi, c
mov f1, edi
 endm II
  
shiftx macro x,n
push ecx
mov esi, x
mov ecx, n
shl esi, cl
mov ecx, 20h
sub ecx, n
mov edi, x
shr edi, cl
or esi, edi
pop ecx
  endm shiftx

  

;*********************************************************************************************************************************************************************************
.code
main proc 
mov eax,00h 
mov ebx,00h 
mov ecx,00h 
mov edx,00h
mov esi,00h 
mov edi,00h 
mov ebp,00h 
;mov esp,00h 
;*************************************************************************  *main*  ************************************************************************************************* 
   
loop1:        
        call clrscr
        call crlf
        call crlf
        call crlf
        mov edx, offset menu1
        call writestring
        call readint 

        cmp al, 1
        jnz nextI
        jmp logins
        mov ecx, 0
        jmp endingloop

        nextI:
        cmp al, 2
        jnz nextII
        jmp regs
        mov ecx, 0
        jmp endingloop

        nextII:
        jmp invalid  

invalid:
        mov edx, offset invalidmsg
        call crlf
        call writestring
        call crlf
        call waitmsg
        jmp out_

endingloop:
        loope loop1    



;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 
regs:

call clrscr
mov edx, offset regg 
call crlf
call crlf
call  writestring 
call crlf
clearedx
mov edx, offset usermsg           ;enter username
call crlf
call  writestring 
mov edx, offset username[0]
mov ecx, sizeof username
call readstring 
;call writestring
clearedx


mov edx, offset passmsg          ;enter user password
call crlf
call writestring
cld
mov edx, offset pass1_r
mov ecx, sizeof pass1_r
call readstring 
;call writestring 

mov edx, offset cpassmsg
call crlf
call writestring
mov edx, offset cpass
mov ecx, sizeof cpass
call readstring 
;call writestring 

mov esi, offset cpass          ;compare user login password and confirm password
mov edi, offset pass1_r
mov ecx, lengthof pass1_r
repe cmpsb
je match
jmp unmatch

match:
     mov esi,00h
     mov cx, lengthof pass1_r -1
     mov edx, offset pass1_r

                                                   ;change to small letter
again:
     mov al, byte ptr [pass1_r+esi]
     cmp al, 0
     je encryp
     cmp al, 'a'                                   ; 41-61= -20
     jae nothingchange
     add al, 20h 
 
     mov  byte ptr [pass1_r+esi], al 
     inc esi
     jmp again 
 nothingchange:
     mov  byte ptr [pass1_r+esi], al
     inc esi
     jmp again


encryp:

    call md555 
;------------------------------------------------------------------------------------REMOVEEEEEEEEEEEEEEEEEEE

    mov edx, offset hashmsg
    call writestring
    call crlf    
    call crlf  
    lea edx, pass3_hex_str
    call writestring
    call crlf

;------------------------------------------------------------------------------------REMOVEEEEEEEEEEEEEEEEEEE

                              ;if match then call encryption 
    call crlf 
    call crlf 
    mov edx, offset regsuccess
    call writestring
    call crlf 
    call crlf 
    call crlf 
    call waitmsg

    xor esi, esi
    xor edi, edi
    cleareax
    clearebx
    clearecx
    clearedx
    jmp endregs


unmatch:
mov edx, offset notmatchmsg
call crlf
call crlf
call writestring 

endregs:
jmp loop1



;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 

logins:

call clrscr
mov edx, offset logg  
call crlf
call crlf
call  writestring 
call crlf
mov edx, offset usermsg              ;read username input
call crlf
call  writestring 
mov edx, offset username2
mov ecx, sizeof username2
call readstring   
;call writestring 

mov edx, offset passmsg             ;read user password input
call writestring
mov edx, offset pass1_r
mov ecx, sizeof pass1_r
call readstring   
;call writestring 

 
 
mov esi,00h
mov cx, lengthof pass1_r -1
mov edx, offset pass1_r
                                                          ;change to small letter
again2:
             mov al, byte ptr [pass1_r+esi]
             cmp al, 0
             je encryp2
             cmp al, 'a'                                   ; 41-61= -20
             jae nothingchange2
             add al, 20h 
 
             mov  byte ptr [pass1_r+esi], al 
             inc esi
             jmp again2 
 nothingchange2:
             mov  byte ptr [pass1_r+esi], al
             inc esi
             jmp again2

                                       ;change back count to 7 from reg, purpose to perform changehex module again, to store a new hash value
mov eax, 7
mov count, al


encryp2:
call md555                             ;...................calling md555



lea esi, pass4_hexcheck_II             ;compare user password and password hash   
lea edi, pass4_hexcheck_I 
mov ecx, lengthof pass4_hexcheck_I
repe cmpsb
jne unmatch2 




;------------------------------------------------------------------------------------REMOVEEEEEEEEEEEEEEEEEEE

mov edx, offset hashmsg
call writestring
call crlf    
call crlf  
lea edx, pass4_hexcheck_II
call writestring
call crlf
 ;------------------------------------------------------------------------------------REMOVEEEEEEEEEEEEEEEEEEE                                    



lea edx, [loginsuccess]
call crlf
call crlf
call writestring
jmp menuside

;????????????????????????????   MOVE HEREE        ????????????????????????????????????
                          

 
;????????????????????????????   MOVE HEREE        ????????????????????????????????????

jmp endlogin
unmatch2:
        mov edx, offset notmatchmsg
        call writestring
        call crlf
        call crlf 
        call waitmsg
        jmp endlogin

endlogin: 
jmp loop1


 ;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 out_:
  exit         
main endp
 

;==============================================================       md555 module       =================================================================================
md555 proc
push ebp
mov ebp, esp  
pushad
getMD5:                  
          ;cleareax   
addpadding :

                                                   ;num = ((pass1_r.length() + 8) / 64) + 1
          mov eax, lengthof pass1_r
          dec eax
          add eax, 8
          shr eax, 6  ;/64
          inc eax     ;num=eax=1
          mov addnum, eax
                                                  ;strByte = new unsigned int[num * 16]          
          Mov esi, offset strbyte
                                                  ;strlength = num * 16
          mov eax, addnum
          shl eax, 4
          mov strlenn, al

           
          clearecx   ;cx=0
For1:           
            Cmp  cl,[strlenn]-1
            Jae endfor1                          ;jump/breaktheloop if more than equal to 
            mov esi, offset strbyte
                                                 ;strByte[i] = 0;
            Mov dword ptr [esi+ecx*type strbyte], 0
            Inc cl 
            jmp for1
            mov eax, [esi]
            call writeint
endfor1:

            clearecx

            mov esi, offset pass1_r  
            lea edi, [strbyte]      
For2:
            cmp ecx, lengthof pass1_r
            jae endloop2           
                        
                                                 ;(i % 4)
            clearedx
            mov eax, ecx                         ;i
            mov ebx, 4
            div ebx
            mov eax, edx
                                                 ;((i % 4) * 8)
            shl eax, 3
            push eax
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; lea esi, pass1_r    ;;
           ;; lea edi, [strbyte]  ;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;
                                                 ; (pass1_r[i])            
            mov bl, byte ptr[esi+ecx*type pass1_r] 
                                                 ; (str[i])  << ((i % 4) * 8)]
            pop eax
            push ecx
            mov cl, al
            shl bl, cl
            pop ecx         
                                                 ; [i >> 2]
            mov eax, ecx
            shr eax, 2
                                                 ;strByte[i >> 2] | (str[i])   ;OR to move char into strbyte
            movsb         
            inc ecx
            jmp for2
endloop2:

;...........................................................................................................................................
;strByte[str.length() >> 2] = strByte[str.length() >> 2] |  0x80 << ((str.length() % 4) * 8)
                                           ; str.length() % 4
clearedx
mov eax, lengthof pass1_r
dec eax
mov ebx, 4
div ebx
mov eax, edx

                                           ; (((str.length() % 4)) * 8)
mov ecx, eax
mov eax, 80h
shl eax, cl
 

mov ebx, lengthof pass1_r
shr ebx, 2
mov ecx, type strbyte
push eax
mov eax, ebx
mul ecx
pop ebx
or strbyte[eax], ebx

            

;...........................................................................................................................................
;strByte[num*16-2] = str.length() * 8;

mov ebx, lengthof pass1_r
dec ebx
shl ebx, 3 
mov eax, addnum
rol eax, 4
sub eax, 2
shl eax, 2
mov byte ptr strbyte[eax], bl

 
mov eax, strbyte[56]
;call writeint  


clearecx ;ecx=0 

formd5_1:         
        cleareax 
        mov al,  strlenn
        shr eax, 4
        cmp ecx, eax
        jae forendmd5_1

                        clearebx
                formd5_2:
                        cmp ebx, 64
                        jae forMD5Halfway
                        mov eax, ecx
                        shl eax, 4
                        add eax, ebx     ;i=eax / j=ebx / ecx reserved

                        lea esi, strbyte
                        lea edi, MM
                        mov edx, dword ptr [esi+eax]
                        mov byte ptr[edi+ebx], dl
                        inc ebx
                        jmp formd5_2
forMD5Halfway:  
        inc ecx
        push ecx
        cld
        call mainloop           ;..................calling mainloop
        pop ecx
        jmp formd5_1 
 
forendmd5_1: 
                                ;...............calling 911

        movzx esi, count
        mov eax, dtemp
        push eax
        call changehex 
        pop eax

        movzx esi, count
        mov eax, ctemp
        push eax
        call changehex 
        pop eax

        movzx esi, count
        mov eax, btemp
        push eax
        call changehex 
        pop eax

        movzx esi, count
        mov eax, atemp
        push eax
        call changehex 
        pop eax

        
        
       clearecx
       lea esi, pass3_hex_str             ;compare user password and password hash  
        movZX ebx, countpass
        cmp ebx, 2
        jne passnext
        lea edi, pass4_hexcheck_I          ;store reg passwordhash
        mov ecx, lengthof pass3_hex_str
        rep movsb
        dec ebx
        mov countpass, bl
        jmp endmd555

        passnext:
        clearecx
        lea edi, pass4_hexcheck_II         ;store login passwordhash  
        mov ecx, lengthof pass3_hex_str 
        rep movsb    


          
endmd555:    
pop ebp
ret 4
md555 endp
;-------------------------------------------------------------------------------------------------------------------------------------


;====================================================   mainloop module    ===================================================================
mainloop proc

Mov eax, A_
Mov atemp, eax
Mov eax, B_
Mov btemp, eax
Mov eax, C_
Mov ctemp, eax
Mov eax, D_     
Mov dtemp, eax

mov eax, atemp   ;atemp will not change till the endof loop
mov ebx, btemp
mov ecx, ctemp
mov edx, dtemp
 
cleareax ;ax=i=0
for4loop:
          cmp ax, 64 
          jae for4endloop
          cmp ax,16
          jae nextI
          
          next:
          FF ebx,ecx,edx,f1
          mov g1, al
          jmp for4loophalfway

                  nextI:
                  cmp ax,32
                  jae nextII

                  push eax
                  GG ebx,ecx,edx,f1
                  mul five
                  inc eax
                  div sixteen
                  mov al,ah
                  mov g1, al
                  pop eax
                  jmp for4loophalfway

                              nextII:
                              cmp ax,48
                              jae nextIII

                              push eax
                              HH ebx,ecx,edx,f1
                              mul three
                              add eax, 5
                              div sixteen
                              mov al, ah
                              mov g1, al
                              pop eax
                              jmp for4loophalfway

                                          nextIII: 
                                          push eax
                                          II ebx,ecx,edx,f1
                                          mul seven
                                          div sixteen
                                          mov al, ah
                                          mov g1, al
                                          pop eax

for4loophalfway:     
cmp eax, 0
ja  for4loophalfwayII
push eax         ;push i
;call writeint
;call crlf
;call crlf
mov eax, atemp
jmp for4loophalfwayIII
 
 for4loophalfwayII:    
                cmp eax, 0
                push eax       ;push i
                ;call writeint
                ;call crlf
                ;call crlf                
                mov eax, tempe

for4loophalfwayIII:
                                    
                add eax, f1             ;-------add F1
                pop edi                ;pop i
                push edi
                shl edi, 2
                add eax, table_t[di]   
                push eax

                push edx               ;--------print 
                lea edx, TTT
                ;call writestring
                pop edx

                mov eax, table_t[di]    ;-------- print T[]
                ;call writehex
                call crlf
                pop eax

                push eax
                movzx eax, g1
                mov esi,  eax
                pop eax
                shl esi, 2
                add eax, MM[si]         ;---------MM[]             
                push eax

                push edx                ;--------- print 
                lea  edx, MMM
                ;call writestring
                pop edx

                mov eax, MM[si]         ;---------- print
                ;call writeint
                call crlf
                pop eax
                
                mov shiftvarp, eax           ;---------------  stop --------------------eax reserved
                mov eax, SHIFT_AMTS[di]
                shiftx shiftvarp, eax
                 

                mov tempe, edx               ;tmp=d
                mov edx, ecx                 ;d = c 
                mov ecx, ebx                 ;c = b
                add ebx, esi                 ;or edi  ;b += a
                mov eax, tempe               ;a = tmp
                 
 
pop eax
inc eax
jmp for4loop

for4endloop:
           mov eax, tempe
           add atemp, eax
           add btemp, ebx
           add ctemp, ecx
           add dtemp, edx
 
ret  
mainloop endp

;----------------------------------------------------------------------------------------------------------------------------------------------


;========================================================= changehex module  =================================================================
changehex proc
push ebp
mov ebp, esp
pushad
mov eax, [ebp+8]                     ;a  = b  = dtemp,ctemp,btemp,atemp
mov x_ , eax

                                    ;---strr1.insert(0, 1, str16[b % 16]);
                                    ;---strr += str1;
                                    ;---b =  ( (a >> i * 8) %   (1 << 8) ) & 0xff  ---(256);

clearecx ;cx=0  i=0
mov edi, 0
for1outer:
        cmp ecx, 4
        je for1end

        ;(a >> i * 8)
        mov  eax, x_ 
        push ecx
        shl ecx, 3
        sar eax, cl 
                                 ;Modulus %   (1 << 8)
        and eax, 800000ffh
        dec eax
        or eax, 0ffffff00h       ;add sign to upper part
        inc eax
        and eax, 0ffh            ;clear the higher bits to zero  ;eax==b
  
                                clearebx   ;j=0
                                mov ebp, 1
                        for1inner:
                                cmp ebx, 2   ;j<2
                                jae for1outerHalfway                                                              
                                push ebx
                                push eax
                                                                ;Modulus % 16
                                and eax, 8000000fh
                                dec eax
                                or eax, 0ffffff00h
                                inc eax
                                and eax, 0ffh

                                movzx ebx, byte ptr str16[eax]    ; strr1.insert(0, 1, str16[b % 16]);  to insert character starting from back                        
                                mov byte ptr strr1[ebp], bl
                                dec ebp
                                pop eax                           ;b = b/16
                                shr eax, 4
                                
                                pop ebx
                                inc ebx
                                jmp for1inner
                                

        for1outerHalfway:            
                                                ;strr += strr1 append hash string into strr starting from back                         
            movzx eax, word ptr strr1
            mov word ptr strr[edi],  ax
            add edi, 2
            pop ecx
            inc ecx
            jmp for1outer
 
for1end:                                        ;this part of code is to store <strr> hashing into <pass3_hex_str> for comparison of both hashing password
cmp si, 0 
jbe endchangehex

mov ecx, 2
mov edi, 1
lop:
mov eax, dword ptr strr[edi*4]                 
mov dword ptr pass3_hex_str[esi*4], eax 
dec esi
dec edi
loop lop
mov eax, esi
mov count, al

;lea edx, pass3_hex_str
;call writestring
                                               ;clear strr
xor eax, eax
mov dword ptr strr, eax
mov dword ptr strr[4], eax
mov word ptr strr1, ax
 


endchangehex:
popad
pop ebp
ret 8
changehex endp

end main
