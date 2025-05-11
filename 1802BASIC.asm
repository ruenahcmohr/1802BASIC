
; 2048 bytes count
; 1806 disasembler (c)1998 Herman Robers PA3FYW
; Usage: disasm [-bxxxx] [-exxxx] [-rrom.bin] [-2]< hexfile.hex > disasm.asm, (example: disasm -b00e0 -e0a00 < dump.hex > dump.asm)
; Disassembling from 8400H to 01DDEADBEEFCAFEH.


; Some of the entry points are wrong, please replace blocks carefully.
; I have worked out a tiny bit of this, but the way this processor works 
; (tho it be damn cool) is hurting my brain.

; I know, its not a true assembler file...
;
;  keywords found:
;     INT
;     IF
;     THEN
;     INPUT
;     RETURN
;     END
;     LIST
;     RUN
;     CLEAR
;     REM
;
;
;     lets hope there are more, obviously, it has no floating point math,
;      some BASIC variants use ? to print
;
;     missing keywords:
;       FOR
;       ELSE
;       GOSUB
;       OUT
;       IN
;       DATA  oh please let there be DATA support!
;
;

8400 C4           NOP            ; No operation
8401 30 B0        BR  84B0H      ; GO SAVE REGISTERS

8403 C0 84 ED     LBR 84EDH      

8406 C0 01 00     LBR 0100H      ; JUMP TO KEY-INPUT ROUTINE

8409 C0 01 03     LBR 0103H      ; JUMP TO KEY-OUTPUT ROUTINE

840C C0 01 06     LBR 0106H      ; JUMP TO BREAK-DETECTION ROUTINE


840F 08           DB   08H	; "BACKSPACE" CHARACTER
8410 18           DB   18H	; "CANCEL" CHARACTER

8411 00           DB   00H	; PAD CODE:	   
                        ;	      ^-------------LSD=# OF PAD CHARS AFTER CR
                        ;	     ^--------------MSD=8: PAD CHAR.IS DELETE (FFH)
                        ;			       =0: PAD CHAR. IS NULL (00)
 
8412 00           DB   00H	; TAPE MODE
8413 19           DB   19H	; SPARE STACK

8414 30 22        BR  8422H      ; Short branch
8416 30 20        BR  8420H      ; Short branch

; POKE
8418 58           STR R8         ; Store D to (R8)
8419 D5           SEP R5         ; Set P=R5 as program counter


841A 8A           GLO RA         ; Get low register RA
841B 81           GLO R1         ; Get low register R1
841C 02           LDN R2         ; Load D with (R2)
841D 00           IDL            ; Idle or wait for interrupt or DMA request
841E 00           IDL            ; Idle or wait for interrupt or DMA request
841F 00           IDL            ; Idle or wait for interrupt or DMA request
8420 48           LDA R8         ; Load D from (R8), increment R8
8421 38 

; PEEK
8422 9D           GHI RD         ; Get high register RD
8423 BA           PHI RA         ; Put high register RA
8424 48           LDA R8         ; Load D from (R8), increment R8
8425 D5           SEP R5         ; Set P=R5 as program counter


8426 C0 8A 51     LBR 8A51H      ; Long branch

8429 D3           SEP R3         ; Set P=R3 as program counter
842A BF           PHI RF         ; Put high register RF
842B E2           SEX R2         ; Set X=R2 as datapointer
842C 86           GLO R6         ; Get low register R6
842D 73           STXD           ; Store via X and decrement
842E 96           GHI R6         ; Get high register R6
842F 73           STXD           ; Store via X and decrement
8430 83           GLO R3         ; Get low register R3
8431 A6           PLO R6         ; Put low register R6
8432 93           GHI R3         ; Get high register R3
8433 B6           PHI R6         ; Put high register R6
8434 46           LDA R6         ; Load D from (R6), increment R6
8435 B3           PHI R3         ; Put high register R3
8436 46           LDA R6         ; Load D from (R6), increment R6
8437 A3           PLO R3         ; Put low register R3
8438 9F           GHI RF         ; Get high register RF
8439 30 29        BR  8429H      ; Short branch


843B D3           SEP R3         ; Set P=R3 as program counter
843C BF           PHI RF         ; Put high register RF
843D E2           SEX R2         ; Set X=R2 as datapointer
843E 96           GHI R6         ; Get high register R6
843F B3           PHI R3         ; Put high register R3
8440 86           GLO R6         ; Get low register R6
8441 A3           PLO R3         ; Put low register R3
8442 12           INC R2         ; Increment (R2)
8443 42           LDA R2         ; Load D from (R2), increment R2
8444 B6           PHI R6         ; Put high register R6
8445 02           LDN R2         ; Load D with (R2)
8446 A6           PLO R6         ; Put low register R6
8447 9F           GHI RF         ; Get high register RF
8448 30 3B        BR  843BH      ; Short branch


844A D3           SEP R3         ; Set P=R3 as program counter
844B 43           LDA R3         ; Load D from (R3), increment R3
844C AD           PLO RD         ; Put low register RD
844D F8 00        LDI 00H        ; Load D immediate
844F BD           PHI RD         ; Put high register RD
8450 4D           LDA RD         ; Load D from (RD), increment RD
8451 ED           SEX RD         ; Set X=RD as datapointer
8452 30 4A        BR  844AH      ; Short branch



             ;---------------------------------------;
             ;       OPCODE TABLE                    ;
             ;---------------------------------------;
8454 85 98   TABLE:  DW  BACK                                 
8456 85 A0           DW  HOP                                  
8458 86 1F           DW  MATCH                                
845A 85 DD           DW  TSTV                                 
845C 85 F0           DW  TSTN                                 
845E 85 D4           DW  TEND                                 
8460 88 81           DW  RTN                                  
8462 86 49           DW  HOOK                                 
8464 84 ED           DW  WARM                                 
8466 88 4E           DW  XINIT                                
8468 85 04           DW  CLEAR                                
846A 89 A2           DW  INSRT                                
846C 85 D3           DW  RETN                                 
846E 85 D3           DW  RETN                                 
8470 88 AA           DW  GETLN                                
8472 85 D3           DW  RETN                                 
8474 85 D3           DW  RETN                                 
8476 86 C5           DW  STRNG                                
8478 86 D5           DW  CRLF                                 
847A 87 03           DW  TAB                                  
847C 86 79           DW  PRS                                  
847E 87 18           DW  PRN                                  
8480 89 3C           DW  LIST
8482 85 D3           DW  RETN                                 
8484 88 29           DW  NXT                                  
8486 87 6C           DW  CMPR                                 
8488 87 CB           DW  IDIV
848B 87 A7           DW  IMUL                           
848C 87 98           DW  ISUB                                 
848E 87 9B           DW  IADD                                 
8490 88 0E           DW  INEG                                 
8492 88 60           DW  XFER                                 
8494 88 6D           DW  RSTR                                 
8496 89 81           DW  SAV                                  
8498 85 B6           DW  STORE                                
849A 86 67           DW  IND                                  
849C 87 48           DW  RSBP                                 
849E 87 4B           DW  SVBP                                 
84A0 85 D3           DW  RETN                                 
84A2 85 D3           DW  RETN                                 
84A4 85 C9           DW  BPOP
84A6 85 C5           DW  APOP                           
84A8 86 4E           DW  DUPS                                 
84AA 86 44           DW  LITN                                 
84AC 86 41           DW  LIT1                                 
84AE 85 D3           DW  RETN                                 




; confirmed entry


                  ;-----------------------------------------------;
                  ;       COLD & WARM START INITIALIZATION        ;
                  ;-----------------------------------------------;
                  ;
                  ; COLD START; ENTRY FOR "NEW?" = YES
; COLD:           ;
84B0 F8 B3        COLD:   LDI  LOW $+3    ; CHANGE PROGRAM COUNTER
84B2 A3                   PLO  R3         ;   FROM R0 TO R3
84B3 F8 84                LDI  HIGH $
84B5 B3                   PHI  R3
84B6 D3                   SEP  R3
                  ; DETERMINE SIZE OF USER RAM
84B7 BA                   PHI  AC         ; GET LOW END ADDR.
84B8 F8 1C                LDI  LOW CONST  ;   OF USER PROGRAM
84BA AA                   PLO  AC         ;   RAM (AT "CONST")
84BB 4A                   LDA  AC
84BC B2                   PHI  R2         ; ..AND PUT IN R2
84BD 4A                   LDA  AC
84BE A2                   PLO  R2
84BF 4A                   LDA  AC         ; SET PZ TO WRAP POINT
84C0 BD                   PHI  PZ         ;   (END OF SEARCH)
84C1 F8 00                LDI  0
84C3 AD                   PLO  PZ
84C4 0D                   LDN  PZ         ; ..AND SAVE BYTE
84C5 BF                   PHI  X          ;   NOW AT ADDR. PZ
84C6 E2           SCAN:   SEX  R2         ; REPEAT TO SEARCH RAM..
84C7 12                   INC  R2         ; - GET NEXT BYTE
84C8 F0                   LDX
84C9 AF                   PLO  X          ; - SAVE A COPY
84CA FB FF                XRI  0FFH       ; - COMPLEMENT IT
84CC 52                   STR  R2         ; - STORE IT
84CD F3                   XOR             ; - SEE IF IT WORKED
84CE ED                   SEX  PZ
84CF C6                   LSNZ            ; - IF MATCHES, IS RAM
84D0 9F                   GHI  X          ;     SET CARRY IF AT
84D1 F3                   XOR             ;     WRAP POINT..
84D2 FC FF                ADI  0FFH       ; - ELSE IS NOT RAM
84D4 8F                   GLO  X          ;     RESTORE ORIGINAL BYTE
84D5 52                   STR  R2
84D6 3B C6                BNF  SCAN       ; - ..UNTIL END OR WRAP POINT
84D8 22                   DEC  R2
84D9 0A                   LDN  AC         ; RAM SIZED: SET
84DA BD                   PHI  PZ         ;   POINTER PZ TO
84DB F8 23                LDI  STACK+1    ;   WORK AREA
84DD AD                   PLO  PZ
84DE 82                   GLO  R2         ; STORE RAM END ADDRESS
84DF 73                   STXD
84E0 92                   GHI  R2
84E1 73                   STXD            ; GET & STORE RAM BEGINNIG
84E2 2A                   DEC  AC         ; REPEAT TO COPY PARAMETERS..
84E3 2A                   DEC  AC         ; - POINT TO NEXT
84E4 0A                   LDN  AC         ; - GET PARAMETER
84E5 73                   STXD            ; - STORE IN WORK AREA
84E6 8D                   GLO  PZ
84E7 FB 12                XRI  BS-1       ; - TEST FOR LAST PARAMETER
84E9 3A E3                BNZ  $-6        ; - ..UNTIL LAST COPIED
84EB F6                   SHR             ; SET DF=0 FOR "CLEAR"
84EC C8                   LSKP

                  ;
                  ; WARM START: ENTRY FOR "NEW?" = NO
                  ;
84ED FF 00        WARM:   SMI  0          ; SET DF=1 FOR "DON'T CLEAR"
84EF F8 F2                LDI  $+3
84F1 A3                   PLO  R3         ; BE SURE PROGRAM COUNTER IS R3
84F2 F8 84                LDI  HIGH $
84F4 B3                   PHI  R3
84F5 D3                   SEP  R3
84F6 B4                   PHI  R4         ; INITIALIZE R4, R5, R7
84F7 B5                   PHI  R5
84F8 B7                   PHI  R7
84F9 F8 2A                LDI  CALL
84FB A4                   PLO  R4
84FC F8 3C                LDI  RETRN
84FE A5                   PLO  R5
84FF F8 4B                LDI  FETCH
8501 A7                   PLO  R7
8502 33 1A                BDF  PEND       ; IF COLD START,
8504 D7 20        CLEAR:  DB   FECH,BASIC ; - MARK PROGRAM EMPTY
8506 BB                   PHI  BP
8507 4D                   LDA  PZ
8508 AB                   PLO  BP
8509 9D                   DB   LDI0       ;   WITH LINE# = 0
850A 5B                   STR  BP
850B 1B                   INC  BP
850C 5B                   STR  BP
850D D7 16                DB   FECH,SPARE-1; - SET MEND = START + SPARE
850F 8B                   GLO  BP         ;   GET START
8510 F4                   ADD             ;   ADD LOW BYTE OF SPARE
8511 BF                   PHI  X          ;   SAVE TEMPORARILY
8512 D7 24                DB   FECH,MEND  ;   GET MEND
8514 9F                   GHI  X
8515 73                   STXD            ;   STORE LOW BYTE OF MEND
8516 9B                   GHI  BP
8517 7C 00                ADCI 0          ;   ADD CARRY
8519 73                   STXD            ;   STORE HIGH BYTE OF MEND
851A D7 22        PEND:   DB   FECH,STACK ; SET STACK TO END OF MEMORY
851C B2                   PHI  R2
851D 4D                   LDA  PZ
851E A2                   PLO  R2
851F D7 26                DB   FECH,TOPS
8521 82                   GLO  R2         ; SET TOPS TO EMPTY
8522 73                   STXD            ; (I.E. STACK END)
8523 92                   GHI  R2
8524 73                   STXD
8525 D4                   SEP R4 ;CALL 
8526 86                   GLO R6         
8527 CC                   LSIE           ; Long skip on IE=1
8528 D7 1E        IIL:    DB   FECH,AIL   ; SET IL PC
852A B9                   PHI  PC
852B 4D                   LDA  PZ
852C A9                   PLO  PC         ; CONTINUE INTO "NEXT"



852D E2           SEX R2         ; Set X=R2 as datapointer
852E 49           LDA R9         ; Load D from (R9), increment R9
852F FF 30        SMI 30H        ; Substract D,DF to value
8531 33 4B        BDF 854BH      ; Short branch on DF=1
8533 FD D7        SDI D7H        ; Substract D,DF from value
8535 33 85        BDF 8585H      ; Short branch on DF=1
8537 FE           SHL            ; Shift left D
8538 FC B0        ADI B0H        ; Add D,DF with value
853A A6           PLO R6         ; Put low register R6
853B F8 2D        LDI 2DH        ; Load D immediate
853D 22           DEC R2         ; Decrement (R2)
853E 22           DEC R2         ; Decrement (R2)
853F 73           STXD           ; Store via X and decrement
8540 93           GHI R3         ; Get high register R3
8541 00           IDL            ; Idle or wait for interrupt or DMA request
8542 97           GHI R7         ; Get high register R7
8543 B6           PHI R6         ; Put high register R6
8544 46           LDA R6         ; Load D from (R6), increment R6
8545 52           STR R2         ; Store D to (R2)
8546 46           LDA R6         ; Load D from (R6), increment R6
8547 A6           PLO R6         ; Put low register R6
8548 F0           LDX            ; Pop stack. Place value in D register
8549 B6           PHI R6         ; Put high register R6
854A D5           SEP R5         ; Set P=R5 as program counter

854B FF 10        SMI 10H        ; Substract D,DF to value
854D 3B 6A        BNF 856AH      ; Short branch on DF=0
854F A6           PLO R6         ; Put low register R6
8550 FA 1F        ANI 1FH        ; Logical AND D with value
8552 32 5C        BZ  855CH      ; Short branch on D=0
8554 52           STR R2         ; Store D to (R2)
8555 89           GLO R9         ; Get low register R9
8556 F4           ADD            ; Add D: D,DF= D+(R(X))
8557 73           STXD           ; Store via X and decrement
8558 99           GHI R9         ; Get high register R9
8559 7C 00        ADCI 00H       ; Add with carry immediate
855B 38 73        SKP            ; Skip next byte
855D 73           STXD           ; Store via X and decrement
855E 86           GLO R6         ; Get low register R6
855F F6           SHR            ; Shift right D

8560 F6           SHR            ; Shift right D
8561 F6           SHR            ; Shift right D
8562 F6           SHR            ; Shift right D
8563 FA FE        ANI FEH        ; Logical AND D with value
8565 FC 54        ADI 54H        ; Add D,DF with value
8567 A6           PLO R6         ; Put low register R6
8568 30 42        BR  8542H      ; Short branch

856A FC 08        ADI 08H        ; Add D,DF with value
856C FA 07        ANI 07H        ; Logical AND D with value
856E B6           PHI R6         ; Put high register R6
856F 49           LDA R9         ; Load D from (R9), increment R9
8570 A6           PLO R6         ; Put low register R6
8571 33 7A        BDF 857AH      ; Short branch on DF=1
8573 89           GLO R9         ; Get low register R9
8574 73           STXD           ; Store via X and decrement
8575 99           GHI R9         ; Get high register R9
8576 73           STXD           ; Store via X and decrement
8577 D4           SEP R4         ; Set P=R4 as program counter
8578 86           GLO R6         ; Get low register R6
8579 37 

857A D7 1E       
857C 86           GLO R6         ; Get low register R6
857D F4           ADD            ; Add D: D,DF= D+(R(X))
857E A9           PLO R9         ; Put low register R9
857F 96           GHI R6         ; Get high register R6
8580 2D           DEC RD         ; Decrement (RD)
8581 74           ADC            ; Add with carry
8582 B9           PHI R9         ; Put high register R9
8583 30 2D        BR  852DH      ; Short branch

8585 FD 07        SDI 07H        ; Substract D,DF from value
8587 52           STR R2         ; Store D to (R2)
8588 D7           SEP R7         ; Set P=R7 as program counter
8589 1A           INC RA         ; Increment (RA)
858A AD           PLO RD         ; Put low register RD
858B E2           SEX R2         ; Set X=R2 as datapointer
858C F4           ADD            ; Add D: D,DF= D+(R(X))
858D A6           PLO R6         ; Put low register R6
858E 9D           GHI RD         ; Get high register RD
858F B6           PHI R6         ; Put high register R6
8590 0D           LDN RD         ; Load D with (RD)
8591 52           STR R2         ; Store D to (R2)
8592 06           LDN R6         ; Load D with (R6)
8593 5D           STR RD         ; Store D to (RD)
8594 02           LDN R2         ; Load D with (R2)
8595 56           STR R6         ; Store D to (R6)
8596 30 2D        BR  852DH      ; Short branch

8598 86           GLO R6         ; Get low register R6
8599 FF 20        SMI 20H        ; Substract D,DF to value
859B A6           PLO R6         ; Put low register R6
859C 96           GHI R6         ; Get high register R6

859D 7F 00        SMBI           ; Substract memory toh borrow, immediate
859F 38 

85A0 96   
85A1 C2 86 7F     LBZ 867FH      ; Long branch on D=0
85A4 B9           PHI R9         ; Put high register R9
85A5 86           GLO R6         ; Get low register R6
85A6 A9           PLO R9         ; Put low register R9
85A7 30 2D        BR  852DH      ; Short branch

85A9 1B           INC RB         ; Increment (RB)
85AA 0B           LDN RB         ; Load D with (RB)
85AB FF 20        SMI 20H        ; Substract D,DF to value
85AD 32 A9        BZ  85A9H      ; Short branch on D=0
85AF FF 10        SMI 10H        ; Substract D,DF to value
85B1 C7           LSNF           ; Long skip on DF=0
85B2 FD 09        SDI 09H        ; Substract D,DF from value
85B4 0B           LDN RB         ; Load D with (RB)
85B5 D5           SEP R5         ; Set P=R5 as program counter

85B6 D4           SEP R4         ; Set P=R4 as program counter
85B7 85           GLO R5         ; Get low register R5
85B8 C5           LSNQ           ; Long skip on Q=0
85B9 4D           LDA RD         ; Load D from (RD), increment RD
85BA AD           PLO RD         ; Put low register RD
85BB 9A           GHI RA         ; Get high register RA
85BC 5D           STR RD         ; Store D to (RD)
85BD 1D           INC RD         ; Increment (RD)
85BE 8A           GLO RA         ; Get low register RA
85BF 5D           STR RD         ; Store D to (RD)
85C0 30 C9        BR  85C9H      ; Short branch

85C2 D4           SEP R4         ; Set P=R4 as program counter
85C3 85           GLO R5         ; Get low register R5
85C4 C5           LSNQ           ; Long skip on Q=0
85C5 D4           SEP R4         ; Set P=R4 as program counter
85C6 85           GLO R5         ; Get low register R5
85C7 C9 BA D7     LBNQ BAD7H     ; Long branch on Q=0
85CA 1A           INC RA         ; Increment (RA)
85CB 2D           DEC RD         ; Decrement (RD)
85CC FC 01        ADI 01H        ; Add D,DF with value
85CE 5D           STR RD         ; Store D to (RD)
85CF AD           PLO RD         ; Put low register RD
85D0 2D           DEC RD         ; Decrement (RD)
85D1 4D           LDA RD         ; Load D from (RD), increment RD
85D2 AA           PLO RA         ; Put low register RA
85D3 D5           SEP R5         ; Set P=R5 as program counter

85D4 D4           SEP R4         ; Set P=R4 as program counter
85D5 85           GLO R5         ; Get low register R5
85D6 AA           PLO RA         ; Put low register RA
85D7 FB 0D        XRI 0DH        ; Logical XOR D with value
85D9 32 2D        BZ  852DH      ; Short branch on D=0
85DB 30 A0        BR  85A0H      ; Short branch

85DD D4           SEP R4         ; Set P=R4 as program counter
85DE 85           GLO R5         ; Get low register R5
85DF AA           PLO RA         ; Put low register RA
85E0 FF 41        SMI 41H        ; Substract D,DF to value
85E2 3B A0        BNF 85A0H      ; Short branch on DF=0

85E4 FF 1A        SMI 1AH        ; Substract D,DF to value
85E6 33 A0        BDF 85A0H      ; Short branch on DF=1
85E8 1B           INC RB         ; Increment (RB)
85E9 9F           GHI RF         ; Get high register RF
85EA FE           SHL            ; Shift left D
85EB D4           SEP R4         ; Set P=R4 as program counter
85EC 86           GLO R6         ; Get low register R6
85ED 59           STR R9         ; Store D to (R9)
85EE 30 2D        BR  852DH      ; Short branch

85F0 D4           SEP R4         ; Set P=R4 as program counter
85F1 85           GLO R5         ; Get low register R5
85F2 AA           PLO RA         ; Put low register RA
85F3 3B A0        BNF 85A0H      ; Short branch on DF=0
85F5 9D           GHI RD         ; Get high register RD
85F6 BA           PHI RA         ; Put high register RA
85F7 AA           PLO RA         ; Put low register RA
85F8 D4           SEP R4         ; Set P=R4 as program counter
85F9 86           GLO R6         ; Get low register R6
85FA 54           STR R4         ; Store D to (R4)
85FB 4B           LDA RB         ; Load D from (RB), increment RB
85FC FA 0F        ANI 0FH        ; Logical AND D with value
85FE AA           PLO RA         ; Put low register RA
85FF 9D           GHI RD         ; Get high register RD
8600 BA           PHI RA         ; Put high register RA
8601 F8 0A        LDI 0AH        ; Load D immediate
8603 AF           PLO RF         ; Put low register RF
8604 ED           SEX RD         ; Set X=RD as datapointer
8605 00           IDL            ; Idle or wait for interrupt or DMA request
8606 8A           GLO RA         ; Get low register RA
8607 F4           ADD            ; Add D: D,DF= D+(R(X))
8608 AA           PLO RA         ; Put low register RA
8609 9A           GHI RA         ; Get high register RA
860A 2D           DEC RD         ; Decrement (RD)
860B 74           ADC            ; Add with carry
860C BA           PHI RA         ; Put high register RA
860D 2F           DEC RF         ; Decrement (RF)
860E 8F           GLO RF         ; Get low register RF
860F 3A 05        BNZ 8605H      ; Short branch on D!=0
8611 9A           GHI RA         ; Get high register RA
8612 5D           STR RD         ; Store D to (RD)
8613 1D           INC RD         ; Increment (RD)
8614 8A           GLO RA         ; Get low register RA
8615 73           STXD           ; Store via X and decrement
8616 D4           SEP R4         ; Set P=R4 as program counter
8617 85           GLO R5         ; Get low register R5
8618 AA           PLO RA         ; Put low register RA
8619 C3 85 FB     LBDF 85FBH     ; Long branch on DF=1
861C C0 85 2D     LBR 852DH      ; Long branch

861F 9B           GHI RB         ; Get high register RB
8620 BA           PHI RA         ; Put high register RA
8621 8B           GLO RB         ; Get low register RB
8622 AA           PLO RA         ; Put low register RA
8623 D4           SEP R4         ; Set P=R4 as program counter
8624 85           GLO R5         ; Get low register R5
8625 AA           PLO RA         ; Put low register RA

8626 1B           INC RB         ; Increment (RB)
8627 52           STR R2         ; Store D to (R2)
8628 49           LDA R9         ; Load D from (R9), increment R9

8629 F3           XOR            ; Logical exclusive OR  D with (R(X))
862A 32 23        BZ  8623H      ; Short branch on D=0
862C FB 80        XRI 80H        ; Logical XOR D with value
862E 32 1C        BZ  861CH      ; Short branch on D=0
8630 9A           GHI RA         ; Get high register RA
8631 BB           PHI RB         ; Put high register RB
8632 8A           GLO RA         ; Get low register RA
8633 AB           PLO RB         ; Put low register RB
8634 C0 85 A0     LBR 85A0H      ; Long branch

8637 D7           SEP R7         ; Set P=R7 as program counter
8638 24           DEC R4         ; Decrement (R4)
8639 82           GLO R2         ; Get low register R2
863A F5           SD             ; Substract D: D,DF=(R(X))-D
863B 2D           DEC RD         ; Decrement (RD)
863C 92           GHI R2         ; Get high register R2
863D 75           SDB            ; Substract D with borrow
863E 33 7F        BDF 867FH      ; Short branch on DF=1
8640 D5           SEP R5         ; Set P=R5 as program counter

8641 49           LDA R9         ; Load D from (R9), increment R9
8642 30 59        BR  8659H      ; Short branch
8644 49           LDA R9         ; Load D from (R9), increment R9
8645 BA           PHI RA         ; Put high register RA
8646 49           LDA R9         ; Load D from (R9), increment R9
8647 30 55        BR  8655H      ; Short branch

8649 D4           SEP R4         ; Set P=R4 as program counter
864A 89           GLO R9         ; Get low register R9
864B 25           DEC R5         ; Decrement (R5)
864C 30 55        BR  8655H      ; Short branch

864E D4           SEP R4         ; Set P=R4 as program counter
864F 85           GLO R5         ; Get low register R5
8650 C5           LSNQ           ; Long skip on Q=0
8651 D4           SEP R4         ; Set P=R4 as program counter
8652 86           GLO R6         ; Get low register R6
8653 54           STR R4         ; Store D to (R4)
8654 8A           GLO RA         ; Get low register RA
8655 D4           SEP R4         ; Set P=R4 as program counter
8656 86           GLO R6         ; Get low register R6
8657 59           STR R9         ; Store D to (R9)
8658 9A           GHI RA         ; Get high register RA
8659 52           STR R2         ; Store D to (R2)
865A D7           SEP R7         ; Set P=R7 as program counter
865B 19           INC R9         ; Increment (R9)
865C F7           SM             ; Substract memory: DF,D=D-(R(X))
865D 33 7F        BDF 867FH      ; Short branch on DF=1
865F F8 01        LDI 01H        ; Load D immediate
8661 F5           SD             ; Substract D: D,DF=(R(X))-D
8662 5D           STR RD         ; Store D to (RD)
8663 AD           PLO RD         ; Put low register RD
8664 02           LDN R2         ; Load D with (R2)
8665 5D           STR RD         ; Store D to (RD)
8666 D5           SEP R5         ; Set P=R5 as program counter

8667 D4           SEP R4         ; Set P=R4 as program counter
8668 85           GLO R5         ; Get low register R5
8669 C9 AD 4D     LBNQ AD4DH     ; Long branch on Q=0
866C BA           PHI RA         ; Put high register RA
866D 4D           LDA RD         ; Load D from (RD), increment RD

866E 30 55        BR  8655H      ; Short branch

8670 FB 2F        XRI 2FH        ; Logical XOR D with value
8672 32 66        BZ  8666H      ; Short branch on D=0
8674 FB 22        XRI 22H        ; Logical XOR D with value
8676 D4           SEP R4         ; Set P=R4 as program counter
8677 86           GLO R6         ; Get low register R6
8678 F4           ADD            ; Add D: D,DF= D+(R(X))
8679 4B           LDA RB         ; Load D from (RB), increment RB
867A FB 0D        XRI 0DH        ; Logical XOR D with value
867C 3A 70        BNZ 8670H      ; Short branch on D!=0
867E 29           DEC R9         ; Decrement (R9)

867F D7           SEP R7         ; Set P=R7 as program counter
8680 18           INC R8         ; Increment (R8)
8681 B8           PHI R8         ; Put high register R8
8682 D4           SEP R4         ; Set P=R4 as program counter
8683 86           GLO R6         ; Get low register R6
8684 CC           LSIE           ; Long skip on IE=1
8685 F8 21        LDI 21H        ; Load D immediate
8687 D4           SEP R4         ; Set P=R4 as program counter
8688 86           GLO R6         ; Get low register R6
8689 F4           ADD            ; Add D: D,DF= D+(R(X))
868A D7           SEP R7         ; Set P=R7 as program counter
868B 1E           INC RE         ; Increment (RE)
868C 89           GLO R9         ; Get low register R9
868D F7           SM             ; Substract memory: DF,D=D-(R(X))
868E AA           PLO RA         ; Put low register RA
868F 99           GHI R9         ; Get high register R9
8690 2D           DEC RD         ; Decrement (RD)
8691 77           SMB            ; Substract memory with borrow
8692 BA           PHI RA         ; Put high register RA
8693 D4           SEP R4         ; Set P=R4 as program counter
8694 87           GLO R7         ; Get low register R7
8695 15           INC R5         ; Increment (R5)
8696 98           GHI R8         ; Get high register R8
8697 32 A9        BZ  86A9H      ; Short branch on D=0
8699 F8 BD        LDI BDH        ; Load D immediate
869B A9           PLO R9         ; Put low register R9
869C 93           GHI R3         ; Get high register R3
869D B9           PHI R9         ; Put high register R9
869E D4           SEP R4         ; Set P=R4 as program counter
869F 86           GLO R6         ; Get low register R6
86A0 C5           LSNQ           ; Long skip on Q=0
86A1 D7           SEP R7         ; Set P=R7 as program counter
86A2 28           DEC R8         ; Decrement (R8)
86A3 BA           PHI RA         ; Put high register RA
86A4 4D           LDA RD         ; Load D from (RD), increment RD
86A5 AA           PLO RA         ; Put low register RA
86A6 D4           SEP R4         ; Set P=R4 as program counter
86A7 87           GLO R7         ; Get low register R7
86A8 15           INC R5         ; Increment (R5)
86A9 F8 07        LDI 07H        ; Load D immediate
86AB D4           SEP R4         ; Set P=R4 as program counter
86AC 84           GLO R4         ; Get low register R4
86AD 09           LDN R9         ; Load D with (R9)
86AE D4           SEP R4         ; Set P=R4 as program counter
86AF 86           GLO R6         ; Get low register R6
86B0 D5           SEP R5         ; Set P=R5 as program counter
86B1 D7           SEP R7         ; Set P=R7 as program counter
86B2 1A           INC RA         ; Increment (RA)
86B3 9D           GHI RD         ; Get high register RD
86B4 5D           STR RD         ; Store D to (RD)
86B5 D7           SEP R7         ; Set P=R7 as program counter
86B6 26           DEC R6         ; Decrement (R6)
86B7 B2           PHI R2         ; Put high register R2
86B8 4D           LDA RD         ; Load D from (RD), increment RD
86B9 A2           PLO R2         ; Put low register R2
86BA C0 85 28     LBR 8528H      ; Long branch

86BD 20 41 54 20          ATMSG:  DB   ' AT ',0A3H; ERROR MESSAGE TEMPLATE
86C1 A3           

86C2 D4           SEP R4         ; Set P=R4 as program counter
86C3 86           GLO R6         ; Get low register R6
86C4 F2           AND            ; Logical AND: D with (R(X))
86C5 49           LDA R9         ; Load D from (R9), increment R9
86C6 FC 80        ADI 80H        ; Add D,DF with value
86C8 3B C2        BNF 86C2H      ; Short branch on DF=0
86CA 30 F2        BR  86F2H      ; Short branch

86CC D7           SEP R7         ; Set P=R7 as program counter
86CD 19           INC R9         ; Increment (R9)
86CE F8 80        LDI 80H        ; Load D immediate
86D0 73           STXD           ; Store via X and decrement
86D1 9D           GHI RD         ; Get high register RD
86D2 73           STXD           ; Store via X and decrement
86D3 73           STXD           ; Store via X and decrement
86D4 C8 

86D5 D7 1B     LSKP           ; Long skip
86D7 FE           SHL            ; Shift left D
86D8 33 66        BDF 8666H      ; Short branch on DF=1
86DA D7           SEP R7         ; Set P=R7 as program counter
86DB 15           INC R5         ; Increment (R5)
86DC AA           PLO RA         ; Put low register RA
86DD F8 0D        LDI 0DH        ; Load D immediate
86DF D4           SEP R4         ; Set P=R4 as program counter
86E0 84           GLO R4         ; Get low register R4
86E1 09           LDN R9         ; Load D with (R9)
86E2 D7           SEP R7         ; Set P=R7 as program counter
86E3 1A           INC RA         ; Increment (RA)
86E4 8A           GLO RA         ; Get low register RA
86E5 FE           SHL            ; Shift left D
86E6 32 EF        BZ  86EFH      ; Short branch on D=0
86E8 2A           DEC RA         ; Decrement (RA)
86E9 9D           GHI RD         ; Get high register RD
86EA C7           LSNF           ; Long skip on DF=0
86EB F8 FF        LDI FFH        ; Load D immediate
86ED 30 DF        BR  86DFH      ; Short branch

86EF 73           STXD           ; Store via X and decrement
86F0 F8 8A        LDI 8AH        ; Load D immediate

86F2 FF 80        SMI 80H        ; Substract D,DF to value
86F4 BF           PHI RF         ; Put high register RF
86F5 D7           SEP R7         ; Set P=R7 as program counter
86F6 1B           INC RB         ; Increment (RB)
86F7 2D           DEC RD         ; Decrement (RD)
86F8 FC 81        ADI 81H        ; Add D,DF with value
86FA FC 80        ADI 80H        ; Add D,DF with value
86FC 3B 66        BNF 8666H      ; Short branch on DF=0
86FE 5D           STR RD         ; Store D to (RD)
86FF 9F           GHI RF         ; Get high register RF
8700 C0 84 09     LBR 8409H      ; Long branch                    sub-braches to 0103

8703 D7           SEP R7         ; Set P=R7 as program counter
8704 1B           INC RB         ; Increment (RB)
8705 FA 07        ANI 07H        ; Logical AND D with value
8707 FD 08        SDI 08H        ; Substract D,DF from value
8709 AA           PLO RA         ; Put low register RA
870A 8A           GLO RA         ; Get low register RA
870B 32 97        BZ  8797H      ; Short branch on D=0

870D F8 20        LDI 20H        ; Load D immediate
870F D4           SEP R4         ; Set P=R4 as program counter
8710 86           GLO R6         ; Get low register R6
8711 F4           ADD            ; Add D: D,DF= D+(R(X))
8712 2A           DEC RA         ; Decrement (RA)
8713 30 0A        BR  870AH      ; Short branch

8715 D4           SEP R4         ; Set P=R4 as program counter
8716 86           GLO R6         ; Get low register R6
8717 54           STR R4         ; Store D to (R4)
8718 D7           SEP R7         ; Set P=R7 as program counter
8719 1A           INC RA         ; Increment (RA)
871A AD           PLO RD         ; Put low register RD
871B D4           SEP R4         ; Set P=R4 as program counter
871C 88           GLO R8         ; Get low register R8
871D 13           INC R3         ; Increment (R3)
871E 3B 25        BNF 8725H      ; Short branch on DF=0
8720 F8 2D        LDI 2DH        ; Load D immediate
8722 D4           SEP R4         ; Set P=R4 as program counter
8723 86           GLO R6         ; Get low register R6
8724 F4           ADD            ; Add D: D,DF= D+(R(X))
8725 9D           GHI RD         ; Get high register RD
8726 73           STXD           ; Store via X and decrement
8727 BA           PHI RA         ; Put high register RA
8728 F8 0A        LDI 0AH        ; Load D immediate
872A D4           SEP R4         ; Set P=R4 as program counter
872B 86           GLO R6         ; Get low register R6
872C 55           STR R5         ; Store D to (R5)
872D 1D           INC RD         ; Increment (RD)
872E D4           SEP R4         ; Set P=R4 as program counter
872F 87           GLO R7         ; Get low register R7
8730 E3           SEX R3         ; Set X=R3 as datapointer
8731 8A           GLO RA         ; Get low register RA
8732 F6           SHR            ; Shift right D
8733 F9 30        ORI 30H        ; Logical OR D with value
8735 73           STXD           ; Store via X and decrement
8736 1D           INC RD         ; Increment (RD)
8737 4D           LDA RD         ; Load D from (RD), increment RD
8738 ED           SEX RD         ; Set X=RD as datapointer
8739 0F           LDN RF         ; Load D with (RF)
873A 2D           DEC RD         ; Decrement (RD)
873B 2D           DEC RD         ; Decrement (RD)
873C 3A 2E        BNZ 872EH      ; Short branch on D!=0
873E 12           INC R2         ; Increment (R2)
873F 02           LDN R2         ; Load D with (R2)
8740 C2 85 C2     LBZ 85C2H      ; Long branch on D=0
8743 D4           SEP R4         ; Set P=R4 as program counter
8744 86           GLO R6         ; Get low register R6
8745 F4           ADD            ; Add D: D,DF= D+(R(X))
8746 30 3E        BR  873EH      ; Short branch

8748 D7           SEP R7         ; Set P=R7 as program counter
8749 2E           DEC RE         ; Decrement (RE)
874A 38 9B        SKP            ; Skip next byte
874C FB 00        XRI 00H        ; Logical XOR D with value
874E 3A 5E        BNZ 875EH      ; Short branch on D!=0
8750 8B           GLO RB         ; Get low register RB
8751 52           STR R2         ; Store D to (R2)
8752 F0           LDX            ; Pop stack. Place value in D register
8753 FF 80        SMI 80H        ; Substract D,DF to value
8755 33 5E        BDF 875EH      ; Short branch on DF=1
8757 D7           SEP R7         ; Set P=R7 as program counter
8758 2E           DEC RE         ; Decrement (RE)
8759 8B           GLO RB         ; Get low register RB
875A 73           STXD           ; Store via X and decrement
875B 9B           GHI RB         ; Get high register RB
875C 5D           STR RD         ; Store D to (RD)

875D D5           SEP R5         ; Set P=R5 as program counter

875E D7           SEP R7         ; Set P=R7 as program counter
875F 2E           DEC RE         ; Decrement (RE)
8760 B8           PHI R8         ; Put high register R8
8761 0D           LDN RD         ; Load D with (RD)
8762 A8           PLO R8         ; Put low register R8
8763 8B           GLO RB         ; Get low register RB
8764 73           STXD           ; Store via X and decrement
8765 9B           GHI RB         ; Get high register RB
8766 5D           STR RD         ; Store D to (RD)
8767 98           GHI R8         ; Get high register R8
8768 BB           PHI RB         ; Put high register RB
8769 88           GLO R8         ; Get low register R8
876A AB           PLO RB         ; Put low register RB
876B D5           SEP R5         ; Set P=R5 as program counter

876C D4           SEP R4         ; Set P=R4 as program counter
876D 85           GLO R5         ; Get low register R5
876E C5           LSNQ           ; Long skip on Q=0
876F 9A           GHI RA         ; Get high register RA
8770 FB 80        XRI 80H        ; Logical XOR D with value
8772 73           STXD           ; Store via X and decrement
8773 8A           GLO RA         ; Get low register RA
8774 73           STXD           ; Store via X and decrement
8775 D4           SEP R4         ; Set P=R4 as program counter
8776 85           GLO R5         ; Get low register R5
8777 C9 AF D4     LBNQ AFD4H     ; Long branch on Q=0
877A 85           GLO R5         ; Get low register R5
877B C5           LSNQ           ; Long skip on Q=0
877C 12           INC R2         ; Increment (R2)
877D 8A           GLO RA         ; Get low register RA
877E F7           SM             ; Substract memory: DF,D=D-(R(X))
877F AA           PLO RA         ; Put low register RA
8780 12           INC R2         ; Increment (R2)
8781 9A           GHI RA         ; Get high register RA
8782 FB 80        XRI 80H        ; Logical XOR D with value
8784 77           SMB            ; Substract memory with borrow
8785 52           STR R2         ; Store D to (R2)
8786 3B 92        BNF 8792H      ; Short branch on DF=0
8788 8A           GLO RA         ; Get low register RA
8789 F1           OR             ; Logical OR  D with (R(X))
878A 32 8F        BZ  878FH      ; Short branch on D=0
878C 8F           GLO RF         ; Get low register RF
878D F6           SHR            ; Shift right D
878E 38 8F        SKP            ; Skip next byte
8790 F6           SHR            ; Shift right D
8791 38 8F        SKP            ; Skip next byte
8793 F6           SHR            ; Shift right D
8794 C7           LSNF           ; Long skip on DF=0
8795 C4           NOP            ; No operation
8796 19           INC R9         ; Increment (R9)
8797 D5           SEP R5         ; Set P=R5 as program counter

8798 D4           SEP R4         ; Set P=R4 as program counter
8799 88           GLO R8         ; Get low register R8
879A 0E           LDN RE         ; Load D with (RE)
879B D4           SEP R4         ; Set P=R4 as program counter
879C 85           GLO R5         ; Get low register R5
879D C5           LSNQ           ; Long skip on Q=0
879E ED           SEX RD         ; Set X=RD as datapointer
879F 00           IDL            ; Idle or wait for interrupt or DMA request
87A0 8A           GLO RA         ; Get low register RA
87A1 F4           ADD            ; Add D: D,DF= D+(R(X))
87A2 73           STXD           ; Store via X and decrement
87A3 9A           GHI RA         ; Get high register RA
87A4 74           ADC            ; Add with carry
87A5 5D           STR RD         ; Store D to (RD)
87A6 D5           SEP R5         ; Set P=R5 as program counter

87A7 D4           SEP R4         ; Set P=R4 as program counter
87A8 85           GLO R5         ; Get low register R5
87A9 C5           LSNQ           ; Long skip on Q=0
87AA F8 10        LDI 10H        ; Load D immediate
87AC AF           PLO RF         ; Put low register RF
87AD 4D           LDA RD         ; Load D from (RD), increment RD
87AE B8           PHI R8         ; Put high register R8
87AF 0D           LDN RD         ; Load D with (RD)
87B0 A8           PLO R8         ; Put low register R8
87B1 0D           LDN RD         ; Load D with (RD)
87B2 FE           SHL            ; Shift left D
87B3 5D           STR RD         ; Store D to (RD)
87B4 2D           DEC RD         ; Decrement (RD)
87B5 0D           LDN RD         ; Load D with (RD)
87B6 7E           SHLC           ; Shift left with carry
87B7 5D           STR RD         ; Store D to (RD)
87B8 D4           SEP R4         ; Set P=R4 as program counter
87B9 88           GLO R8         ; Get low register R8
87BA 22           DEC R2         ; Decrement (R2)
87BB 3B C5        BNF 87C5H      ; Short branch on DF=0
87BD ED           SEX RD         ; Set X=RD as datapointer
87BE 1D           INC RD         ; Increment (RD)
87BF 88           GLO R8         ; Get low register R8
87C0 F4           ADD            ; Add D: D,DF= D+(R(X))
87C1 73           STXD           ; Store via X and decrement
87C2 98           GHI R8         ; Get high register R8
87C3 74           ADC            ; Add with carry
87C4 5D           STR RD         ; Store D to (RD)
87C5 2F           DEC RF         ; Decrement (RF)
87C6 8F           GLO RF         ; Get low register RF
87C7 1D           INC RD         ; Increment (RD)
87C8 3A B1        BNZ 87B1H      ; Short branch on D!=0
87CA D5           SEP R5         ; Set P=R5 as program counter

87CB D4           SEP R4         ; Set P=R4 as program counter
87CC 85           GLO R5         ; Get low register R5
87CD C5           LSNQ           ; Long skip on Q=0
87CE 9A           GHI RA         ; Get high register RA
87CF 52           STR R2         ; Store D to (R2)
87D0 8A           GLO RA         ; Get low register RA
87D1 F1           OR             ; Logical OR  D with (R(X))
87D2 C2 86 7F     LBZ 867FH      ; Long branch on D=0
87D5 0D           LDN RD         ; Load D with (RD)
87D6 F3           XOR            ; Logical exclusive OR  D with (R(X))
87D7 73           STXD           ; Store via X and decrement

87D8 D4           SEP R4         ; Set P=R4 as program counter
87D9 88           GLO R8         ; Get low register R8
87DA 13           INC R3         ; Increment (R3)
87DB 2D           DEC RD         ; Decrement (RD)
87DC 2D           DEC RD         ; Decrement (RD)
87DD D4           SEP R4         ; Set P=R4 as program counter
87DE 88           GLO R8         ; Get low register R8
87DF 13           INC R3         ; Increment (R3)
87E0 1D           INC RD         ; Increment (RD)
87E1 9D           GHI RD         ; Get high register RD
87E2 C8 9D 73     LSKP           ; Long skip
87E5 AA           PLO RA         ; Put low register RA
87E6 BA           PHI RA         ; Put high register RA
87E7 F8 11        LDI 11H        ; Load D immediate
87E9 AF           PLO RF         ; Put low register RF
87EA ED           SEX RD         ; Set X=RD as datapointer
87EB 8A           GLO RA         ; Get low register RA
87EC F7           SM             ; Substract memory: DF,D=D-(R(X))
87ED 52           STR R2         ; Store D to (R2)
87EE 2D           DEC RD         ; Decrement (RD)
87EF 9A           GHI RA         ; Get high register RA
87F0 77           SMB            ; Substract memory with borrow
87F1 3B F6        BNF 87F6H      ; Short branch on DF=0
87F3 BA           PHI RA         ; Put high register RA
87F4 02           LDN R2         ; Load D with (R2)
87F5 AA           PLO RA         ; Put low register RA
87F6 1D           INC RD         ; Increment (RD)
87F7 1D           INC RD         ; Increment (RD)
87F8 1D           INC RD         ; Increment (RD)
87F9 F0           LDX            ; Pop stack. Place value in D register
87FA 7E           SHLC           ; Shift left with carry
87FB 73           STXD           ; Store via X and decrement
87FC F0           LDX            ; Pop stack. Place value in D register
87FD 7E           SHLC           ; Shift left with carry
87FE 73           STXD           ; Store via X and decrement
87FF 8A           GLO RA         ; Get low register RA
8800 7E           SHLC           ; Shift left with carry
8801 D4           SEP R4         ; Set P=R4 as program counter
8802 88           GLO R8         ; Get low register R8
8803 24           DEC R4         ; Decrement (R4)
8804 2F           DEC RF         ; Decrement (RF)
8805 8F           GLO RF         ; Get low register RF
8806 CA 87 EA     LBNZ 87EAH     ; Long branch on D!=0
8809 12           INC R2         ; Increment (R2)
880A 02           LDN R2         ; Load D with (R2)
880B FE           SHL            ; Shift left D
880C 3B 21        BNF 8821H      ; Short branch on DF=0
880E D7           SEP R7         ; Set P=R7 as program counter
880F 1A           INC RA         ; Increment (RA)
8810 AD           PLO RD         ; Put low register RD
8811 30 18        BR  8818H      ; Short branch
8813 ED           SEX RD         ; Set X=RD as datapointer
8814 F0           LDX            ; Pop stack. Place value in D register
8815 FE           SHL            ; Shift left D
8816 3B 21        BNF 8821H      ; Short branch on DF=0
8818 1D           INC RD         ; Increment (RD)

8819 9D           GHI RD         ; Get high register RD
881A F7           SM             ; Substract memory: DF,D=D-(R(X))
881B 73           STXD           ; Store via X and decrement
881C 9D           GHI RD         ; Get high register RD
881D 77           SMB            ; Substract memory with borrow
881E 5D           STR RD         ; Store D to (RD)
881F FF 00        SMI 00H        ; Substract D,DF to value
8821 D5           SEP R5         ; Set P=R5 as program counter

8822 8A           GLO RA         ; Get low register RA
8823 FE           SHL            ; Shift left D
8824 AA           PLO RA         ; Put low register RA
8825 9A           GHI RA         ; Get high register RA
8826 7E           SHLC           ; Shift left with carry
8827 BA           PHI RA         ; Put high register RA
8828 D5           SEP R5         ; Set P=R5 as program counter

8829 D7           SEP R7         ; Set P=R7 as program counter
882A 18           INC R8         ; Increment (R8)
882B C2 86 B1     LBZ 86B1H      ; Long branch on D=0
882E 4B           LDA RB         ; Load D from (RB), increment RB
882F FB 0D        XRI 0DH        ; Logical XOR D with value
8831 3A 2E        BNZ 882EH      ; Short branch on D!=0
8833 D4           SEP R4         ; Set P=R4 as program counter
8834 89           GLO R9         ; Get low register R9
8835 98           GHI R8         ; Get high register R8
8836 32 4B        BZ  884BH      ; Short branch on D=0
8838 D4           SEP R4         ; Set P=R4 as program counter
8839 84           GLO R4         ; Get low register R4
883A 0C           LDN RC         ; Load D with (RC)
883B 33 46        BDF 8846H      ; Short branch on DF=1
883D D7           SEP R7         ; Set P=R7 as program counter
883E 1C           INC RC         ; Increment (RC)
883F B9           PHI R9         ; Put high register R9
8840 4D           LDA RD         ; Load D from (RD), increment RD
8841 A9           PLO R9         ; Put low register R9
8842 D7           SEP R7         ; Set P=R7 as program counter
8843 17           INC R7         ; Increment (R7)
8844 5D           STR RD         ; Store D to (RD)

8845 D5           SEP R5         ; Set P=R5 as program counter
8846 D7           SEP R7         ; Set P=R7 as program counter
8847 1E           INC RE         ; Increment (RE)
8848 B9           PHI R9         ; Put high register R9
8849 4D           LDA RD         ; Load D from (RD), increment RD
884A A9           PLO R9         ; Put low register R9
884B C0 86 7F     LBR 867FH      ; Long branch

884E D7           SEP R7         ; Set P=R7 as program counter
884F 20           DEC R0         ; Decrement (R0)
8850 BB           PHI RB         ; Put high register RB
8851 4D           LDA RD         ; Load D from (RD), increment RD
8852 AB           PLO RB         ; Put low register RB
8853 D4           SEP R4         ; Set P=R4 as program counter
8854 89           GLO R9         ; Get low register R9
8855 98           GHI R8         ; Get high register R8
8856 32 4B        BZ  884BH      ; Short branch on D=0
8858 D7           SEP R7         ; Set P=R7 as program counter
8859 1C           INC RC         ; Increment (RC)
885A 89           GLO R9         ; Get low register R9
885B 73           STXD           ; Store via X and decrement
885C 99           GHI R9         ; Get high register R9
885D 5D           STR RD         ; Store D to (RD)

885E 30 42        BR  8842H      ; Short branch

8860 D4           SEP R4         ; Set P=R4 as program counter
8861 88           GLO R8         ; Get low register R8
8862 FE           SHL            ; Shift left D
8863 32 38        BZ  8838H      ; Short branch on D=0
8865 D7           SEP R7         ; Set P=R7 as program counter
8866 28           DEC R8         ; Decrement (R8)
8867 8A           GLO RA         ; Get low register RA
8868 73           STXD           ; Store via X and decrement
8869 9A           GHI RA         ; Get high register RA
886A 5D           STR RD         ; Store D to (RD)
886B 30 4B        BR  884BH      ; Short branch

886D D4           SEP R4         ; Set P=R4 as program counter
886E 88           GLO R8         ; Get low register R8
886F 8B           GLO RB         ; Get low register RB
8870 42           LDA R2         ; Load D from (R2), increment R2
8871 BA           PHI RA         ; Put high register RA
8872 02           LDN R2         ; Load D with (R2)
8873 AA           PLO RA         ; Put low register RA
8874 D7           SEP R7         ; Set P=R7 as program counter
8875 26           DEC R6         ; Decrement (R6)
8876 82           GLO R2         ; Get low register R2
8877 73           STXD           ; Store via X and decrement
8878 92           GHI R2         ; Get high register R2
8879 73           STXD           ; Store via X and decrement
887A D4           SEP R4         ; Set P=R4 as program counter
887B 89           GLO R9         ; Get low register R9
887C 01           LDN R1         ; Load D with (R1)
887D 3A 65        BNZ 8865H      ; Short branch on D!=0
887F 30 88        BR  8888H      ; Short branch

8881 D4           SEP R4         ; Set P=R4 as program counter
8882 88           GLO R8         ; Get low register R8
8883 8B           GLO RB         ; Get low register RB
8884 42           LDA R2         ; Load D from (R2), increment R2
8885 B9           PHI R9         ; Put high register R9
8886 02           LDN R2         ; Load D with (R2)
8887 A9           PLO R9         ; Put low register R9
8888 C0 85 2D     LBR 852DH      ; Long branch

888B D7           SEP R7         ; Set P=R7 as program counter
888C 22           DEC R2         ; Decrement (R2)
888D 12           INC R2         ; Increment (R2)
888E 12           INC R2         ; Increment (R2)
888F 82           GLO R2         ; Get low register R2
8890 FC 02        ADI 02H        ; Add D,DF with value
8892 F3           XOR            ; Logical exclusive OR  D with (R(X))
8893 2D           DEC RD         ; Decrement (RD)
8894 3A 9C        BNZ 889CH      ; Short branch on D!=0
8896 92           GHI R2         ; Get high register R2
8897 7C 00        ADCI 00H       ; Add with carry immediate
8899 F3           XOR            ; Logical exclusive OR  D with (R(X))
889A 32 4B        BZ  884BH      ; Short branch on D=0

889C 12           INC R2         ; Increment (R2)
889D D5           SEP R5         ; Set P=R5 as program counter

889E D7           SEP R7         ; Set P=R7 as program counter
889F 16           INC R6         ; Increment (R6)
88A0 38 9D        SKP            ; Skip next byte

88A2 FE           SHL            ; Shift left D
88A3 D7           SEP R7         ; Set P=R7 as program counter
88A4 1A           INC RA         ; Increment (RA)
88A5 9D           GHI RD         ; Get high register RD
88A6 76           SHRC           ; Shift right with carry
88A7 5D           STR RD         ; Store D to (RD)
88A8 30 B2        BR  88B2H      ; Short branch

88AA F8 30        LDI 30H        ; Load D immediate
88AC AB           PLO RB         ; Put low register RB
88AD D4           SEP R4         ; Set P=R4 as program counter
88AE 86           GLO R6         ; Get low register R6
88AF 54           STR R4         ; Store D to (R4)
88B0 9D           GHI RD         ; Get high register RD
88B1 BB           PHI RB         ; Put high register RB
88B2 D4           SEP R4         ; Set P=R4 as program counter
88B3 84           GLO R4         ; Get low register R4
88B4 06           LDN R6         ; Load D with (R6)
88B5 FA 7F        ANI 7FH        ; Logical AND D with value
88B7 32 B2        BZ  88B2H      ; Short branch on D=0
88B9 52           STR R2         ; Store D to (R2)
88BA FB 7F        XRI 7FH        ; Logical XOR D with value
88BC 32 B2        BZ  88B2H      ; Short branch on D=0
88BE FB 75        XRI 75H        ; Logical XOR D with value
88C0 32 9E        BZ  889EH      ; Short branch on D=0
88C2 FB 19        XRI 19H        ; Logical XOR D with value
88C4 32 A1        BZ  88A1H      ; Short branch on D=0
88C6 D7           SEP R7         ; Set P=R7 as program counter
88C7 13           INC R3         ; Increment (R3)
88C8 02           LDN R2         ; Load D with (R2)
88C9 F3           XOR            ; Logical exclusive OR  D with (R(X))
88CA 32 D7        BZ  88D7H      ; Short branch on D=0
88CC 2D           DEC RD         ; Decrement (RD)
88CD 02           LDN R2         ; Load D with (R2)
88CE F3           XOR            ; Logical exclusive OR  D with (R(X))
88CF 3A DD        BNZ 88DDH      ; Short branch on D!=0
88D1 2B           DEC RB         ; Decrement (RB)
88D2 8B           GLO RB         ; Get low register RB
88D3 FF 30        SMI 30H        ; Substract D,DF to value
88D5 33 B2        BDF 88B2H      ; Short branch on DF=1
88D7 F8 30        LDI 30H        ; Load D immediate
88D9 AB           PLO RB         ; Put low register RB
88DA F8 0D        LDI 0DH        ; Load D immediate
88DC 38 02        SKP            ; Skip next byte
88DE 5B           STR RB         ; Store D to (RB)
88DF D7           SEP R7         ; Set P=R7 as program counter
88E0 19           INC R9         ; Increment (R9)
88E1 8B           GLO RB         ; Get low register RB
88E2 F7           SM             ; Substract memory: DF,D=D-(R(X))
88E3 3B EC        BNF 88ECH      ; Short branch on DF=0
88E5 F8 07        LDI 07H        ; Load D immediate
88E7 D4           SEP R4         ; Set P=R4 as program counter
88E8 86           GLO R6         ; Get low register R6
88E9 F4           ADD            ; Add D: D,DF= D+(R(X))
88EA 2B           DEC RB         ; Decrement (RB)
88EB 2B           DEC RB         ; Decrement (RB)
88EC 4B           LDA RB         ; Load D from (RB), increment RB
88ED FB 0D        XRI 0DH        ; Logical XOR D with value
88EF 3A B2        BNZ 88B2H      ; Short branch on D!=0

88F1 D4           SEP R4         ; Set P=R4 as program counter
88F2 86           GLO R6         ; Get low register R6
88F3 D5           SEP R5         ; Set P=R5 as program counter
88F4 D7           SEP R7         ; Set P=R7 as program counter
88F5 18           INC R8         ; Increment (R8)
88F6 8B           GLO RB         ; Get low register RB
88F7 5D           STR RD         ; Store D to (RD)
88F8 F8 30        LDI 30H        ; Load D immediate
88FA AB           PLO RB         ; Put low register RB
88FB C0 85 C5     LBR 85C5H      ; Long branch

88FE D4           SEP R4         ; Set P=R4 as program counter
88FF 85           GLO R5         ; Get low register R5
8900 C5           LSNQ           ; Long skip on Q=0
8901 8A           GLO RA         ; Get low register RA
8902 52           STR R2         ; Store D to (R2)
8903 9A           GHI RA         ; Get high register RA
8904 F1           OR             ; Logical OR  D with (R(X))
8905 C2 86 7F     LBZ 867FH      ; Long branch on D=0
8908 D7           SEP R7         ; Set P=R7 as program counter
8909 20           DEC R0         ; Decrement (R0)
890A BB           PHI RB         ; Put high register RB
890B 4D           LDA RD         ; Load D from (RD), increment RD
890C AB           PLO RB         ; Put low register RB
890D D4           SEP R4         ; Set P=R4 as program counter
890E 89           GLO R9         ; Get low register R9
890F 98           GHI R8         ; Get high register R8
8910 C6           LSNZ           ; Long skip on D!=0
8911 8D           GLO RD         ; Get low register RD
8912 D5           SEP R5         ; Set P=R5 as program counter
8913 ED           SEX RD         ; Set X=RD as datapointer
8914 8A           GLO RA         ; Get low register RA
8915 F5           SD             ; Substract D: D,DF=(R(X))-D
8916 52           STR R2         ; Store D to (R2)
8917 9A           GHI RA         ; Get high register RA
8918 2D           DEC RD         ; Decrement (RD)
8919 75           SDB            ; Substract D with borrow
891A E2           SEX R2         ; Set X=R2 as datapointer
891B F1           OR             ; Logical OR  D with (R(X))
891C 33 12        BDF 8912H      ; Short branch on DF=1
891E 4B           LDA RB         ; Load D from (RB), increment RB
891F FB 0D        XRI 0DH        ; Logical XOR D with value
8921 3A 1E        BNZ 891EH      ; Short branch on D!=0
8923 30 0D        BR  890DH      ; Short branch

8925 D4           SEP R4         ; Set P=R4 as program counter
8926 89           GLO R9         ; Get low register R9
8927 28           DEC R8         ; Decrement (R8)
8928 D4           SEP R4         ; Set P=R4 as program counter
8929 85           GLO R5         ; Get low register R5
892A C5           LSNQ           ; Long skip on Q=0
892B 4D           LDA RD         ; Load D from (RD), increment RD
892C B8           PHI R8         ; Put high register R8
892D 4D           LDA RD         ; Load D from (RD), increment RD
892E A8           PLO R8         ; Put low register R8
892F 4D           LDA RD         ; Load D from (RD), increment RD
8930 B6           PHI R6         ; Put high register R6
8931 4D           LDA RD         ; Load D from (RD), increment RD
8932 A6           PLO R6         ; Put low register R6
8933 8D           GLO RD         ; Get low register RD
8934 52           STR R2         ; Store D to (R2)
8935 D7           SEP R7         ; Set P=R7 as program counter
8936 19           INC R9         ; Increment (R9)
8937 02           LDN R2         ; Load D with (R2)

8938 5D           STR RD         ; Store D to (RD)
8939 AD           PLO RD         ; Put low register RD
893A 8A           GLO RA         ; Get low register RA
893B D5           SEP R5         ; Set P=R5 as program counter

893C D7           SEP R7         ; Set P=R7 as program counter
893D 2C           DEC RC         ; Decrement (RC)
893E 8B           GLO RB         ; Get low register RB
893F 73           STXD           ; Store via X and decrement
8940 9B           GHI RB         ; Get high register RB
8941 5D           STR RD         ; Store D to (RD)
8942 D4           SEP R4         ; Set P=R4 as program counter
8943 88           GLO R8         ; Get low register R8
8944 FE           SHL            ; Shift left D
8945 D7           SEP R7         ; Set P=R7 as program counter
8946 2A           DEC RA         ; Decrement (RA)
8947 8B           GLO RB         ; Get low register RB
8948 73           STXD           ; Store via X and decrement
8949 9B           GHI RB         ; Get high register RB
894A 73           STXD           ; Store via X and decrement
894B D4           SEP R4         ; Set P=R4 as program counter
894C 88           GLO R8         ; Get low register R8
894D FE           SHL            ; Shift left D
894E 2B           DEC RB         ; Decrement (RB)
894F 2B           DEC RB         ; Decrement (RB)
8950 D7           SEP R7         ; Set P=R7 as program counter
8951 2A           DEC RA         ; Decrement (RA)
8952 8B           GLO RB         ; Get low register RB
8953 F7           SM             ; Substract memory: DF,D=D-(R(X))
8954 2D           DEC RD         ; Decrement (RD)
8955 9B           GHI RB         ; Get high register RB
8956 77           SMB            ; Substract memory with borrow
8957 33 7B        BDF 897BH      ; Short branch on DF=1
8959 4B           LDA RB         ; Load D from (RB), increment RB
895A BA           PHI RA         ; Put high register RA
895B 4B           LDA RB         ; Load D from (RB), increment RB
895C AA           PLO RA         ; Put low register RA
895D 3A 62        BNZ 8962H      ; Short branch on D!=0
895F 9A           GHI RA         ; Get high register RA
8960 32 7B        BZ  897BH      ; Short branch on D=0
8962 D4           SEP R4         ; Set P=R4 as program counter
8963 87           GLO R7         ; Get low register R7
8964 15           INC R5         ; Increment (R5)
8965 F8 2D        LDI 2DH        ; Load D immediate
8967 FB 0D        XRI 0DH        ; Logical XOR D with value
8969 D4           SEP R4         ; Set P=R4 as program counter
896A 86           GLO R6         ; Get low register R6
896B F4           ADD            ; Add D: D,DF= D+(R(X))
896C D4           SEP R4         ; Set P=R4 as program counter
896D 84           GLO R4         ; Get low register R4
896E 0C           LDN RC         ; Load D with (RC)
896F 33 7B        BDF 897BH      ; Short branch on DF=1
8971 4B           LDA RB         ; Load D from (RB), increment RB
8972 FB 0D        XRI 0DH        ; Logical XOR D with value
8974 3A 67        BNZ 8967H      ; Short branch on D!=0
8976 D4           SEP R4         ; Set P=R4 as program counter
8977 86           GLO R6         ; Get low register R6
8978 D5           SEP R5         ; Set P=R5 as program counter
8979 30 50        BR  8950H      ; Short branch

897B D7           SEP R7         ; Set P=R7 as program counter
897C 2C           DEC RC         ; Decrement (RC)
897D BB           PHI RB         ; Put high register RB
897E 4D           LDA RD         ; Load D from (RD), increment RD
897F AB           PLO RB         ; Put low register RB
8980 D5           SEP R5         ; Set P=R5 as program counter

8981 D7           SEP R7         ; Set P=R7 as program counter
8982 26           DEC R6         ; Decrement (R6)
8983 82           GLO R2         ; Get low register R2
8984 73           STXD           ; Store via X and decrement
8985 92           GHI R2         ; Get high register R2
8986 5D           STR RD         ; Store D to (RD)
8987 D7           SEP R7         ; Set P=R7 as program counter
8988 18           INC R8         ; Increment (R8)
8989 2D           DEC RD         ; Decrement (RD)
898A CE           LSZ            ; Long skip on D=0
898B D7           SEP R7         ; Set P=R7 as program counter
898C 28           DEC R8         ; Decrement (R8)
898D AA           PLO RA         ; Put low register RA
898E 4D           LDA RD         ; Load D from (RD), increment RD
898F 12           INC R2         ; Increment (R2)
8990 12           INC R2         ; Increment (R2)
8991 E2           SEX R2         ; Set X=R2 as datapointer
8992 73           STXD           ; Store via X and decrement
8993 8A           GLO RA         ; Get low register RA
8994 73           STXD           ; Store via X and decrement
8995 C0 85 2D     LBR 852DH      ; Long branch

8998 D7           SEP R7         ; Set P=R7 as program counter
8999 27           DEC R7         ; Decrement (R7)
899A 4B           LDA RB         ; Load D from (RB), increment RB
899B 5D           STR RD         ; Store D to (RD)
899C 1D           INC RD         ; Increment (RD)
899D 4B           LDA RB         ; Load D from (RB), increment RB
899E 73           STXD           ; Store via X and decrement
899F F1           OR             ; Logical OR  D with (R(X))
89A0 1D           INC RD         ; Increment (RD)
89A1 D5           SEP R5         ; Set P=R5 as program counter

89A2 D4           SEP R4         ; Set P=R4 as program counter
89A3 87           GLO R7         ; Get low register R7
89A4 5E           STR RE         ; Store D to (RE)
89A5 D4           SEP R4         ; Set P=R4 as program counter
89A6 88           GLO R8         ; Get low register R8
89A7 FE           SHL            ; Shift left D
89A8 FC FF        ADI FFH        ; Add D,DF with value
89AA 9D           GHI RD         ; Get high register RD
89AB AF           PLO RF         ; Put low register RF
89AC 33 BA        BDF 89BAH      ; Short branch on DF=1
89AE 9B           GHI RB         ; Get high register RB
89AF BD           PHI RD         ; Put high register RD
89B0 8B           GLO RB         ; Get low register RB
89B1 AD           PLO RD         ; Put low register RD
89B2 2F           DEC RF         ; Decrement (RF)
89B3 2F           DEC RF         ; Decrement (RF)
89B4 2F           DEC RF         ; Decrement (RF)
89B5 4D           LDA RD         ; Load D from (RD), increment RD
89B6 FB 0D        XRI 0DH        ; Logical XOR D with value
89B8 3A B4        BNZ 89B4H      ; Short branch on D!=0
89BA 2B           DEC RB         ; Decrement (RB)
89BB 2B           DEC RB         ; Decrement (RB)
89BC D4           SEP R4         ; Set P=R4 as program counter
89BD 87           GLO R7         ; Get low register R7
89BE 5E           STR RE         ; Store D to (RE)
89BF D7           SEP R7         ; Set P=R7 as program counter
89C0 28           DEC R8         ; Decrement (R8)
89C1 0B           LDN RB         ; Load D with (RB)
89C2 FB 0D        XRI 0DH        ; Logical XOR D with value

89C4 73           STXD           ; Store via X and decrement
89C5 5D           STR RD         ; Store D to (RD)
89C6 32 D9        BZ  89D9H      ; Short branch on D=0
89C8 9A           GHI RA         ; Get high register RA
89C9 5D           STR RD         ; Store D to (RD)
89CA 1D           INC RD         ; Increment (RD)
89CB 8A           GLO RA         ; Get low register RA
89CC 5D           STR RD         ; Store D to (RD)
89CD 9B           GHI RB         ; Get high register RB
89CE BA           PHI RA         ; Put high register RA
89CF 8B           GLO RB         ; Get low register RB
89D0 AA           PLO RA         ; Put low register RA
89D1 00           IDL            ; Idle or wait for interrupt or DMA request
89D2 1F           INC RF         ; Increment (RF)
89D3 1F           INC RF         ; Increment (RF)
89D4 4A           LDA RA         ; Load D from (RA), increment RA
89D5 FB 0D        XRI 0DH        ; Logical XOR D with value
89D7 3A D3        BNZ 89D3H      ; Short branch on D!=0
89D9 D7           SEP R7         ; Set P=R7 as program counter
89DA 2E           DEC RE         ; Decrement (RE)
89DB BA           PHI RA         ; Put high register RA
89DC 4D           LDA RD         ; Load D from (RD), increment RD
89DD AA           PLO RA         ; Put low register RA
89DE D7           SEP R7         ; Set P=R7 as program counter
89DF 24           DEC R4         ; Decrement (R4)
89E0 8A           GLO RA         ; Get low register RA
89E1 F7           SM             ; Substract memory: DF,D=D-(R(X))
89E2 AA           PLO RA         ; Put low register RA
89E3 2D           DEC RD         ; Decrement (RD)
89E4 9A           GHI RA         ; Get high register RA
89E5 77           SMB            ; Substract memory with borrow
89E6 BA           PHI RA         ; Put high register RA
89E7 1D           INC RD         ; Increment (RD)
89E8 8F           GLO RF         ; Get low register RF
89E9 F4           ADD            ; Add D: D,DF= D+(R(X))
89EA BF           PHI RF         ; Put high register RF
89EB 8F           GLO RF         ; Get low register RF
89EC FA 80        ANI 80H        ; Logical AND D with value
89EE CE           LSZ            ; Long skip on D=0
89EF F8 FF        LDI FFH        ; Load D immediate
89F1 2D           DEC RD         ; Decrement (RD)
89F2 74           ADC            ; Add with carry
89F3 E2           SEX R2         ; Set X=R2 as datapointer
89F4 73           STXD           ; Store via X and decrement
89F5 B8           PHI R8         ; Put high register R8
89F6 9F           GHI RF         ; Get high register RF
89F7 73           STXD           ; Store via X and decrement
89F8 52           STR R2         ; Store D to (R2)
89F9 82           GLO R2         ; Get low register R2
89FA F5           SD             ; Substract D: D,DF=(R(X))-D
89FB 98           GHI R8         ; Get high register R8
89FC 52           STR R2         ; Store D to (R2)
89FD 92           GHI R2         ; Get high register R2

89FE 75           SDB            ; Substract D with borrow
89FF C3 86 7E     LBDF 867EH     ; Long branch on DF=1
8A02 8F           GLO RF         ; Get low register RF
8A03 32 30        BZ  8A30H      ; Short branch on D=0
8A05 52           STR R2         ; Store D to (R2)
8A06 FE           SHL            ; Shift left D
8A07 3B 1E        BNF 8A1EH      ; Short branch on DF=0
8A09 D7           SEP R7         ; Set P=R7 as program counter
8A0A 2E           DEC RE         ; Decrement (RE)
8A0B BF           PHI RF         ; Put high register RF
8A0C 4D           LDA RD         ; Load D from (RD), increment RD
8A0D AF           PLO RF         ; Put low register RF
8A0E E2           SEX R2         ; Set X=R2 as datapointer
8A0F F7           SM             ; Substract memory: DF,D=D-(R(X))
8A10 A8           PLO R8         ; Put low register R8
8A11 9F           GHI RF         ; Get high register RF
8A12 7C 00        ADCI 00H       ; Add with carry immediate
8A14 B8           PHI R8         ; Put high register R8
8A15 48           LDA R8         ; Load D from (R8), increment R8
8A16 5F           STR RF         ; Store D to (RF)
8A17 1F           INC RF         ; Increment (RF)
8A18 1A           INC RA         ; Increment (RA)
8A19 9A           GHI RA         ; Get high register RA
8A1A 3A 15        BNZ 8A15H      ; Short branch on D!=0
8A1C 30 30        BR  8A30H      ; Short branch
8A1E 9F           GHI RF         ; Get high register RF
8A1F AF           PLO RF         ; Put low register RF
8A20 98           GHI R8         ; Get high register R8
8A21 BF           PHI RF         ; Put high register RF
8A22 D7           SEP R7         ; Set P=R7 as program counter
8A23 24           DEC R4         ; Decrement (R4)
8A24 B8           PHI R8         ; Put high register R8
8A25 4D           LDA RD         ; Load D from (RD), increment RD
8A26 A8           PLO R8         ; Put low register R8
8A27 2A           DEC RA         ; Decrement (RA)
8A28 EF           SEX RF         ; Set X=RF as datapointer
8A29 08           LDN R8         ; Load D with (R8)
8A2A 28           DEC R8         ; Decrement (R8)
8A2B 73           STXD           ; Store via X and decrement
8A2C 1A           INC RA         ; Increment (RA)
8A2D 9A           GHI RA         ; Get high register RA
8A2E 3A 29        BNZ 8A29H      ; Short branch on D!=0
8A30 D7           SEP R7         ; Set P=R7 as program counter
8A31 24           DEC R4         ; Decrement (R4)
8A32 12           INC R2         ; Increment (R2)
8A33 42           LDA R2         ; Load D from (R2), increment R2
8A34 73           STXD           ; Store via X and decrement
8A35 02           LDN R2         ; Load D with (R2)
8A36 5D           STR RD         ; Store D to (RD)
8A37 D7           SEP R7         ; Set P=R7 as program counter
8A38 2E           DEC RE         ; Decrement (RE)
8A39 BA           PHI RA         ; Put high register RA
8A3A 4D           LDA RD         ; Load D from (RD), increment RD
8A3B AA           PLO RA         ; Put low register RA
8A3C D7           SEP R7         ; Set P=R7 as program counter

8A3D 28           DEC R8         ; Decrement (R8)
8A3E AF           PLO RF         ; Put low register RF
8A3F 0F           LDN RF         ; Load D with (RF)
8A40 32 4E        BZ  8A4EH      ; Short branch on D=0
8A42 8F           GLO RF         ; Get low register RF
8A43 5A           STR RA         ; Store D to (RA)
8A44 1A           INC RA         ; Increment (RA)
8A45 4D           LDA RD         ; Load D from (RD), increment RD
8A46 5A           STR RA         ; Store D to (RA)
8A47 1A           INC RA         ; Increment (RA)
8A48 4B           LDA RB         ; Load D from (RB), increment RB
8A49 5A           STR RA         ; Store D to (RA)
8A4A FB 0D        XRI 0DH        ; Logical XOR D with value
8A4C 3A 47        BNZ 8A47H      ; Short branch on D!=0
8A4E C0 86 B5     LBR 86B5H      ; Long branch

8A51 73           STXD           ; Store via X and decrement
8A52 52           STR R2         ; Store D to (R2)
8A53 9D           GHI RD         ; Get high register RD
8A54 BA           PHI RA         ; Put high register RA
8A55 2D           DEC RD         ; Decrement (RD)
8A56 43           LDA R3         ; Load D from (R3), increment R3
8A57 D5           SEP R5         ; Set P=R5 as program counter
8A58 5D           STR RD         ; Store D to (RD)
8A59 2D           DEC RD         ; Decrement (RD)
8A5A 88           GLO R8         ; Get low register R8
8A5B FA 0F        ANI 0FH        ; Logical AND D with value
8A5D F9 60        ORI 60H        ; Logical OR D with value
8A5F 5D           STR RD         ; Store D to (RD)
8A60 FA 08        ANI 08H        ; Logical AND D with value
8A62 CE           LSZ            ; Long skip on D=0
8A63 C4           NOP            ; No operation
8A64 12           INC R2         ; Increment (R2)
8A65 DD           SEP RD         ; Set P=RD as program counter

8A66 FC 00        ADI 00H        ; Add D,DF with value

8A68 35 6E        B2  8A6EH      ; Short branch on EF2=1

8A6A FF 00        SMI 00H        ; Substract D,DF to value

8A6C 3D 6C        BN2 8A6CH      ; Short branch on EF2=0

8A6E D5           SEP R5         ; Set P=R5 as program counter



; major diff here
8A6F D7           SEP R7         ; Set P=R7 as program counter
8A70 11           INC R1         ; Increment (R1)
8A71 8D           GLO RD         ; Get low register RD
8A72 73           STXD           ; Store via X and decrement
8A73 C0 81 40     LBR 8140H      ; Long branch
8A76 D7           SEP R7         ; Set P=R7 as program counter
8A77 12           INC R2         ; Increment (R2)
8A78 32 7E        BZ  8A7EH      ; Short branch on D=0
8A7A DC           SEP RC         ; Set P=RC as program counter
8A7B 17           INC R7         ; Increment (R7)
8A7C 2D           DEC RD         ; Decrement (RD)
8A7D 5D           STR RD         ; Store D to (RD)
8A7E C0 81 A4     LBR 81A4H      ; Long branch
8A81 24           DEC R4         ; Decrement (R4)
8A82 3A 91        BNZ 8A91H      ; Short branch on D!=0
8A84 27           DEC R7         ; Decrement (R7)
8A85 10           INC R0         ; Increment (R0)
8A86 E1           SEX R1         ; Set X=R1 as datapointer
8A87 59           STR R9         ; Store D to (R9)
8A88 C5           LSNQ           ; Long skip on Q=0
8A89 2A           DEC RA         ; Decrement (RA)
8A8A 56           STR R6         ; Store D to (R6)
8A8B 10           INC R0         ; Increment (R0)
8A8C 11           INC R1         ; Increment (R1)
8A8D 2C           DEC RC         ; Decrement (RC)
8A8E 8B           GLO RB         ; Get low register RB
8A8F 4C           LDA RC         ; Load D from (RC), increment RC
8A90 45           LDA R5         ; Load D from (R5), increment R5
8A91 D4           SEP R4         ; Set P=R4 as program counter
8A92 A0           PLO R0         ; Put low register R0
8A93 80           GLO R0         ; Get low register R0
8A94 BD           PHI RD         ; Put high register RD
8A95 30 BC        BR  8ABCH      ; Short branch
8A97 E0           SEX R0         ; Set X=R0 as datapointer
8A98 13           INC R3         ; Increment (R3)
8A99 1D           INC RD         ; Increment (RD)
8A9A 94           GHI R4         ; Get high register R4
8A9B 47           LDA R7         ; Load D from (R7), increment R7
8A9C CF           LSDF           ; Long skip on DF=1
8A9D 88           GLO R8         ; Get low register R8
8A9E 54           STR R4         ; Store D to (R4)
8A9F CF           LSDF           ; Long skip on DF=1
8AA0 30 BC        BR  8ABCH      ; Short branch
8AA2 E0           SEX R0         ; Set X=R0 as datapointer
8AA3 10           INC R0         ; Increment (R0)
8AA4 11           INC R1         ; Increment (R1)
8AA5 16           INC R6         ; Increment (R6)
8AA6 80           GLO R0         ; Get low register R0
8AA7 53           STR R3         ; Store D to (R3)
8AA8 55           STR R5         ; Store D to (R5)
8AA9 C2 30 BC     LBZ 30BCH      ; Long branch on D=0
8AAC E0           SEX R0         ; Set X=R0 as datapointer
8AAD 14           INC R4         ; Increment (R4)
8AAE 16           INC R6         ; Increment (R6)
8AAF 90           GHI R0         ; Get high register R0
8AB0 50           STR R0         ; Store D to (R0)
8AB1 D2           SEP R2         ; Set P=R2 as program counter
8AB2 83           GLO R3         ; Get low register R3

;aligned with 06AC
8AB3 49 4E                       ; "IN" (int)
8AB5 D4                          ; 'T' + 0x80

8AB6 E5           
8AB7 71           
8AB8 88           
8AB9 BB                 DB  (";"+80H)       
8ABA E1          P7:	DB  BE+P8-$	; BE P8 
8ABB 1D           
8ABC 8F           
8ABD A2                 DB  ('"'+80H)
8ABE 21           
8ABF 58           
8AC0 6F           
8AC1 83           P5:	DB  BC+P6-$	; BC P6 ","
8AC2 AC                 DB  (","+80H)       
8AC3 22                 DB  PT		; PT
8AC4 55           
8AC5 83           P9:	DB  BC+P10-$	; BC P10 "^"
8AC6 BA                 DB  (":"+80H)     
8AC7 24 93              DB  PS,93H	; PS "<DC3>"	PRINT <DC3>=13H=^S
8AC9 E0           P10:	DB  BE+1	; BE *
8ACA 23           P12:	DB  NL		; NL		THEN <CR><LF>
8ACB 1D           P11:	DB  NX		; NX
8ACC 30 BC        
8ACE 20           
8ACF 48           
8AD0 91           

; aligned with 06CF
8AD1 49                          ; 'I' (if)
8AD2 C6                          ; 'F'+0x80

8AD3 30 BC        BR  8ABCH      ; Short branch
8AD5 31 34        BQ  8A34H      ; Short branch on Q=1
8AD7 30 BC        BR  8ABCH      ; Short branch
8AD9 84           GLO R4         ; Get low register R4

8ADA 54 48 45                    ; "THE" (then)
8ADD CE                          ; 'N'+0x80

8ADE 1C           I1:	DB  CP		; CP
8ADF 1D                 DB  NX		; NX
8AE0 38 0D              DW  JU+GOTO-STRT; JU STMT

8AE2 9A           

8AE3 49 4E 50 55                 ; "INPU" (input)
8AE7 D4                          ; 'T'+0x80

8AE8 A0           
8AE9 10           
8AEA E7           
8AEB 24           
8AEC 3F 20        
8AEE 91           
8AEF 27           
8AF0 E1           
8AF1 59           
8AF2 81           
8AF3 AC           
8AF4 30 BC        
8AF6 13           
8AF7 11           
8AF8 82           
8AF9 AC           
8AFA 4D           
8AFB E0           
8AFC 1D           
8AFD 89           

8AFE 52 45 54 55 52              ; "RETUR" (return)
8B03 CE                          ; 'N'+0x80

8B04 E0           SEX R0         ; Set X=R0 as datapointer
8B05 15           INC R5         ; Increment (R5)
8B06 1D           INC RD         ; Increment (RD)
8B07 85           GLO R5         ; Get low register R5

8B08 45 4E                       ; "EN" (end)
8B0A C4                          ; 'D"+0x80

8B0B E0           SEX R0         ; Set X=R0 as datapointer
8B0C 2D           DEC RD         ; Decrement (RD)
8B0D 98           GHI R8         ; Get high register R8

8B0E 4C 49 53                    ; "LIS" (list)
8B11 D4                          ; 'T"+0x80

8B12 6C           INP 4          ; Input to (R(X)) and D, N=100
8B13 24           DEC R4         ; Decrement (R4)
8B14 00           IDL            ; Idle or wait for interrupt or DMA request
8B15 00           IDL            ; Idle or wait for interrupt or DMA request
8B16 00           IDL            ; Idle or wait for interrupt or DMA request
8B17 00           IDL            ; Idle or wait for interrupt or DMA request
8B18 0A           LDN RA         ; Load D with (RA)
8B19 80           GLO R0         ; Get low register R0
8B1A 1F           INC RF         ; Increment (RF)
8B1B 24           DEC R4         ; Decrement (R4)
8B1C 93           GHI R3         ; Get high register R3
8B1D 23           DEC R3         ; Decrement (R3)
8B1E 1D           INC RD         ; Increment (RD)
8B1F E2           SEX R2         ; Set X=R2 as datapointer
8B20 39 57        BNQ 8B57H      ; Short branch on Q=0
8B22 30 BC        BR  8BBCH      ; Short branch
8B24 39 5E        BNQ 8B5EH      ; Short branch on Q=0
8B26 85           GLO R5         ; Get low register R5

8B27 52 55                       ; "RU" (run) 
8B29 CE                          ; 'N'+0x80

8B2A 38 0A        SKP            ; Skip next byte
8B2C 86           GLO R6         ; Get low register R6

8B2D 43 4C 45 41                 ; "CLEA" (clear)
8B31 D2                          ; 'R'+0x80

8B32 2B           DEC RB         ; Decrement (RB)
8B33 84           GLO R4         ; Get low register R4

8B34 52 45                       ; "RE" (rem)
8B36 CD                          ; 'M'+0x80

8B37 1D           INC RD         ; Increment (RD)
8B38 A0           PLO R0         ; Put low register R0
8B39 80           GLO R0         ; Get low register R0
8B3A BD           PHI RD         ; Put high register RD
8B3B 38 14        SKP            ; Skip next byte
8B3D 85           GLO R5         ; Get low register R5
8B3E AD           PLO RD         ; Put low register RD
8B3F 30 D3        BR  8BD3H      ; Short branch
8B41 17           INC R7         ; Increment (R7)
8B42 64           OUT 4          ; Output (R(X)); Increment R(X), N=100
8B43 81           GLO R1         ; Get low register R1
8B44 AB           PLO RB         ; Put low register RB
8B45 30 D3        BR  8BD3H      ; Short branch
8B47 85           GLO R5         ; Get low register R5
8B48 AB           PLO RB         ; Put low register RB
8B49 30 D3        BR  8BD3H      ; Short branch
8B4B 18           INC R8         ; Increment (R8)
8B4C 5A           STR RA         ; Store D to (RA)
8B4D 85           GLO R5         ; Get low register R5
8B4E AD           PLO RD         ; Put low register RD
8B4F 30 D3        BR  8BD3H      ; Short branch
8B51 19           INC R9         ; Increment (R9)
8B52 54           STR R4         ; Store D to (R4)
8B53 2F           DEC RF         ; Decrement (RF)
8B54 30 E2        BR  8BE2H      ; Short branch
8B56 85           GLO R5         ; Get low register R5
8B57 AA           PLO RA         ; Put low register RA
8B58 30 E2        BR  8BE2H      ; Short branch
8B5A 1A           INC RA         ; Increment (RA)
8B5B 5A           STR RA         ; Store D to (RA)
8B5C 85           GLO R5         ; Get low register R5
8B5D AF           PLO RF         ; Put low register RF
8B5E 30 E2        BR  8BE2H      ; Short branch
8B60 1B           INC RB         ; Increment (RB)
8B61 54           STR R4         ; Store D to (R4)
8B62 2F           DEC RF         ; Decrement (RF)
8B63 97           GHI R7         ; Get high register R7
8B64 52           STR R2         ; Store D to (R2)
8B65 4E           LDA RE         ; Load D from (RE), increment RE
8B66 C4           NOP            ; No operation
8B67 0A           LDN RA         ; Load D with (RA)
8B68 80           GLO R0         ; Get low register R0
8B69 80           GLO R0         ; Get low register R0
8B6A 12           INC R2         ; Increment (R2)
8B6B 0A           LDN RA         ; Load D with (RA)
8B6C 09           LDN R9         ; Load D with (R9)
8B6D 29           DEC R9         ; Decrement (R9)
8B6E 1A           INC RA         ; Increment (RA)
8B6F 0A           LDN RA         ; Load D with (RA)
8B70 1A           INC RA         ; Increment (RA)
8B71 85           GLO R5         ; Get low register R5
8B72 18           INC R8         ; Increment (R8)
8B73 13           INC R3         ; Increment (R3)
8B74 09           LDN R9         ; Load D with (R9)
8B75 80           GLO R0         ; Get low register R0
8B76 12           INC R2         ; Increment (R2)
8B77 0B           LDN RB         ; Load D with (RB)
8B78 31 30        BQ  8B30H      ; Short branch on Q=1
8B7A 61           OUT 1          ; Output (R(X)); Increment R(X), N=001
8B7B 73           STXD           ; Store via X and decrement
8B7C 0B           LDN RB         ; Load D with (RB)
8B7D 02           LDN R2         ; Load D with (R2)
8B7E 04           LDN R4         ; Load D with (R4)
8B7F 02           LDN R2         ; Load D with (R2)
8B80 03           LDN R3         ; Load D with (R3)
8B81 05           LDN R5         ; Load D with (R5)
8B82 03           LDN R3         ; Load D with (R3)
8B83 1B           INC RB         ; Increment (RB)
8B84 1A           INC RA         ; Increment (RA)
8B85 19           INC R9         ; Increment (R9)
8B86 0B           LDN RB         ; Load D with (RB)
8B87 09           LDN R9         ; Load D with (R9)
8B88 06           LDN R6         ; Load D with (R6)
8B89 0A           LDN RA         ; Load D with (RA)
8B8A 00           IDL            ; Idle or wait for interrupt or DMA request
8B8B 00           IDL            ; Idle or wait for interrupt or DMA request
8B8C 1C           INC RC         ; Increment (RC)
8B8D 17           INC R7         ; Increment (R7)
8B8E 2F           DEC RF         ; Decrement (RF)
8B8F 8F           GLO RF         ; Get low register RF
8B90 55           STR R5         ; Store D to (R5)
8B91 53           STR R3         ; Store D to (R3)
8B92 D2           SEP R2         ; Set P=R2 as program counter
8B93 80           GLO R0         ; Get low register R0
8B94 A8           PLO R8         ; Put low register R8
8B95 30 BC        BR  8BBCH      ; Short branch
8B97 31 2A        BQ  8B2AH      ; Short branch on Q=1
8B99 31 2A        BQ  8B2AH      ; Short branch on Q=1
8B9B 80           GLO R0         ; Get low register R0
8B9C A9           PLO R9         ; Put low register R9
8B9D 2E           DEC RE         ; Decrement (RE)
8B9E 2F           DEC RF         ; Decrement (RF)
8B9F A2           PLO R2         ; Put low register R2
8BA0 12           INC R2         ; Increment (R2)
8BA1 2F           DEC RF         ; Decrement (RF)
8BA2 C1 2F 80     LBQ 2F80H      ; Long branch on Q=1
8BA5 A8           PLO R8         ; Put low register R8
8BA6 30 BC        BR  8BBCH      ; Short branch
8BA8 80           GLO R0         ; Get low register R0
8BA9 A9           PLO R9         ; Put low register R9
8BAA 2F           DEC RF         ; Decrement (RF)
8BAB 83           GLO R3         ; Get low register R3
8BAC AC           PLO RC         ; Put low register RC
8BAD 38 BC        SKP            ; Skip next byte
8BAF 0B           LDN RB         ; Load D with (RB)
8BB0 2F           DEC RF         ; Decrement (RF)
8BB1 80           GLO R0         ; Get low register R0
8BB2 A8           PLO R8         ; Put low register R8
8BB3 52           STR R2         ; Store D to (R2)
8BB4 2F           DEC RF         ; Decrement (RF)
8BB5 84           GLO R4         ; Get low register R4
8BB6 BD           PHI RD         ; Put high register RD
8BB7 09           LDN R9         ; Load D with (R9)
8BB8 02           LDN R2         ; Load D with (R2)
8BB9 2F           DEC RF         ; Decrement (RF)
8BBA 8E           GLO RE         ; Get low register RE
8BBB BC           PHI RC         ; Put high register RC
8BBC 84           GLO R4         ; Get low register R4
8BBD BD           PHI RD         ; Put high register RD
8BBE 09           LDN R9         ; Load D with (R9)
8BBF 03           LDN R3         ; Load D with (R3)
8BC0 2F           DEC RF         ; Decrement (RF)
8BC1 84           GLO R4         ; Get low register R4
8BC2 BE           PHI RE         ; Put high register RE
8BC3 09           LDN R9         ; Load D with (R9)
8BC4 05           LDN R5         ; Load D with (R5)
8BC5 2F           DEC RF         ; Decrement (RF)
8BC6 09           LDN R9         ; Load D with (R9)
8BC7 01           LDN R1         ; Load D with (R1)
8BC8 2F           DEC RF         ; Decrement (RF)
8BC9 80           GLO R0         ; Get low register R0
8BCA BE           PHI RE         ; Put high register RE
8BCB 84           GLO R4         ; Get low register R4
8BCC BD           PHI RD         ; Put high register RD
8BCD 09           LDN R9         ; Load D with (R9)
8BCE 06           LDN R6         ; Load D with (R6)
8BCF 2F           DEC RF         ; Decrement (RF)
8BD0 84           GLO R4         ; Get low register R4
8BD1 BC           PHI RC         ; Put high register RC
8BD2 09           LDN R9         ; Load D with (R9)
8BD3 05           LDN R5         ; Load D with (R5)
8BD4 2F           DEC RF         ; Decrement (RF)
8BD5 09           LDN R9         ; Load D with (R9)
8BD6 04           LDN R4         ; Load D with (R4)
8BD7 2F           DEC RF         ; Decrement (RF)
8BD8 0A           LDN RA         ; Load D with (RA)
8BD9 00           IDL            ; Idle or wait for interrupt or DMA request
8BDA 01           LDN R1         ; Load D with (R1)
8BDB 0A           LDN RA         ; Load D with (RA)
8BDC 7F           SMBI           ; Substract memory toh borrow, immediate
8BDD FF 66        SMI 66H        ; Substract D,DF to value
8BDF 84           GLO R4         ; Get low register R4
8BE0 AC           PLO RC         ; Put low register RC
8BE1 30 BC        BR  8BBCH      ; Short branch
8BE3 61           OUT 1          ; Output (R(X)); Increment R(X), N=001
8BE4 0B           LDN RB         ; Load D with (RB)
8BE5 38 92        SKP            ; Skip next byte
8BE7 90           GHI R0         ; Get high register R0
8BE8 B1           PHI R1         ; Put high register R1
8BE9 F8 ED        LDI EDH        ; Load D immediate
8BEB 0A           LDN RA         ; Load D with (RA)
8BEC D1           SEP R1         ; Set P=R1 as program counter
8BED F8 00        LDI 00H        ; Load D immediate
8BEF B0           PHI R0         ; Put high register R0
8BF0 C0 84 00     LBR 8400H      ; Long branch
8BF3 90           GHI R0         ; Get high register R0
8BF4 B1           PHI R1         ; Put high register R1
8BF5 F8 F9        LDI F9H        ; Load D immediate
8BF7 A1           PLO R1         ; Put low register R1
8BF8 D1           SEP R1         ; Set P=R1 as program counter
8BF9 F8 00        LDI 00H        ; Load D immediate
8BFB B0           PHI R0         ; Put high register R0
8BFC C0 84 03     LBR 8403H      ; Long branch                               subbraches to 84ED
8BFF 00           IDL            ; Idle or wait for interrupt or DMA request
