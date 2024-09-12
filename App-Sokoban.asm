;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               S o k o b a n                                @
;@                                                                            @
;@               (c) 2021 by Prodatron / SymbiosiS (Jörn Mika)                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Bugs

;Todo
;- player placed everywhere (double)
;- advanced features
;   - doors


;==============================================================================
;### CODE AREA ################################################################
;==============================================================================


;### PRGPRZ -> Application process
prgwin  db 0    ;main window ID

prgprz  call SySystem_HLPINI
        ld hl,(App_BegCode)
        ld de,App_BegCode
        dec h
        add hl,de           ;HL = CodeEnd = path
        push hl
        ld de,mappths
        ld bc,256
        ldir
        inc b
        pop hl
        ld de,levusrp+4
        ldir

        ld hl,mapfil1
        call mappth
        ld hl,mappths
        call maplod
        call mapscn

        call mapget
        call mapasc
        call mapgen
        call mapini

        ld de,wingam
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open startmenu edit window
        jp c,prgend                 ;memory full -> quit process
        ld (prgwin),a               ;window has been opened -> store ID

        call dspstp
        call dspbox

prgprz0 ld ix,(App_PrcID)           ;check for messages
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        ld a,(timflg)
        or a
        jr nz,prgprz2
        rst #08         ;no timer running -> sleep until message
        jr prgprz3
prgprz2 rst #18
        push ix
        rst #30
        call timdsp
        pop ix
prgprz3 db #dd:dec l
        jr nz,prgprz0
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
        cp MSR_DSK_WCLICK
        jr nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_KEY
        jr z,prgkey
        cp DSK_ACT_CLOSE
        jr z,prgend
        cp DSK_ACT_MENU
        jr z,prgprz1
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
prgprz1 ld hl,(App_MsgBuf+8)
        ld a,h
        or h
        jr z,prgprz0
        jp (hl)

;### PRGEND -> End program
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGINF -> open info window
prginf  ld a,(App_BnkNum)       ;** PUZZLE SOLVED
        ld hl,prgmsginf
        ld b,8*2+1+64+128
        ld de,wingam
        call SySystem_SYSWRN
        jp prgprz0

;### PRGHLP -> shows help
prghlp  call SySystem_HLPOPN
        jp prgprz0

;### PRGKEY -> key has been clicked
prgkey  xor a
        ld (manrep2),a
        ld a,(App_MsgBuf+4)
        call clcucs
        cp "E"
        jp z,slpkey
        cp "U"
        jp z,undund
        sub 136
        cp 4
        jp nc,prgprz0
        ld (movdir),a
        push af
        call undstp
        pop af
prgkey0 ld hl,(manxps)
        cp 1
        jp c,movup
        jp z,movdwn
        cp 3
        jp c,movlft
        jp movrgt


;==============================================================================
;### USER CONTROL #############################################################
;==============================================================================

;### Button Events
butres  call mapget
        call mapnew
        jp prgprz0

butpre  ld a,(mapact)
        sub 1
        jr nc,butnxt1
        ld a,(mapnum)
        dec a
        jr butnxt1

butnxt  ld a,(mapact)
        inc a
        ld hl,mapnum
        cp (hl)
        jr nz,butnxt1
        xor a
butnxt1 ld (mapact),a
        inc a
        call clcdez
        ld (txtlevval),hl
        ld e,wingam_dsp_num+7
        call dskdrw
        call mapget
        call mapnew
        jp prgprz0

;### Level loading
levusrp db "ysb",0
        ds 256

level1  ld hl,mapfil1
        jr level0
level2  ld hl,mapfil2
        jr level0
level3  ld hl,mapfil3
        jr level0
level4  ld hl,mapfil4
        jr level0
level5  ld hl,mapfil5
        jr level0
level6  ld hl,mapfil6
        jr level0
level7  ld hl,mapfil7
        jr level0
level8  ld hl,mapfil8
        jr level0
level9  ld hl,mapfil9
        jr level0

level0  call mappth
        ld hl,mappths
levelz  call maplod
        call c,maperr
        call mapscn
        call mapget
        call mapasc
        call mapgen
        call mapini
        xor a
        jp butnxt1
levelu  ld hl,levusrp
        ld a,(App_BnkNum)
        ld c,8
        ld ix,100
        ld iy,5000
        ld de,wingam
        call SySystem_SELOPN
        or a
        jp nz,prgprz0
        ld hl,levusrp+4
        jr levelz

;### Undo/Save/Load
boxmax  equ 16

undsiz  equ 10*boxmax+5+6+100+200   ;boxcontrols bitmap/x/y, boxlst, boxcnt, boxtdo, boxend, manxps, manyps, worker control bitmap/x/y, mapmem, mapbitmaps

undbuf  ds undsiz   ;buffer for last step
undsna  ds undsiz   ;buffer for stored snapshot

undstp  ld de,undbuf    ;new step -> store for an undo
        call undsto
        ld a,1
        ld (prgwinmen3+2),a
        ret

undund  ld hl,undbuf    ;make an undo
        jr undlod1

undsav  ld de,undsna    ;save snapshot
        call undsto
        ld a,1
        ld (prgwinmen3+2+24),a
        jp prgprz0

undlod  ld hl,undsna    ;load snapshot
undlod1 call undres
        jp prgprz0

undsto  ld hl,boxlst    ;makes a snapshot
        ld bc,4*boxmax+5
        ldir
        ld hl,wingam_man+4
        ld a,boxmax+1
undsto1 ld bc,6
        ldir
        ld bc,16-6
        add hl,bc
        dec a
        jr nz,undsto1
        ld hl,mapmem
        ld bc,map_xlen*map_ylen
        ldir
        ld hl,wingam_map+4
        ld a,map_xlen*map_ylen
undsto2 ld c,16
        ldi
        ldi
        add hl,bc
        dec a
        jr nz,undsto2
        ret

undres  ld de,boxlst    ;restores a snapshot und updates screen
        ld bc,4*boxmax+5
        ldir
        ld de,wingam_man+4
        ld a,boxmax+1
undres1 ld bc,6
        ldir
        ld bc,16-6
        ex de,hl
        add hl,bc
        ex de,hl
        dec a
        jr nz,undres1
        ld de,mapmem
        ld bc,map_xlen*map_ylen
        ldir
        ld de,wingam_map+4
        ld a,map_xlen*map_ylen
undres2 ld c,16
        ldi
        ldi
        ex de,hl
        add hl,bc
        ex de,hl
        dec a
        jr nz,undres2
        call dspbox
        jp mapnew1


;### Options
optslw  ld a,2
        ld hl,256*1+3
optslw1 ld (anistp),a
        ld a,l
        ld (prgwinmen4+2),a
        ld a,h
        ld (prgwinmen4+10),a
        jp prgprz0
optfst  ld a,16
        ld hl,256*3+1
        jr optslw1

;### SLPKEY -> exchange workers by keyboard
slpkey  ld hl,boxlst+2
        ld b,boxmax
        ld de,4
slpkey1 ld a,(hl)       ;find sleeping worker
        cp 7
        jr z,slpkey2
        add hl,de
        djnz slpkey1
        jp prgprz0
slpkey2 dec hl
        dec hl
        ld c,(hl)
        inc hl
        ld l,(hl)
        ld h,map_xlen
        call clcmu8
        ld a,c
        add l
        ld l,a
        jr slpclk

;### SLPCLK -> field has been clicked
;### Input      L=field number (0-99)
slpdat  dw 0
slpctr  dw 0
slpcti  db 0,0

slpclk  ld iy,(slpdat)
        ld l,(iy+0)
        ld h,(iy+1)
        push hl
        call mappos
        res 7,(ix+0)        ;remove box status from old sleeper position on map
        ld hl,(manxps)
        push hl
        call mappos
        set 7,(ix+0)        ;set box status for old worker position on map
        pop hl
        pop de
        ld (manxps),de      ;swap positions
        ld (iy+0),l
        ld (iy+1),h
        ld hl,(slpctr)
        ld de,6
        add hl,de
        ld de,wingam_man+6  ;de=worker control
        ld ixl,4
fldclk1 ld a,(de)           ;swap control coordinates
        ld b,(hl)
        ld (hl),a
        ld a,b
        ld (de),a
        inc hl
        inc de
        dec ixl
        jr nz,fldclk1
        ld de,(slpcti)
        call dskdrw
        ld e,wingam_man_num
        call dskdrw
        jp prgprz0


;==============================================================================
;### MOVE-ROUTINES ############################################################
;==============================================================================

movdir  db 0,0  ;0=up, 1=down, 2=left, 3=right
movdif  db 0,0  ;x difference, y difference (-1/0/1)

manbtm  dw 0    ;worker direction bitmap pointer

mannew  db 0,0  ;new worker position
boxold  db 0,0  ;old box    position
boxnew  db 0,0  ;new box    position

boxnum  db 0    ;moved box number
boxdat  dw 0    ;moved box data
boxctr  dw 0    ;moved box control record
boxcti  db 0    ;moved box control id
boxcol  db 0    ;moved box colour

boxupd  db 0    ;flag, if finished boxes changed

movact  db 1    ;flag, of moving is active
manrep  db 0    ;flag, if worker should move again
manrep2 db 0    ;same additional flag for box-stop
boxrep  db 0    ;flag, if box should move again

;### MOVUP/MOVDWN/MOVLFT/MOVRGT -> move worker
;### Input      L=current xpos, H=current ypos
movup   dec h:ld e,l:ld d,h:dec d:ld iy,movchkt+00:ld bc,256*255+000:jr movchk
movdwn  inc h:ld e,l:ld d,h:inc d:ld iy,movchkt+04:ld bc,256*001+000:jr movchk
movlft  dec l:ld e,l:ld d,h:dec e:ld iy,movchkt+08:ld bc,256*000+255:jr movchk
movrgt  inc l:ld e,l:ld d,h:inc e:ld iy,movchkt+12:ld bc,256*000+001:jr movchk


;### MOVCHK -> tries to move worker to a new position
;### Input      L,H=new worker position, E,D=new position of potential box, BC=move x/y differences, (IY+0/2)=worker bitmap pointer
movchkt dw gfx_man_uw, gfx_man_up
        dw gfx_man_dw, gfx_man_dp
        dw gfx_man_lw, gfx_man_lp
        dw gfx_man_rw, gfx_man_rp

movchk  ld a,(movact)
        or a
        jp z,prgprz0            ;worker is dead -> no movement
        ld (movdif),bc
        ld (mannew),hl
        ld (boxold),hl
        ld (boxnew),de
        ld (manbtm),iy
        xor a
        ld (boxupd),a
        call timgo

        ld hl,(boxold)
        call mappos
        ld (movchka+2),ix   ;store old box pos
        bit 7,(ix+0)
        jr nz,movchk4
        ld bc,fld_wen           ;** no box -> check, if worker can walk alone
        ld hl,(mannew)
        call fldchk
        jr nz,movchk2
movchk1 ld iy,(manbtm)          ;** worker can't move -> just show pushing worker
        ld l,(iy+2)
        ld h,(iy+3)
        ld (wingam_man+4),hl
        ld e,wingam_man_num
        call dskdrw
        jp movchke
movchk2 ld hl,(mannew)          ;** worker can move
        ld iy,(manbtm)
        ld de,(manxps)
        ld ix,wingam_man
        call aniini
movchk3 rst #30
        call aniwrk
        jr nz,movchk3

        call movchkg
        jp movchkf

        ld hl,manrep
        ld a,(hl)
        ld (hl),0
        or a
        jp z,movchke
        ld a,(movdir)       ;repeat move
        jp prgkey0

movchkg ld bc,fld_wet           ;** worker move finished
        ld hl,(mannew)      ;entered new field event
        call fldchk
        ld bc,fld_wlf       ;left old field event
        ld hl,(manxps)
        call fldchk
        ld hl,(mannew)
        ld (manxps),hl
        ld hl,(manstp)      ;update step display
        inc hl
        ld (manstp),hl
        jp dspstp

movchk4 ld hl,manrep2           ;** box
        ld a,(hl)
        ld (hl),0
        or a
        jr nz,movchk1       ;if move is repeated, and there is a box, don't move
        call movbox
        jr c,movchk1
        ld hl,(mannew)
        ld iy,(manbtm)
        inc iy:inc iy
        ld de,(manxps)
        ld ix,wingam_man
        call aniini
movchk7 rst #30
        call aniwrk
        push af
        call anibox
        pop af
        jr nz,movchk7
movchka ld ix,0                 ;ix=old box mapmem
movchkb ld iy,0                 ;iy=new box mapmem
        res 7,(ix+0)
        set 7,(iy+0)            ;update map fields
        ld bc,(boxcol)          ;c=box colour
        inc c:dec c
        ld de,0
        jr z,movchk9            ;wildcard -> skip destination check
        ld hl,boxend
        ld a,(ix+0)
        and 127
        cp c
        jr nz,movchk8
        dec (hl)                ;box WAS at destination -> decrease score
        ld a,1
        ld (boxupd),a
movchk8 ld a,(iy+0)
        and 127
        cp c
        jr nz,movchk9
        inc (hl)                ;box IS at destination -> increase score
        ld a,1
        ld (boxupd),a
        ld e,2
movchk9 ld a,(boxcol)
        call mapini0
        add hl,de
        ld ix,(boxctr)          ;ix=box control record
        ld a,(hl)
        ld (ix+4),a             ;set new box bitmap
        inc hl
        ld a,(hl)
        ld (ix+5),a
        ld de,(boxcti)          ;update box control
        call dskdrw

        ld bc,fld_bet           ;** box move finished
        ld hl,(boxnew)      ;entered new field event
        call fldchk
        ld a,(boxrep)
        or a
        jr z,movchkc

        xor a                   ;** move box again
        ld (boxrep),a
        ld hl,(boxnew)
        ld (boxold),hl
        ld a,(movdif+0):add l:ld l,a
        ld a,(movdif+1):add h:ld h,a
        ld (boxnew),hl
        ld hl,(movchkb+2)
        ld (movchka+2),hl
        call movbox
        jr c,movchkc        ;can't move again
        ld hl,(boxnew)
        ld iy,(manbtm)
        inc iy:inc iy
        ld de,(boxold)
        ld ix,(boxctr)
        call aniini
movchkd rst #30
        call aniwrk0
        push af
        call anibox
        pop af
        jr nz,movchkd
        jp movchka

movchkc call movchkg
        ld a,(boxupd)
        or a
        call nz,dspbox

movchkf ld hl,manrep
        ld a,(hl)
        ld (hl),0
        or a
        ld a,(movdir)       ;repeat worker move
        jp nz,prgkey0

movchke ld a,(boxend)
        ld hl,boxtdo
        cp (hl)
        jp nz,prgprz0
        ld a,(App_BnkNum)       ;** PUZZLE SOLVED
        ld hl,prgmsgwin
        ld b,8*2+1+64+128
        ld de,wingam
        call SySystem_SYSWRN
        jp butnxt

;### MOVBOX -> try to move a box
;### Input      (boxold),(boxnew)=old/new box positions
;### Output     CF=1 box can't be moved
;###            CF=0 box can be moved; (boxdat),(boxnum),(boxcti),(boxctr),(boxcol),(movchkb+2) set
movbox  call movbox0
        ret c
        ld (boxcol),a       ;store box colour
        ld hl,(boxnew)
        ld (ix+0),l         ;update box position
        ld (ix+1),h
        ld hl,boxcnt
        ld a,c
        neg
        add (hl)            ;a=box number
        ld (boxnum),a
        ld c,a
        add wingam_box_num
        ld (boxcti),a
        ld a,c
        add a:add a:add a:add a
        ld c,a
        ld b,0
        ld hl,wingam_box
        add hl,bc           ;hl=box control record
        ld (boxctr),hl
        or a
        ret
movbox0 call magchk     ;** box -> check, if box can be moved
        ret c               ;box hold by an magnet
        ld hl,(boxnew)
        push hl
        call mappos
        ld (movchkb+2),ix   ;store new box pos
        pop hl
        bit 7,(ix+0)
        scf
        ret nz              ;box blocked by another box
        ld bc,fld_ben
        call fldchk
        scf
        ret z               ;box not allowed to enter
        ld bc,fld_blv
        ld hl,(boxold)
        call fldchk
        scf
        ret z               ;box not allowed to leave
        ld de,(boxold)          ;** box can be moved
        call boxfnd         ;de=current box position -> find it
        ret c               ;not found -> should be an error
        ld (boxdat),ix
        ld c,a
        ld a,(ix+2)
        cp 7
        scf                 ;box is a sleeping worker, don't move
        ret z
        or a
        ret

;### DSPSTP -> updates step display
dspstp  ld hl,(manstp)
        ld iy,txtmovval
        ld e,wingam_dsp_num+4
        jr dspupd

;### DSPBOX -> updates box finished/remaining display
dspbox  ld a,(boxtdo)       ;remaining boxes
        ld hl,boxend
        sub (hl)
        ld l,a
        ld h,0
        ld iy,txtbxrval
        ld e,wingam_dsp_num+0
        call dspupd
        ld hl,(boxend)      ;finished boxes
        ld h,0
        ld iy,txtbxfval
        ld e,wingam_dsp_num+2
        jr dspupd

;### DSPUPD -> updates a display
;### Input      HL=value, IY=text, E=control
dspupd  push de
        push hl:pop ix
        ld e,5
        call clcnum
        pop de
        jp dskdrw

;### TIMDIF -> calculates time difference
;### Input      L,D,E = second1, minute1, hour1, A,B,C = second2, minute2, hour2
;### Output     A,B,C = time2-time1
;### Destroyed  F,E
timdif  sub l
        jr nc,timdif1
        add 60
timdif1 ld h,a
        ld a,b
        sbc d
        jr nc,timdif2
        add 60
timdif2 ld b,a
        ld a,c
        sbc e
        jr nc,timdif3
        add 24
timdif3 ld a,h
        ret

;### TIMGO -> starts timer, if not already running
timgo   ld hl,timflg
        bit 0,(hl)
        ret nz
        ld (hl),1
        rst #20:dw jmp_timget
        ld (timsta+0),a
        ld (timsta+1),bc
        ret

;### TIMDSP -> updates and displays timer
timflg  db 0    ;1=active timer

timsta  db 0,0,0
timsec  db 0
timmin  db 0

timdsp  rst #20:dw jmp_timget
        ld hl,(timsta+0)
        ld de,(timsta+1)
        call timdif
        ld hl,timsec
        cp (hl)
        ret z
        ld (hl),a
        inc hl
        ld (hl),b
;### TIMDSP0 -> shows timer
timdsp0 ld a,(timsec)
        call clcdez
        ld (txttimval+3),hl
        ld a,(timmin)
        call clcdez
        ld (txttimval+0),hl
        ld e,wingam_dsp_num+6
        jp dskdrw


;### ANIWRK -> does one worker animation
;### Output     ZF=1 -> animation finished
aniwrk  ld hl,(anidif)
        ld a,(wingam_man+6)     ;update worker position
        add l
        ld (wingam_man+6),a
        ld a,(wingam_man+8)
        add h
        ld (wingam_man+8),a
        ld e,wingam_man_num     ;plot worker
        call dskdrw
aniwrk0 ld hl,(anirxp)          ;plot restored area
        ld c,h
        ld h,0                  ;hl=xpos
        ld b,h                  ;bc=ypos
        ld de,(anirxl)
        ld ixl,e
        ld ixh,b                ;ix=xlen
        ld iyl,d
        ld iyh,b                ;iy=ylen
aniwrk1 ld e,0
        ld a,(prgwin)
        call SyDesktop_WINPIN
        ld bc,(anidif)
        ld hl,(anirxp)          ;update restoration area position
        ld a,l:add c:ld l,a
        ld a,h:add b:ld h,a
        ld (anirxp),hl
        ld hl,aninum
        dec (hl)
        ret

;### ANIBOX -> does one box animation
anibox  ld ix,(boxctr)      ;move box control
        ld hl,anidif
        ld a,(ix+6)
        add (hl)
        ld (ix+6),a
        inc hl
        ld a,(ix+8)
        add (hl)
        ld (ix+8),a
        ld de,(boxcti)      ;show box control
        jp dskdrw

;### ANIINI -> prepares a movement animation
;### Input      E/D=old position, L/H=new position, (IY)=worker bitmap pointer, IX=control record
;### Output     aninum,anirxp/yp/xl/yl initialised, worker bitmap set
anistp  db 2                    ;pixels moved per frame (configurable value)

anidif  db 0,0                  ;x difference, y difference (-1*steps/0/1*steps)
aninum  db 0                    ;number of animations

anirxp  db 0                    ;xstart of restored area
aniryp  db 0                    ;ystart
anirxl  db 0                    ;xlen of restored area
aniryl  db 0                    ;ylen

aniini  push hl
        ld l,d
        ld h,10
        ld c,e
        call clcmu8
        ld a,c
        add l
        add wingam_map_num
        ld (aniwrk1+1),a
        pop hl
        ld l,(iy+0)
        ld h,(iy+1)
        ld (wingam_man+4),hl
        ld a,(ix+6)
        ld c,a                  ;c=cur x
        ld a,(ix+8)
        ld b,a                  ;b=cur y
        ld hl,(anistp-1)
        ld l,til_xlen           ;move up/down -> h=ylen (pixel steps), l=xlen (tile width)
        ld e,til_ylen
        ld a,(movdir)
        cp 1
        jr z,aniini2            ;move down -> restoration y = bitmap
        jr nc,aniini1
        ld a,(anistp)           ;move up -> restoration y = bitmap bottom end - step size
        neg
        add e
        add b
        ld b,a
        jr aniini2
aniini1 ld l,h                  ;move left/right -> l=xlen (pixel steps), h=ylen (tile height)
        ld h,e
        ld e,til_xlen
        cp 3
        jr z,aniini2            ;move right -> restoration x = bitmap
        ld a,(anistp)           ;move left -> restoration x = bitmap right end - step size
        neg
        add e
        add c
        ld c,a
aniini2 ld (anirxp),bc          ;store restoration area coordinates
        ld (anirxl),hl
        ld a,(anistp)
        ld hl,(movdif)
aniini3 rra
        jr c,aniini4
        srl e
        sla l
        sla h
        jr aniini3
aniini4 ld a,e
        ld (aninum),a
        ld (anidif),hl
        ret

;### BOXFND -> find box
;### Input      DE=position
;### Output     CF=1 not found, CF=0 -> IX=data, A=count - number
boxfnd  ld ix,boxlst            ;** box can be moved
        ld a,(boxcnt)
        ld bc,4
boxfnd1 ld l,(ix+0)
        ld h,(ix+1)
        or a
        sbc hl,de
        scf
        ccf
        ret z
        add ix,bc
        dec a
        jr nz,boxfnd1
        scf
        ret                 


;==============================================================================
;### FIELD-ROUTINES ###########################################################
;==============================================================================

fld_ben equ  0
fld_blv equ  4
fld_bet equ  8
fld_wen equ 12
fld_wlf equ 16
fld_wet equ 20

;### BENxxx -> box tries to enter a field
;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data
;### Output     ZF=0 field can be entered

;### BLVxxx -> box tries to leave a field
;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data
;### Output     ZF=0 field can be leaved

;### BETxxx -> box has entered a field
;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data

;### WENxxx -> worker tries to enter a field
;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data
;### Output     ZF=0 field can be entered

;### WLFxxx -> worker has left a field
;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data
;###            mechanisms may have worked

;### WETxxx -> worker has entered a field
;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data

;### FLDCHK -> calls field check routine
;### Input      L=xpos, H=ypos, BC=function offset
;### Output     ZF=0 field can be entered (for enter check routines)
fldchk  push bc
        ld c,l
        ld l,10
        call clcmu8
        ld a,l
        add c
        ld (fldchk4+1),a
        add wingam_map_num      ;a=control id
        ld (fldchk2+1),a
        ld b,0
        add hl,bc
        ld bc,mapmem
        add hl,bc               ;hl=mapmem
        ld (fldchk1+1),hl
        ld a,(hl)
        res 7,a
        sub 8
        jr nc,fldchk0
        pop hl                  ;0-7 -> always free
        ret
fldchk0 ld l,a
        ld h,6*4
        call clcmu8
        ld bc,fldjmp
        add hl,bc
        pop bc
        add hl,bc
        ld e,(hl):inc hl
        ld d,(hl):inc hl        ;de=field routine address
        ld c,(hl):inc hl
        ld b,(hl)               ;bc=additional data
        push de
        push bc
fldchk4 ld hl,16*256
        call clcmu8
        ex de,hl
        ld ix,wingam_map
        add ix,de               ;ix=control record
        pop bc                  ;bc=additonal data
fldchk1 ld hl,0                 ;hl=mapmem
fldchk2 ld e,0                  ;e=control ID
        pop iy
        inc iyh:dec iyh         ;if it's a function address, iyh is always >=1
        jr z,fldchk3
        jp (iy)
fldchk3 dec iyl
        ret

;### Rails
blvrai
benrai  ld a,(movdir)
        inc a
        ld b,a
        ld a,128
benrai1 rlca
        djnz benrai1
        and c
        ret

;### Fragile floor
wlffrg  ld (hl),37      ;floor now has a hole
        ld hl,gfx_floor_hole
        ld (ix+4),l
        ld (ix+5),h
        jp dskdrw

bethol  ld (hl),0       ;floor is stuffed
        ld hl,gfx_floor_stuffed
        ld (ix+4),l
        ld (ix+5),h
        ld hl,(boxdat)  ;box is gone
        ld (hl),-1
        ld ix,(boxctr)
        ld (ix+6),-16
        ld (ix+7),0
        jp dskdrw

wethol  xor a           ;deactivate moving
        ld (movact),a
        ld hl,gfx_floor_dead
        ld (ix+4),l
        ld (ix+5),h
        ld hl,-16       ;hide worker control
        ld (wingam_man+6),hl
        jp dskdrw

;### Portal
betporm dw 0        ;old portal mapmem
betporn db 0,0      ;new portal mapmem
betporc db 0        ;old portal control id
betporp db 0,0      ;new portal position
betpord db 0,0      ;new movdif
betporr db 0        ;current movdir

;### Input      HL=mapmem, IX=control record, E=control ID, BC=additional data
wetpor  ld a,e
        ld (betporc),a
        call porfnd             ;hl=mapmem of destination, de=position
        ld (wetpor5+2),hl
        ld bc,(movdir)
        ld b,4
wetpor1 push bc
        push de
        call movpsd             ;DE=new pos, HL=xydif, BC=mapmem offset
wetpor5 ld ix,0
        add ix,bc
        bit 7,(ix+0)
        jr nz,wetpor3
        ex de,hl                ;** no box -> check, if worker can walk alone
        ld bc,fld_wen           
        call fldchk
        jr nz,wetpor4
wetpor2 pop de
        pop bc
        call movpsn
        djnz wetpor1
        ret                     ;no movement possible -> do nothing
wetpor3 ld (boxold),de          ;** box -> check, if box can move
        ld a,l
        add e
        ld l,a
        ld a,h
        add d
        ld h,a
        ld (boxnew),hl
        call movbox0
        jr c,wetpor2
wetpor4 pop hl                  ;** worker can move
        ld (mannew),hl          ;store new startpos -> will be copied to manxps
        pop bc
        ld a,c
        ld (movdir),a
        call mappos
        ld (wingam_man+6),de    ;set worker control to new position
        ld (wingam_man+8),hl
        ld de,(betporc)
        call dskdrw             ;show old portal
        ld a,1
        ld (manrep),a
        ret

betpor  ld (betporm),hl
        ld a,(movdir)
        ld (betporr),a
        ld a,e
        ld (betporc),a
        call porfnd     ;hl=mapmem of destination, DE=position
        ld (betporn),hl
        ret c           ;not found -> map error, do nothing
        ld (betporp),de
        ld bc,(movdir)
        ld b,4
betpor1 push bc
        ld a,c
        ld (movdir),a
        push de
        call movpsd
        ld (betpord),hl ;hl=movdif
        ld hl,(betporn)
        add hl,bc
        bit 7,(hl)
        jr nz,betpor2   ;field occupied by box
        ex de,hl
        ld bc,fld_ben
        push ix
        call fldchk
        pop ix
        jr nz,betpor3   ;field can be entered by box
betpor2 pop de
        pop bc
        call movpsn
        djnz betpor1
        ld a,(betporr)
        ld (movdir),a
        ret             ;no movement possible -> do nothing
betpor3 ld iy,(boxdat)
        ld hl,(betporp)
        ld (iy+0),l
        ld (iy+1),h
        call mappos
        ld iy,(boxctr)
        ld (iy+6),e     ;set control position to new portal
        ld (iy+7),d
        ld (iy+8),l
        ld (iy+9),h
        ld hl,(betporm)
        res 7,(hl)      ;remove box from old mapmem
        ld de,(betporc)
        call dskdrw     ;show old portal
        ld hl,(betporp)
        ld (boxnew),hl
        pop de
        pop bc          ;c=new move direction
        ld a,c
        ld (movdir),a
        ld hl,(betpord)
        ld (movdif),hl
        ld hl,(betporn)
        ld (movchkb+2),hl
        jr betice

;### Ice
wetice  ld a,1
        ld (manrep),a
        ld (manrep2),a
        ret
betice  ld a,1
        ld (boxrep),a
        ret

;### MOVPSD -> calculates new position with move direction
;### Input      DE=old pos, C=move direction (0=up, 1=down, 2=left, 3=right)
;### Output     DE=new pos, HL=xydif, BC=mapmem offset
;### Destroyed  AF
movpsd  ld a,c
        ld bc,-10
        ld hl,256*#ff+#00
        cp 1
        jr c,movpsd1
        ld bc,10
        ld h,#01
        jr z,movpsd1
        ld bc,1
        dec h
        inc l
        cp 3
        jr z,movpsd1
        dec bc:dec bc
        ld l,#ff
movpsd1 ld a,l:add e:ld e,a
        ld a,h:add d:ld d,a
        ret

;### MOVPSN -> calculates next move direction
;### Input      C=actual move direction
;### Output     C=next move direction
;### Destroyed  AF
movpsn  ld a,c
        sub 1
        ld c,3      ;0 up    -> 3 right
        ret c
        ld c,2      ;1 down  -> 2 left
        ret z
        dec c       ;3 right -> 1 down
        dec a
        ret nz
        dec c       ;2 left  -> 0 up
        ret

;### PORFND -> find partner portal
;### Input      HL=mapmem of portal
;### Output     CF=1 -> not found
;###            CF=0 -> HL=mapmem of partner, DE=position
porfnd  ld c,(hl)
        res 7,c
        ld de,mapmem        ;find partner portal
        ld b,map_xlen*map_ylen
porfnd1 ld a,(de)
        cp c
        jr nz,porfnd2
        push hl
        sbc hl,de
        pop hl
        jr nz,porfnd3
porfnd2 inc de
        djnz porfnd1
        scf
        ret
porfnd3 push de
        ex de,hl
        ld bc,mapmem
        or a
        sbc hl,bc
        ld c,10
        call clcdiv         ;l=ypos, h=xpos
        ld d,l
        ld e,h
        pop hl
        or a
        ret

;### Magnets
wenmagt dw gfx_magnet_left, gfx_magnet_up, gfx_magnet_right, gfx_magnet_down

wenmag  ld hl,mapmem            ;hl=mapmen
        ld ix,wingam_map        ;ix=control record
        ld c,wingam_map_num     ;c=control number
        ld b,map_xlen*map_ylen  ;b=mapsize (number of fields)
wenmag1 ld a,(hl)
        cp 24
        jr c,wenmag3            ;no magnet -> next
        cp 28
        jr nc,wenmag3           ;no magnet -> next
        inc a
        cp 28
        jr nz,wenmag2
        ld a,24
wenmag2 ld (hl),a           ;magnet rotated
        push hl

        sub 24
        add a
        ld l,a
        ld h,0
        ld de,wenmagt
        add hl,de
        ld a,(hl)
        ld (ix+4),a
        inc hl
        ld a,(hl)
        ld (ix+5),a         ;magnet new bitmap
        push ix
        push bc
        ld e,c
        call dskdrw         ;draw new magnet
        pop bc
        pop ix

        pop hl
wenmag3 inc hl              ;next
        ld de,16
        add ix,de           ;next control
        inc c
        djnz wenmag1
        xor a
        ret

;ix=mapmem -> cf=1 hold
magchk  ld hl,(boxold)
        call mappos
        ld a,24
        cp (ix+1)
        scf
        ret z
        inc a
        cp (ix+10)
        scf
        ret z
        inc a
        cp (ix-1)
        scf
        ret z
        inc a
        cp (ix-10)
        scf
        ret z
        or a
        ret


fldjmp  ;0=returns zf=1, 1=returns zf=0, >1=function
dw 0,0, 0,0, 0,0, 0,0, 0,0, 0,0
dw 0,0, 0,0, betice,0, 0,0, 0,0, wetice,0           ;ice
dw benrai,%1100, blvrai,%1100, 0,0, 0,0, 0,0, 0,0   ;rail left right
dw benrai,%0011, blvrai,%0011, 0,0, 0,0, 0,0, 0,0   ;rail up down
dw benrai,%0101, blvrai,%1010, 0,0, 0,0, 0,0, 0,0   ;rail edge up left
dw benrai,%1001, blvrai,%0110, 0,0, 0,0, 0,0, 0,0   ;rail edge up right
dw benrai,%0110, blvrai,%1001, 0,0, 0,0, 0,0, 0,0   ;rail edge down left
dw benrai,%1010, blvrai,%0101, 0,0, 0,0, 0,0, 0,0   ;rail edge down right
dw benrai,%1011, blvrai,%0111, 0,0, 0,0, 0,0, 0,0   ;rail tcross left
dw benrai,%0111, blvrai,%1011, 0,0, 0,0, 0,0, 0,0   ;rail tcross right
dw benrai,%1110, blvrai,%1101, 0,0, 0,0, 0,0, 0,0   ;rail tcross up
dw benrai,%1101, blvrai,%1110, 0,0, 0,0, 0,0, 0,0   ;rail tcross down
dw benrai,%1000, blvrai,%0100, 0,0, 0,0, 0,0, 0,0   ;rail end left
dw benrai,%0100, blvrai,%1000, 0,0, 0,0, 0,0, 0,0   ;rail end right
dw benrai,%0010, blvrai,%0001, 0,0, 0,0, 0,0, 0,0   ;rail end up
dw benrai,%0001, blvrai,%0010, 0,0, 0,0, 0,0, 0,0   ;rail end down
dw 1,0, 0,0, 0,0, 1,0, 0,0, 0,0                     ;magnets
dw 1,0, 0,0, 0,0, 1,0, 0,0, 0,0
dw 1,0, 0,0, 0,0, 1,0, 0,0, 0,0
dw 1,0, 0,0, 0,0, 1,0, 0,0, 0,0
dw 1,0, 0,0, 0,0, wenmag,0, 0,0, 0,0                ;magnet switch
dw 0,0, 0,0,betpor,0, 0,0, 0,0, wetpor,0            ;portal 1
dw 0,0, 0,0,betpor,0, 0,0, 0,0, wetpor,0            ;portal 2
dw 0,0, 0,0,betpor,0, 0,0, 0,0, wetpor,0            ;portal 3
dw 0,0, 0,0, 0,0, 0,0, 0,0, 0,0  ;doors
dw 0,0, 0,0, 0,0, 0,0, 0,0, 0,0
dw 0,0, 0,0, 0,0, 0,0, 0,0, 0,0  ;door switches
dw 0,0, 0,0, 0,0, 0,0, 0,0, 0,0
dw 0,0, 0,0, 0,0,      0,0, wlffrg,0, 0,0       ;floor fragile
dw 0,0, 0,0, bethol,0, 0,0, 0,0,      wethol,0  ;floor hole
dw 0,0, 0,0, 0,0, 0,0, 0,0, 0,0  ;outside
dw 1,0, 0,0, 0,0, 1,0, 0,0, 0,0  ;wall

;  ben  blv  bet  wen  wlf  wet


;==============================================================================
;### MAP-ROUTINES #############################################################
;==============================================================================

mapnum  db 0
mapact  db 0
maptab  ds 62*2

mapdef  ;default level
db "  #####",13,10
db "###   #",13,10
db "#a@A  #",13,10
db "### Ee#",13,10
db "#b##$ #",13,10
db "# # . ##",13,10
db "#B *CDd#",13,10
db "#   c  #",13,10
db "########",13,10
db 13,10
db "."  ;terminator

mapfil1 db "sok-mdrn.ysb",0
mapfil2 db "sok-clas.ysb",0
mapfil3 db "sok-room.ysb",0

mapfil4 db "sok-colr.ysb",0
mapfil5 db "sok-ice.ysb",0
mapfil6 db "sok-frag.ysb",0
mapfil7 db "sok-rail.ysb",0
mapfil8 db "sok-magn.ysb",0
mapfil9 db "sok-port.ysb",0
mapfila db "sok-door.ysb",0

map_xlen    equ 10
map_ylen    equ 10

til_xlen    equ 16
til_ylen    equ 16

;init
;11111ccc -> player (ccc=cpl[destination colour])
;1CCC???? -> box (CCC=box colour)
;????0ccc -> floor (ccc=destination colour)
;????1xxx -> ?/ice/rail1-6
;0??????? -> see below

;game
;1??????? xx -> box
;?0000ccc 0x -> floor (ccc=destination colour)
;?0001000 08 -> ?
;?0001001 09 -> ice
;?0001010 10 -> rail left-right
;?0001011 11 -> rail up-down
;?0001100 12 -> rail edge up-left
;?0001101 13 -> rail edge up-right
;?0001110 14 -> rail edge down-left
;?0001111 15 -> rail edge down-right
;?0010000 16 -> rail tcross left
;?0010001 17 -> rail tcross right
;?0010010 18 -> rail tcross up
;?0010011 19 -> rail tcross down
;?0010100 20 -> rail end left
;?0010101 21 -> rail end right
;?0010110 22 -> rail end up
;?0010111 23 -> rail end down
;00011000 24 -> magnet left
;00011001 25 -> magnet up
;00011010 26 -> magnet right
;00011011 27 -> magnet down
;00011100 28 -> magnet switch
;00011101 29 -> portal 1
;00011110 30 -> portal 2
;00011111 31 -> portal 3
;?0100000 32 -> door 1
;?0100001 33 -> door 2
;?0100010 34 -> door switch 1
;?0100011 35 -> door switch 2
;?0100100 36 -> floor fragile
;?0100101 37 -> floor hole
;00100110 38 -> outside
;00100111 39 -> wall


mapmem     ds map_xlen*map_ylen

maptra              ;translate table

db " ",0            ;floor
db "#",39           ;wall
db "=",38           ;outside
db "$",16*1+128     ;box alone
db "*",16*1+128+1   ;box on destination
db ".",1            ;destination

db "A",16*2+128     ;coloured boxes
db "B",16*3+128
db "C",16*4+128
db "D",16*5+128
db "E",16*6+128

db "R",16*7+128     ;resting worker

db "a",2            ;empty coloured destinations
db "b",3
db "c",4
db "d",5
db "e",6

db "?",16*0+128     ;wildcards on destinations
db "0",16*0+128+1
db "1",16*0+128+2
db "2",16*0+128+3
db "3",16*0+128+4
db "4",16*0+128+5
db "5",16*0+128+6
db "6",16*0+128+7

db "/", 9           ;ice
db "%", 9+128+16*1  ;ice with box

db "-",10           ;railroad left-right
db "|",11           ;railroad up-down
db "<",12           ;railroad edge up-left
db "^",13           ;railroad edge up-right
db "V",14           ;railroad edge down-left
db ">",15           ;railroad edge down-right
db "{",16           ;railroad tcross left
db "}",17           ;railroad tcross right
db "[",18           ;railroad tcross up
db "]",19           ;railroad tcross down
db "M",24           ;magnet left
db "m",26           ;magnet right
db "N",25           ;magnet up
db "n",27           ;magnet down
db "S",28           ;magnet switch
db "P",29           ;portal 1
db "p",30           ;portal 2
db "O",31           ;portal 3

db "'",36           ;floor fragile
db 34 ,37           ;floor hole

db "z",-8 ;=7dest    worker on coloured destinations
db "y",-7 ;=6dest
db "x",-6 ;=5dest
db "w",-5 ;=4dest
db "v",-4 ;=3dest
db "u",-3 ;=2dest
db "+",-2 ;=1dest
db "@",-1           ;worker alone (last in translation list!)


boxlst  ds 4*boxmax ;box position (x,y), colour, 0
boxcnt  db 0        ;number of boxes (including wildcards)
boxtdo  db 0        ;number of boxes to be solved
boxend  db 0        ;boxes at destination

manxps  db 0        ;xpos of worker
manyps  db 0        ;ypos of worker

manstp  dw 0        ;number of steps, which already has been done


;### MAPGET -> gets actual map data
;### Output     IX=map source
mapget  ld hl,(mapact)
        ld h,0
        add hl,hl
        ld bc,maptab
        add hl,bc
        ld c,(hl)
        inc hl
        ld b,(hl)
        push bc
        pop ix
        ret

;### MAPNEW -> sets up and initializes everything for a map
;### Input      IX=map source
mapnew  call mapasc     ;init all
        call mapgen
        call mapini
        call dspbox     ;show displays
        call dspstp
        xor a           ;init timer
        ld (timflg),a
        ld l,a:ld h,a
        ld (timsec),hl
        call timdsp0
mapnew1 ld hl,0         ;redraw playfield
        ld c,l
        ld b,h
        ld ix,160
        ld iy,160
        ld e,-1
        ld a,(prgwin)
        jp SyDesktop_WINPIN


;### MAPPTH -> build path for predefined map files
;### Input      HL=filename
;### Output     (mappths)=path
mappths ds 256

mappth  push hl
        ld hl,mappths
        ld e,l
        ld d,h              ;DE=HL
        ld b,255
mappth1 ld a,(hl)           ;search end of path
        or a
        jr z,mappth2
        inc hl
        djnz mappth1
        jr mappth4
        ld a,255
        sub b
        jr z,mappth4
        ld b,a
mappth2 ld (hl),0
        dec hl              ;search start of filename
        call mappth5
        jr z,mappth3
        djnz mappth2
        jr mappth4
mappth3 inc hl
        ex de,hl
mappth4 pop hl              ;replace application filename with map filename
mappth6 ld a,(hl)
        ldi
        or a
        jr nz,mappth6
        ret
mappth5 ld a,(hl)
        cp "/"
        ret z
        cp "\"
        ret z
        cp ":"
        ret

;### MAPERR -> shows an error, when fileloading didn't work
maperr  push hl
        ld a,(App_BnkNum)
        ld hl,prgmsgerr
        ld b,8*1+1+64
        ld de,wingam
        call SySystem_SYSWRN
        pop hl
        ret

;### MAPLOD -> loads an ascii file with maps
;### Input      HL=filepath and name
;### Output     HL=mapbuffer (default on error)
;###            CF=1 error while loading
maplodt db 13,10,13,10,"."

maplod  ld ix,(App_BnkNum-1)
        call SyFile_FILOPN
        jr c,maplod1
        push af
        ld bc,mapbuf_size
        ld hl,mapbuf
        ld de,(App_BnkNum)
        call SyFile_FILINP
        pop de
        push af
        push bc
        ld a,d
        call SyFile_FILCLO
        pop bc
        pop af
        jr c,maplod1
        ld hl,mapbuf
        push hl
        add hl,bc
        ex de,hl
        ld hl,maplodt
        ld bc,5
        ldir
        pop hl
        ret
maplod1 ld hl,mapdef
        ret

;### MAPSCN -> scans map ascii data for maps and builds table
;### Input      HL=ascii data ("."-terminated)
;### Output     maptab,mapnum,mapact updated
mapscn  xor a
        ld (mapnum),a
        ld (mapact),a
        ld ix,maptab
mapscn1 ld a,(hl)
        cp "."
        ret z
        push hl
        call mapsiz
        pop de
        jr c,mapscn1
        ld (ix+0),e
        ld (ix+1),d
        inc ix
        inc ix
        ex de,hl
        ld hl,mapnum
        inc (hl)
        ex de,hl
        jr mapscn1

;### MAPSIZ -> finds out map size and calculates align space
;### Input      HL=ascii data (empty line terminated)
;### Output     HL=points behind ascii data
;###            CF=0 -> BC=skip offset, E=xofs, D=yofs
;###            CF=1 -> map too large or empty
mapsiz  ld bc,0             ;c=xmax, b=ylen
mapsiz1 ld e,0              ;e=actual xlen
        ld a,(hl)
        cp 13
        jr z,mapsiz3
mapsiz2 inc e
        inc hl
        ld a,(hl)
        cp "\"
        jr nz,mapsiz5
        inc hl
        inc hl
        jr mapsiz2
mapsiz5 cp 13
        jr nz,mapsiz2
        inc hl
        inc hl
        inc b
        ld a,c
        cp e
        jr nc,mapsiz1
        ld c,e
        jr mapsiz1
mapsiz3 inc hl
        inc hl
        ld a,c
        or b
        scf
        ret z
        push hl
        ld a,map_ylen
        sub b
        jr c,mapsiz4
        rra
        push af
        ld l,a
        ld h,map_xlen
        call clcmu8
        ld a,map_xlen
        sub c
        pop de
        jr c,mapsiz4
        rra
        ld e,a
        add l
mapsiz4 pop hl
        ld c,a
        ld b,0
        ret

;### MAPASC -> loads a map from ascii data
;### Input      IX=ascii data
mapascn db 0    ;0=new line

mapasc  push ix:pop hl      ;align map
        call mapsiz
        push bc
        ld (mapasc0+2),de
        ld a,e
        ld (mapasc9+2),a
        xor a               ;init boxes and map
        ld (boxcnt),a
        ld (boxtdo),a
        ld (boxend),a
        ld l,a:ld h,a
        ld (manstp),hl
        ld bc,map_xlen*map_ylen-1
        ld hl,mapmem
        push hl
        ld de,mapmem+1
        ld (hl),38
        ldir
        pop hl
        pop bc
        add hl,bc
mapasc0 ld iy,0             ;iyl=x, iyh=y
mapasc1 ld (mapasc3+1),hl
        ex de,hl
mapasc2 ld a,(ix+0)         ;get ascii char
        inc ix
        cp "\"
        jr z,mapascd
        cp 13
        jr nz,mapasc4
mapasce inc ix              ;line feed
        xor a
        ld (mapascn),a
        ld a,(ix+0)
        cp 13
        ret z               ;next line empty -> finished
mapasc3 ld hl,0
        ld de,map_xlen
        add hl,de
mapasc9 ld iyl,0
        inc iyh
        jr mapasc1
mapasc4 ld hl,mapascn       ;skip spaces at the beginning
        inc (hl):dec (hl)
        jr nz,mapasca
        cp " "
        jr z,mapasc5
        ld (hl),1
mapasca call mapcod         ;find SINGLE code
        jr z,mapasc6
mapasc5 inc de              ;unknown -> ignore
        inc iyl
        jr mapasc2
mapascd ld a,(ix+0)         ;find DOUBLE code
        inc ix
        cp 13
        jr z,mapasce
        call mapcod
        ld b,a
        ld a,(ix+0)
        inc ix
        cp 13
        jr z,mapasce
        call mapcod
        or b
mapasc6 cp -8               ;write code to map
        jr nc,mapasc7
        ld c,a
        bit 7,a
        jr z,mapascc
        and #8f
mapascc ld (de),a
        ld a,c
        bit 7,a
        jr z,mapasc5
        inc de
        ld hl,(boxcnt)      ;box found -> save position and colour
        ld h,0
        add hl,hl
        add hl,hl
        ld bc,boxlst
        add hl,bc
        ld c,iyl:ld (hl),c:inc hl
        ld c,iyh:ld (hl),c:inc hl
        ld c,a
        and #70
        rra:rra:rra:rra     ;a=box colour
        ld (hl),a
        jr z,mapascb        ;wild card -> no need to solve it
        cp 7
        jr z,mapascb        ;sleeping worker -> not a moveable box
        ld hl,boxtdo
        inc (hl)
mapascb inc iyl
        ld hl,boxcnt
        inc (hl)
        ld b,a
        ld a,c
        and #0f
        jp z,mapasc2
        cp b
        jp nz,mapasc2
        ld hl,boxend
        inc (hl)
        jp mapasc2
mapasc7 ld (manxps),iy      ;place worker
        neg
        dec a
        ld (de),a
        jr mapasc5

;### MAPCOD -> find map code
;### Input      A=ascii
;### Output     ZF=0 not found, A=0
;###            ZF=1 -> A=code
;### Destroyed  C,HL
mapcod  ld hl,maptra
mapcod1 cp (hl)
        inc hl
        jr z,mapcod2
        ld c,(hl)
        inc hl
        inc c
        jr nz,mapcod1
        ld a,0
        dec c
        ret
mapcod2 ld a,(hl)
        ret

;### MAPGEN -> generates map controls
mapgent dw gfx_floor,       gfx_des_0,      gfx_des_1,      gfx_des_2,      gfx_des_3,      gfx_des_4,      gfx_des_5,      0               ;00-07
        dw 0,               gfx_ice,        gfx_rail_lr,    gfx_rail_ud,    gfx_rail_elu,   gfx_rail_eru,   gfx_rail_eld,   gfx_rail_erd    ;08-15
        dw gfx_rail_tl,     gfx_rail_tr,    gfx_rail_tu,    gfx_rail_td,    0,              0,              0,              0               ;16-23
        dw gfx_magnet_left, gfx_magnet_up, gfx_magnet_right,gfx_magnet_down,gfx_magnet_switch,gfx_portal_1, gfx_portal_2,   gfx_portal_3    ;24-31
        dw 0,               0,              0,              0,            gfx_floor_fragile,gfx_floor_hole, gfx_black,      gfx_wall        ;32-39

mapgen  ld de,mapmem
        ld ix,wingam_map
        ld iyl,map_xlen*map_ylen
        ld b,0
mapgen1 ld a,(de)
        inc de
        and 127
        call mapgen0
        ld c,16
        add ix,bc
        dec iyl
        jr nz,mapgen1
        ret
mapgen0 add a
        ld c,a
        ld hl,mapgent
        add hl,bc
        ld a,(hl)
        ld (ix+4),a
        inc hl
        ld a,(hl)
        ld (ix+5),a
        ret

;### MAPINI -> initialises all worker and box sprite positions
mapinit dw gfx_box_00,gfx_box_00
        dw gfx_box_0u,gfx_box_0s
        dw gfx_box_1u,gfx_box_1s
        dw gfx_box_2u,gfx_box_2s
        dw gfx_box_3u,gfx_box_3s
        dw gfx_box_4u,gfx_box_4s
        dw gfx_box_5u,gfx_box_5s
        dw gfx_man_sleep,gfx_man_sleep

mapinic db 0    ;box control id for sleeper

mapini  xor a
        ld (prgwinmen3+2),a
        ld (prgwinmen3+2+24),a
        inc a
        ld (movact),a

        ld hl,gfx_man_lw        ;init worker
        ld (wingam_man+4),hl
        ld hl,(manxps)
        call mappos
        ld (wingam_man+6),de
        ld (wingam_man+8),hl
        ld hl,wingam_box+2      ;init boxes
        ld de,16
        ld b,boxmax
mapini3 ld (hl),64
        add hl,de
        djnz mapini3
        ld ix,boxlst
        ld iy,wingam_box
        ld a,wingam_box_num
        ld (mapinic),a
        ld a,(boxcnt)
mapini1 push af
        ld a,(ix+2)
        call mapini0
        push ix
        push hl
        ld l,(ix+0)
        ld h,(ix+1)
        ld (iy+0),0
        ld (iy+1),0
        ld a,(ix+2)
        cp 7
        jr nz,mapini4
        ld (slpctr),iy
        ld (slpdat),ix
        ld de,(mapinic)
        ld (slpcti),de
        ld de,slpclk
        ld (iy+0),e
        ld (iy+1),d
mapini4 push af
        call mappos
        ld (iy+6),e
        ld (iy+7),d
        ld (iy+8),l
        ld (iy+9),h
        pop bc                  ;c=box colour
        ld a,(ix+0)
        and 127                 ;a=destination colour
        cp b
        ld bc,0
        jr nz,mapini2
        ld c,2
mapini2 pop hl
        add hl,bc               ;(hl)=bitmap pointer
        ld a,(hl)
        ld (iy+4),a
        inc hl
        ld a,(hl)
        ld (iy+5),a
        ld (iy+2),10
        pop ix
        ld c,4
        add ix,bc
        ld c,16
        add iy,bc
        pop af
        ld hl,mapinic
        inc (hl)
        dec a
        jr nz,mapini1
        ret
mapini0 add a:add a             ;a=colour -> (hl)=box bitmap pointers
        ld c,a
        ld b,0
        ld hl,mapinit
        add hl,bc               ;hl=bitmap pointer tab
        ret

;### MAPPOS -> calculates position on bitmap and in mapmemory
;### Input      L=xpos, H=ypos
;### Output     DE=bitmap xpos, HL=bitmap ypos, IX=mapmemory
;### Destroyed  F,BC
mappos  push hl
        ld c,l
        ld l,map_xlen
        call clcmu8
        ex de,hl
        ld ix,mapmem
        add ix,de
        ld b,0
        add ix,bc       ;ix=mapmem + xpos + ypos*map_xlen
        pop de
        push de
        ld l,til_xlen
        ld h,e
        call clcmu8
        ex (sp),hl      ;(sp)=xpos * til_xlen
        ld l,til_ylen
        call clcmu8
        pop de
        ret


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

;### CLCMU8 -> 8bit unsigned multiplication
;### Input      L,H=values
;### Output     HL=L*H
;### Destroyed  F,B,DE
clcmu8  ld e,l
        ld l,0
        ld d,0
        ld b,8
clcmu81 add hl,hl
        jr nc,clcmu82
        add hl,de
clcmu82 djnz clcmu81
        ret

;### CLCDIV -> division
;### Output     L=HL/C, H=HL mod C
;### Destroyed  AF,BC
clcdiv  ld b,8
clcdiv1 add hl,hl       ;3
        ld a,h          ;1
        sub c           ;1
        jr c,clcdiv2    ;2/3
        ld h,a          ;1
        inc l           ;1 9/8
clcdiv2 djnz clcdiv1
        ret

;### CLCDEZ -> Calculates 2digit decimal string
;### Input      A=value
;### Output     L=10er digit, H=1er digit
;### Veraendert AF
clcdez  ld l,"0"
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ret

;### CLCNUM -> Converts 16bit Number into sting (0-terminated)
;### Input      IX=value, IY=address, E=max numbers of digits
;### Output     (IY)=last digit
;### Destroyed  AF,BC,DE,HL,IX,IY
clcnumt dw 1,10,100,1000,10000
clcnum  ld d,0
        ld b,e
        dec e
        push ix
        pop hl
        ld ix,clcnumt
        add ix,de
        add ix,de               ;IX=first divider
        dec b
        jr z,clcnum4
        ld c,0
clcnum1 ld e,(ix)
        ld d,(ix+1)
        dec ix
        dec ix
        ld a,"0"
        or a
clcnum2 sbc hl,de
        jr c,clcnum5
        inc c
        inc a
        jr clcnum2
clcnum5 add hl,de
        inc c
        dec c
        jr z,clcnum3
        ld (iy+0),a
        inc iy
clcnum3 djnz clcnum1
clcnum4 ld a,"0"
        add l
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CLCUCS -> upper case
;### Input      A=char
;### Output     A=ucase(A)
;### Destroyed  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret

;### DSKDRW -> redraws one or more controls
;### Input      E,D=controls
dskdrw  ld a,(prgwin)
        jp SyDesktop_WINDIN


;### map buffer
mapbuf_size equ 3900
mapbuf  db 0    ;*** LAST BYTE in code area ***


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #EE,#EE,#FF,#83,#32,#22,#22,#22,#22,#22,#23,#38
db #FF,#FF,#EF,#83,#23,#33,#33,#33,#33,#33,#32,#38
db #FF,#FF,#F8,#88,#33,#33,#33,#33,#33,#33,#33,#88
db #88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88
db #99,#99,#99,#98,#88,#88,#81,#11,#18,#88,#88,#88
db #99,#99,#99,#19,#81,#88,#17,#77,#71,#88,#88,#88
db #11,#11,#11,#99,#88,#11,#77,#77,#77,#18,#88,#88
db #99,#99,#9A,#99,#88,#81,#11,#11,#77,#71,#88,#88
db #99,#99,#9A,#99,#88,#88,#EE,#EE,#17,#71,#88,#88
db #A9,#99,#9A,#99,#88,#8E,#E1,#EE,#E1,#71,#88,#88
db #AA,#99,#9A,#99,#E8,#8E,#E1,#EE,#E1,#71,#88,#88
db #AA,#A9,#9A,#99,#EE,#88,#EE,#EE,#11,#18,#88,#88
db #AA,#AA,#9A,#99,#EE,#17,#77,#77,#77,#71,#88,#88
db #AA,#A9,#9A,#99,#8E,#17,#77,#77,#77,#77,#18,#88
db #AA,#99,#9A,#99,#88,#88,#81,#77,#77,#77,#71,#88
db #A9,#99,#9A,#99,#88,#88,#81,#77,#77,#77,#71,#88
db #99,#99,#9A,#99,#88,#88,#81,#77,#77,#77,#71,#88
db #AA,#AA,#AA,#99,#88,#88,#88,#17,#71,#11,#1E,#18
db #99,#99,#99,#A9,#88,#88,#88,#EE,#18,#88,#EE,#18
db #99,#99,#99,#98,#88,#88,#81,#11,#88,#81,#11,#88
db #88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88
db #FF,#FF,#F8,#88,#33,#33,#33,#33,#33,#33,#33,#88
db #FF,#FF,#1F,#83,#13,#33,#33,#33,#33,#33,#31,#38
db #11,#11,#FF,#83,#31,#11,#11,#11,#11,#11,#13,#38

;### STRINGS ##################################################################

wintittxt   db "Sokoban",0


txtbxftit   db "Finished",0
txtbxfval   db "00000",0
txtbxrtit   db "Remaining",0
txtbxrval   db "00000",0
txtmovtit   db "Steps",0
txtmovval   db "00000",0
txttimtit   db "Time",0
txttimval   db "00:00",0
txtlevval   db "01",0

txtbutres   db "Restart",0
txtbutpre   db "<<",0
txtbutnxt   db ">>",0

prgwinmentx1 db "File",0
prgwinmentx2 db "Puzzles",0
prgwinmentx3 db "Moves",0
prgwinmentx4 db "Options",0
prgwinmentx5 db "?",0

prgwinmen1tx0 db "Quit",0

prgwinmen2tx1 db "Modern",0
prgwinmen2tx2 db "Classic",0
prgwinmen2tx3 db "Two Rooms",0
prgwinmen2tx4 db "Colours",0
prgwinmen2tx5 db "Ice",0
prgwinmen2tx6 db "Fragile floor",0
prgwinmen2tx7 db "Rails",0
prgwinmen2tx8 db "Magnets",0
prgwinmen2tx9 db "Portals",0
prgwinmen2txa db "Doors",0
prgwinmen2txu db "Load puzzle pack...",0

prgwinmen3tx1 db "Undo",0
prgwinmen3tx2 db "Save snapshot",0
prgwinmen3tx3 db "Restore snapshot",0

prgwinmen4tx1 db "Walk smooth",0
prgwinmen4tx2 db "Walk quick",0

prgwinmen5tx1 db "Index",0
prgwinmen5tx2 db "About Sokoban",0

;### you win
prgmsgwon1  db "Congratulation!",0
prgmsgwon2  db " Puzzle solved.",0
prgmsgwon3  db " Let's go for the next one!",0

;### info
prgmsginf1 db "Sokoban for SymbOS",0
prgmsginf2 db " Version 1.0 (Build 211014pdt)",0
prgmsginf3 db " Copyright <c> 2021 SymbiosiS",0

;### load error
prgmsgerr1  db "Error while loading",0
prgmsgerr2  db " The selected file couldn't",0
prgmsgerr3  db " be loaded",0


;### BITMAPS ##################################################################

gfx_logo db 32,64,45:dw $+7:dw $+4,32*45:db 5
db #88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#82,#22,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#65,#55,#55,#55,#55,#58,#AA,#AA,#99,#99,#9A,#AA,#A9,#98,#22,#22,#33,#82,#22,#33,#33,#38,#66,#66,#55,#55,#56,#66,#65,#58
db #86,#66,#65,#55,#55,#55,#55,#58,#AA,#AA,#99,#99,#9A,#AA,#A9,#98,#22,#22,#33,#82,#22,#33,#33,#38,#66,#66,#55,#55,#56,#66,#65,#58
db #86,#66,#65,#58,#88,#88,#88,#88,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#33,#82,#22,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #86,#66,#65,#58,#88,#88,#88,#88,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#22,#22,#22,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#22,#23,#33,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#22,#23,#33,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#22,#23,#38,#88,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #85,#55,#55,#55,#56,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#22,#22,#22,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #85,#55,#55,#55,#56,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#33,#32,#22,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #88,#88,#88,#88,#86,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#33,#32,#22,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #88,#88,#88,#88,#86,#66,#65,#58,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#33,#82,#22,#33,#88,#88,#66,#66,#55,#88,#86,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#82,#22,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#83,#32,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#83,#32,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #86,#66,#66,#66,#66,#66,#65,#58,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#66,#66,#66,#66,#65,#58
db #85,#55,#55,#55,#55,#55,#55,#58,#99,#99,#99,#99,#99,#99,#99,#98,#33,#33,#33,#88,#83,#33,#33,#38,#55,#55,#55,#55,#55,#55,#55,#58
db #85,#55,#55,#55,#55,#55,#55,#58,#99,#99,#99,#99,#99,#99,#99,#98,#33,#33,#33,#88,#83,#33,#33,#38,#55,#55,#55,#55,#55,#55,#55,#58
db #88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#88,#88,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#58,#88,#88,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#88,#88,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#58,#88,#88,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#88,#88,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#58,#88,#88,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#88,#88,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#58,#88,#88,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#99,#99,#9A,#AA,#A9,#98,#22,#22,#33,#33,#32,#22,#23,#38,#66,#66,#66,#66,#65,#58,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#99,#99,#9A,#AA,#A9,#98,#22,#22,#33,#33,#32,#22,#23,#38,#66,#66,#66,#66,#65,#58,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#66,#66,#65,#58,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#66,#66,#65,#58,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#55,#66,#65,#58,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#55,#66,#65,#58,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#58,#66,#65,#58,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#AA,#A9,#98,#22,#22,#22,#22,#22,#22,#23,#38,#66,#66,#65,#58,#66,#66,#66,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#99,#99,#9A,#AA,#A9,#98,#22,#22,#33,#33,#32,#22,#23,#38,#66,#66,#65,#58,#55,#66,#66,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#99,#99,#9A,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#65,#58,#55,#66,#66,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#99,#88,#8A,#AA,#A9,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#65,#58,#88,#66,#66,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#99,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#65,#58,#88,#55,#55,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#99,#98,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#65,#58,#88,#55,#55,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#88,#88,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#65,#58,#88,#88,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#AA,#AA,#AA,#AA,#AA,#99,#88,#88,#22,#22,#33,#88,#82,#22,#23,#38,#66,#66,#65,#58,#88,#88,#88,#66,#66,#55,#88,#88,#88
db #88,#88,#88,#99,#99,#99,#99,#99,#99,#88,#88,#33,#33,#33,#88,#83,#33,#33,#38,#55,#55,#55,#58,#88,#88,#88,#55,#55,#55,#88,#88,#88
db #88,#88,#88,#99,#99,#99,#99,#99,#99,#88,#88,#33,#33,#33,#88,#83,#33,#33,#38,#55,#55,#55,#58,#88,#88,#88,#55,#55,#55,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88,#88

;### worker (left/right/up/down, walk/push)

gfx_man_lw db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#11,#11,#88,#88,#88, #88,#18,#81,#77,#77,#18,#88,#88, #88,#81,#17,#77,#77,#71,#88,#88, #88,#88,#11,#11,#17,#77,#18,#88
db #88,#88,#8E,#EE,#E1,#77,#18,#88, #88,#88,#EE,#1E,#EE,#17,#18,#88, #88,#88,#EE,#1E,#EE,#17,#18,#88, #88,#88,#8E,#EE,#E1,#11,#88,#88
db #88,#88,#81,#71,#17,#18,#88,#88, #88,#88,#17,#81,#18,#71,#88,#88, #88,#81,#77,#77,#77,#77,#18,#88, #88,#EE,#77,#77,#77,#77,#EE,#88
db #88,#EE,#77,#77,#77,#77,#EE,#88, #88,#88,#87,#71,#17,#78,#88,#88, #88,#88,#8E,#E1,#8E,#E1,#88,#88, #88,#88,#11,#18,#11,#18,#88,#88

gfx_man_rw db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#11,#11,#88,#88,#88, #88,#88,#81,#77,#77,#18,#81,#88, #88,#88,#17,#77,#77,#71,#18,#88, #88,#81,#77,#71,#11,#11,#88,#88
db #88,#81,#77,#1E,#EE,#E8,#88,#88, #88,#81,#71,#EE,#E1,#EE,#88,#88, #88,#81,#71,#EE,#E1,#EE,#88,#88, #88,#88,#11,#1E,#EE,#E8,#88,#88
db #88,#88,#81,#71,#17,#18,#88,#88, #88,#88,#17,#81,#18,#71,#88,#88, #88,#81,#77,#77,#77,#77,#18,#88, #88,#EE,#77,#77,#77,#77,#EE,#88
db #88,#EE,#77,#77,#77,#77,#EE,#88, #88,#88,#87,#71,#17,#78,#88,#88, #88,#88,#1E,#E8,#1E,#E8,#88,#88, #88,#88,#81,#11,#81,#11,#88,#88

gfx_man_uw db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#8E,#E8,#88,#88,#88, #88,#88,#88,#11,#11,#88,#88,#88, #88,#88,#81,#77,#77,#18,#88,#88
db #88,#88,#11,#11,#11,#11,#88,#88, #88,#88,#17,#77,#77,#71,#8E,#E8, #88,#88,#17,#77,#77,#71,#71,#E8, #88,#81,#71,#77,#77,#17,#71,#88
db #88,#17,#77,#11,#11,#77,#18,#88, #8E,#17,#77,#77,#77,#77,#88,#88, #8E,#E8,#77,#77,#77,#17,#88,#88, #88,#81,#77,#11,#11,#77,#18,#88
db #88,#88,#11,#88,#87,#77,#18,#88, #88,#88,#88,#88,#88,#11,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_man_dw db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#11,#88,#88,#88,#88,#88, #88,#81,#77,#78,#88,#11,#88,#88
db #88,#81,#77,#11,#11,#77,#18,#88, #88,#88,#71,#77,#77,#77,#8E,#E8, #88,#88,#77,#77,#77,#77,#71,#E8, #88,#81,#77,#11,#11,#77,#71,#88
db #88,#17,#71,#77,#77,#17,#18,#88, #8E,#17,#17,#77,#77,#71,#88,#88, #8E,#E8,#17,#77,#77,#71,#88,#88, #88,#88,#11,#11,#11,#11,#88,#88
db #88,#88,#81,#77,#77,#18,#88,#88, #88,#88,#88,#11,#11,#88,#88,#88, #88,#88,#88,#8E,#E8,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_man_lp db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#81,#11,#18,#88,#88,#88, #81,#88,#17,#77,#71,#88,#88,#88, #88,#11,#77,#77,#77,#18,#88,#88, #88,#81,#11,#11,#77,#71,#88,#88
db #88,#88,#EE,#EE,#17,#71,#88,#88, #88,#8E,#E1,#EE,#E1,#71,#88,#88, #E8,#8E,#E1,#EE,#E1,#71,#88,#88, #EE,#88,#EE,#EE,#11,#18,#88,#88
db #EE,#17,#77,#77,#77,#71,#88,#88, #8E,#17,#77,#77,#77,#77,#18,#88, #88,#88,#81,#77,#77,#77,#71,#88, #88,#88,#81,#77,#77,#77,#71,#88
db #88,#88,#81,#77,#77,#77,#71,#88, #88,#88,#88,#17,#71,#11,#1E,#18, #88,#88,#88,#EE,#18,#88,#EE,#18, #88,#88,#81,#11,#88,#81,#11,#88

gfx_man_rp db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#81,#11,#18,#88,#88, #88,#88,#88,#17,#77,#71,#88,#18, #88,#88,#81,#77,#77,#77,#11,#88, #88,#88,#17,#77,#11,#11,#18,#88
db #88,#88,#17,#71,#EE,#EE,#88,#88, #88,#88,#17,#1E,#EE,#1E,#E8,#88, #88,#88,#17,#1E,#EE,#1E,#E8,#8E, #88,#88,#81,#11,#EE,#EE,#88,#EE
db #88,#88,#81,#77,#77,#77,#71,#EE, #88,#88,#17,#77,#77,#77,#71,#E8, #88,#88,#17,#77,#77,#18,#88,#88, #88,#88,#17,#77,#77,#18,#88,#88
db #88,#88,#17,#77,#77,#18,#88,#88, #88,#88,#17,#11,#77,#88,#88,#88, #88,#81,#EE,#88,#1E,#E8,#88,#88, #88,#88,#11,#18,#81,#11,#88,#88

gfx_man_up db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#EE,#8E,#88,#88,#E8,#EE,#88, #88,#8E,#E8,#8E,#E8,#8E,#E8,#88, #88,#81,#18,#11,#11,#81,#18,#88, #88,#87,#71,#77,#77,#17,#78,#88
db #88,#87,#11,#11,#11,#11,#78,#88, #88,#87,#17,#77,#77,#71,#78,#88, #88,#87,#17,#77,#77,#71,#78,#88, #88,#88,#71,#77,#77,#17,#88,#88
db #88,#88,#77,#11,#11,#78,#88,#88, #88,#87,#77,#77,#77,#78,#88,#88, #88,#87,#17,#77,#77,#71,#88,#88, #88,#17,#71,#11,#17,#71,#88,#88
db #88,#17,#77,#88,#81,#18,#88,#88, #88,#81,#18,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_man_dp db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#81,#18,#88, #88,#88,#81,#18,#88,#77,#71,#88
db #88,#88,#17,#71,#11,#17,#71,#88, #88,#88,#17,#77,#77,#71,#78,#88, #88,#88,#87,#77,#77,#77,#78,#88, #88,#88,#87,#11,#11,#77,#88,#88
db #88,#88,#71,#77,#77,#17,#88,#88, #88,#87,#17,#77,#77,#71,#78,#88, #88,#87,#17,#77,#77,#71,#78,#88, #88,#87,#11,#11,#11,#11,#78,#88
db #88,#87,#71,#77,#77,#17,#78,#88, #88,#81,#18,#11,#11,#81,#18,#88, #88,#8E,#E8,#8E,#E8,#8E,#E8,#88, #88,#EE,#8E,#88,#88,#E8,#EE,#88

gfx_man_sleep db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #81,#88,#88,#88,#88,#88,#88,#88, #18,#18,#88,#88,#88,#88,#88,#88
db #81,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#81,#88,#81,#81,#11,#18,#88, #88,#18,#18,#88,#11,#FF,#F1,#88
db #88,#81,#88,#88,#EE,#1F,#FF,#18, #88,#88,#88,#8E,#EE,#E1,#FF,#18, #88,#88,#88,#88,#EE,#EE,#1F,#18, #88,#88,#88,#88,#FE,#EE,#1F,#18
db #18,#88,#88,#8F,#FF,#FF,#F1,#88, #1E,#88,#88,#11,#11,#FF,#11,#88, #1E,#EF,#FF,#FF,#FF,#1F,#1E,#E8, #81,#EF,#FF,#FF,#FF,#F1,#8E,#E8



;### wild card box
gfx_box_00   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #8D,#DD,#DD,#DD,#DD,#DD,#DD,#D8, #D1,#DD,#DD,#DD,#DD,#DD,#DD,#1D, #DD,#11,#11,#11,#11,#11,#11,#DD, #DD,#1D,#DD,#DD,#DD,#DD,#D7,#DD
db #DD,#1D,#DD,#88,#88,#8D,#D7,#DD, #DD,#1D,#D8,#88,#DD,#88,#D7,#DD, #DD,#1D,#DD,#DD,#DD,#88,#D7,#DD, #DD,#1D,#DD,#DD,#88,#8D,#D7,#DD
db #DD,#1D,#DD,#D8,#8D,#DD,#D7,#DD, #DD,#1D,#DD,#D8,#8D,#DD,#D7,#DD, #DD,#1D,#DD,#DD,#DD,#DD,#D7,#DD, #DD,#1D,#DD,#D8,#8D,#DD,#D7,#DD
db #DD,#1D,#DD,#DD,#DD,#DD,#D7,#DD, #DD,#77,#77,#77,#77,#77,#77,#DD, #D7,#DD,#DD,#DD,#DD,#DD,#DD,#7D, #8D,#DD,#DD,#DD,#DD,#DD,#DD,#D8

;### boxes (0/1/2, unsorted/sorted)

gfx_box_0u   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #8E,#EE,#EE,#EE,#EE,#EE,#EE,#E8, #E1,#EE,#EE,#EE,#EE,#EE,#EE,#1E, #EE,#11,#11,#11,#11,#11,#11,#EE, #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE
db #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE, #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE, #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE, #EE,#1E,#EE,#E8,#8E,#EE,#E3,#EE
db #EE,#1E,#EE,#E8,#8E,#EE,#E3,#EE, #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE, #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE, #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE
db #EE,#1E,#EE,#EE,#EE,#EE,#E3,#EE, #EE,#33,#33,#33,#33,#33,#33,#EE, #E3,#EE,#EE,#EE,#EE,#EE,#EE,#3E, #8E,#EE,#EE,#EE,#EE,#EE,#EE,#E8
gfx_box_0s   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #83,#33,#33,#33,#33,#33,#33,#38, #31,#33,#33,#33,#33,#33,#33,#13, #33,#11,#11,#11,#11,#11,#11,#33, #33,#13,#33,#33,#33,#33,#32,#33
db #33,#13,#33,#33,#33,#33,#32,#33, #33,#13,#33,#33,#33,#33,#32,#33, #33,#13,#33,#33,#33,#33,#32,#33, #33,#13,#33,#32,#23,#33,#32,#33
db #33,#13,#33,#32,#23,#33,#32,#33, #33,#13,#33,#33,#33,#33,#32,#33, #33,#13,#33,#33,#33,#33,#32,#33, #33,#13,#33,#33,#33,#33,#32,#33
db #33,#13,#33,#33,#33,#33,#32,#33, #33,#22,#22,#22,#22,#22,#22,#33, #32,#33,#33,#33,#33,#33,#33,#23, #83,#33,#33,#33,#33,#33,#33,#38

gfx_box_1u   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #87,#77,#77,#77,#77,#77,#77,#78, #71,#77,#77,#77,#77,#77,#77,#17, #77,#11,#11,#11,#11,#11,#11,#77, #77,#17,#77,#77,#77,#77,#76,#77
db #77,#17,#77,#77,#77,#77,#76,#77, #77,#17,#77,#77,#77,#77,#76,#77, #77,#17,#77,#78,#88,#77,#76,#77, #77,#17,#77,#78,#88,#77,#76,#77
db #77,#17,#78,#87,#87,#88,#76,#77, #77,#17,#78,#88,#88,#88,#76,#77, #77,#17,#78,#87,#87,#88,#76,#77, #77,#17,#77,#78,#88,#77,#76,#77
db #77,#17,#77,#77,#77,#77,#76,#77, #77,#66,#66,#66,#66,#66,#66,#77, #76,#77,#77,#77,#77,#77,#77,#67, #87,#77,#77,#77,#77,#77,#77,#78
gfx_box_1s   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #85,#55,#55,#55,#55,#55,#55,#58, #51,#55,#55,#55,#55,#55,#55,#15, #55,#11,#11,#11,#11,#11,#11,#55, #55,#15,#55,#55,#55,#55,#57,#55
db #55,#15,#55,#55,#55,#55,#57,#55, #55,#15,#55,#55,#55,#55,#57,#55, #55,#15,#55,#56,#66,#55,#57,#55, #55,#15,#55,#56,#66,#55,#57,#55
db #55,#15,#56,#65,#65,#66,#57,#55, #55,#15,#56,#66,#66,#66,#57,#55, #55,#15,#56,#65,#65,#66,#57,#55, #55,#15,#55,#56,#66,#55,#57,#55
db #55,#15,#55,#55,#55,#55,#57,#55, #55,#77,#77,#77,#77,#77,#77,#55, #57,#55,#55,#55,#55,#55,#55,#75, #85,#55,#55,#55,#55,#55,#55,#58

gfx_box_2u   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #8F,#FF,#FF,#FF,#FF,#FF,#FF,#F8, #F1,#FF,#FF,#FF,#FF,#FF,#FF,#1F, #FF,#11,#11,#11,#11,#11,#11,#FF, #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF
db #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF, #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF, #FF,#1F,#FF,#88,#F8,#8F,#FE,#FF, #FF,#1F,#F8,#88,#88,#88,#FE,#FF
db #FF,#1F,#F8,#88,#88,#88,#FE,#FF, #FF,#1F,#FF,#88,#88,#8F,#FE,#FF, #FF,#1F,#FF,#F8,#88,#FF,#FE,#FF, #FF,#1F,#FF,#FF,#8F,#FF,#FE,#FF
db #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF, #FF,#EE,#EE,#EE,#EE,#EE,#EE,#FF, #FE,#FF,#FF,#FF,#FF,#FF,#FF,#EF, #8F,#FF,#FF,#FF,#FF,#FF,#FF,#F8
gfx_box_2s   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #83,#33,#33,#33,#33,#33,#33,#38, #31,#33,#33,#33,#33,#33,#33,#13, #33,#11,#11,#11,#11,#11,#11,#33, #33,#13,#33,#33,#33,#33,#3F,#33
db #33,#13,#33,#33,#33,#33,#3F,#33, #33,#13,#33,#33,#33,#33,#3F,#33, #33,#13,#33,#22,#32,#23,#3F,#33, #33,#13,#32,#22,#22,#22,#3F,#33
db #33,#13,#32,#22,#22,#22,#3F,#33, #33,#13,#33,#22,#22,#23,#3F,#33, #33,#13,#33,#32,#22,#33,#3F,#33, #33,#13,#33,#33,#23,#33,#3F,#33
db #33,#13,#33,#33,#33,#33,#3F,#33, #33,#FF,#FF,#FF,#FF,#FF,#FF,#33, #3F,#33,#33,#33,#33,#33,#33,#F3, #83,#33,#33,#33,#33,#33,#33,#38

gfx_box_3u   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #8A,#AA,#AA,#AA,#AA,#AA,#AA,#A8, #A1,#AA,#AA,#AA,#AA,#AA,#AA,#1A, #AA,#11,#11,#11,#11,#11,#11,#AA, #AA,#1A,#AA,#AA,#AA,#AA,#A8,#AA
db #AA,#1A,#AA,#AA,#AA,#AA,#A8,#AA, #AA,#1A,#AA,#AA,#9A,#AA,#A8,#AA, #AA,#1A,#AA,#A9,#99,#AA,#A8,#AA, #AA,#1A,#AA,#99,#99,#9A,#A8,#AA
db #AA,#1A,#A9,#99,#99,#99,#A8,#AA, #AA,#1A,#AA,#99,#99,#9A,#A8,#AA, #AA,#1A,#AA,#A9,#99,#AA,#A8,#AA, #AA,#1A,#AA,#AA,#9A,#AA,#A8,#AA
db #AA,#1A,#AA,#AA,#AA,#AA,#A8,#AA, #AA,#88,#88,#88,#88,#88,#88,#AA, #A8,#AA,#AA,#AA,#AA,#AA,#AA,#8A, #8A,#AA,#AA,#AA,#AA,#AA,#AA,#A8
gfx_box_3s   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #89,#99,#99,#99,#99,#99,#99,#98, #91,#99,#99,#99,#99,#99,#99,#19, #99,#11,#11,#11,#11,#11,#11,#99, #99,#19,#99,#99,#99,#99,#9A,#99
db #99,#19,#99,#99,#99,#99,#9A,#99, #99,#19,#99,#99,#A9,#99,#9A,#99, #99,#19,#99,#9A,#AA,#99,#9A,#99, #99,#19,#99,#AA,#AA,#A9,#9A,#99
db #99,#19,#9A,#AA,#AA,#AA,#9A,#99, #99,#19,#99,#AA,#AA,#A9,#9A,#99, #99,#19,#99,#9A,#AA,#99,#9A,#99, #99,#19,#99,#99,#A9,#99,#9A,#99
db #99,#19,#99,#99,#99,#99,#9A,#99, #99,#AA,#AA,#AA,#AA,#AA,#AA,#99, #9A,#99,#99,#99,#99,#99,#99,#A9, #89,#99,#99,#99,#99,#99,#99,#98

gfx_box_4u   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #87,#77,#77,#77,#77,#77,#77,#78, #71,#77,#77,#77,#77,#77,#77,#17, #77,#11,#11,#11,#11,#11,#11,#77, #77,#17,#77,#77,#77,#77,#76,#77
db #77,#17,#77,#77,#77,#77,#76,#77, #77,#17,#77,#78,#88,#77,#76,#77, #77,#17,#77,#88,#88,#87,#76,#77, #77,#17,#78,#88,#88,#88,#76,#77
db #77,#17,#78,#88,#88,#88,#76,#77, #77,#17,#78,#88,#88,#88,#76,#77, #77,#17,#77,#88,#88,#87,#76,#77, #77,#17,#77,#78,#88,#77,#76,#77
db #77,#17,#77,#77,#77,#77,#76,#77, #77,#66,#66,#66,#66,#66,#66,#77, #76,#77,#77,#77,#77,#77,#77,#67, #87,#77,#77,#77,#77,#77,#77,#78
gfx_box_4s   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #85,#55,#55,#55,#55,#55,#55,#58, #51,#55,#55,#55,#55,#55,#55,#15, #55,#11,#11,#11,#11,#11,#11,#55, #55,#15,#55,#55,#55,#55,#57,#55
db #55,#15,#55,#55,#55,#55,#57,#55, #55,#15,#55,#56,#66,#55,#57,#55, #55,#15,#55,#66,#66,#65,#57,#55, #55,#15,#56,#66,#66,#66,#57,#55
db #55,#15,#56,#66,#66,#66,#57,#55, #55,#15,#56,#66,#66,#66,#57,#55, #55,#15,#55,#66,#66,#65,#57,#55, #55,#15,#55,#56,#66,#55,#57,#55
db #55,#15,#55,#55,#55,#55,#57,#55, #55,#77,#77,#77,#77,#77,#77,#55, #57,#55,#55,#55,#55,#55,#55,#75, #85,#55,#55,#55,#55,#55,#55,#58

gfx_box_5u   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #8F,#FF,#FF,#FF,#FF,#FF,#FF,#F8, #F1,#FF,#FF,#FF,#FF,#FF,#FF,#1F, #FF,#11,#11,#11,#11,#11,#11,#FF, #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF
db #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF, #FF,#1F,#FF,#FF,#8F,#FF,#FE,#FF, #FF,#1F,#FF,#F8,#88,#FF,#FE,#FF, #FF,#1F,#FF,#88,#88,#8F,#FE,#FF
db #FF,#1F,#F8,#88,#88,#88,#FE,#FF, #FF,#1F,#F8,#88,#88,#88,#FE,#FF, #FF,#1F,#FF,#FF,#8F,#FF,#FE,#FF, #FF,#1F,#FF,#88,#88,#8F,#FE,#FF
db #FF,#1F,#FF,#FF,#FF,#FF,#FE,#FF, #FF,#EE,#EE,#EE,#EE,#EE,#EE,#FF, #FE,#FF,#FF,#FF,#FF,#FF,#FF,#EF, #8F,#FF,#FF,#FF,#FF,#FF,#FF,#F8
gfx_box_5s   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #83,#33,#33,#33,#33,#33,#33,#38, #31,#33,#33,#33,#33,#33,#33,#13, #33,#11,#11,#11,#11,#11,#11,#33, #33,#13,#33,#33,#33,#33,#3F,#33
db #33,#13,#33,#33,#33,#33,#3F,#33, #33,#13,#33,#33,#23,#33,#3F,#33, #33,#13,#33,#32,#22,#33,#3F,#33, #33,#13,#33,#22,#22,#23,#3F,#33
db #33,#13,#32,#22,#22,#22,#3F,#33, #33,#13,#32,#22,#22,#22,#3F,#33, #33,#13,#33,#33,#23,#33,#3F,#33, #33,#13,#33,#22,#22,#23,#3F,#33
db #33,#13,#33,#33,#33,#33,#3F,#33, #33,#FF,#FF,#FF,#FF,#FF,#FF,#33, #3F,#33,#33,#33,#33,#33,#33,#F3, #83,#33,#33,#33,#33,#33,#33,#38


;### maze elements

gfx_floor   db 8,16,16:dw $+7:dw $+4,8*16:db 5
ds 8*16,#88

gfx_black   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #77,#77,#77,#17,#77,#77,#77,#17, #77,#77,#77,#71,#77,#77,#77,#71, #17,#77,#77,#77,#17,#77,#67,#77, #71,#77,#77,#77,#71,#77,#77,#77
db #77,#17,#77,#77,#77,#17,#77,#77, #77,#71,#77,#77,#77,#71,#77,#77, #77,#77,#17,#77,#67,#77,#17,#77, #77,#77,#71,#77,#77,#77,#71,#77
db #77,#77,#77,#17,#77,#77,#77,#17, #77,#77,#77,#71,#77,#77,#77,#71, #17,#77,#67,#77,#17,#77,#77,#77, #71,#77,#77,#77,#71,#77,#77,#77
db #77,#17,#77,#77,#77,#17,#77,#77, #77,#71,#77,#77,#77,#71,#77,#77, #77,#77,#17,#77,#77,#77,#17,#77, #77,#77,#71,#77,#77,#77,#71,#77

gfx_wall    db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #3C,#22,#22,#22,#3C,#22,#22,#22, #3C,#22,#22,#22,#3C,#22,#22,#22, #3C,#CC,#CC,#CC,#3C,#CC,#CC,#CC, #33,#33,#33,#33,#33,#33,#33,#33
db #22,#22,#3C,#22,#22,#22,#3C,#22, #22,#22,#3C,#22,#22,#22,#3C,#22, #CC,#CC,#3C,#CC,#CC,#CC,#3C,#CC, #33,#33,#33,#33,#33,#33,#33,#33
db #3C,#22,#22,#22,#3C,#22,#22,#22, #3C,#22,#22,#22,#3C,#22,#22,#22, #3C,#CC,#CC,#CC,#3C,#CC,#CC,#CC, #33,#33,#33,#33,#33,#33,#33,#33
db #22,#22,#3C,#22,#22,#22,#3C,#22, #22,#22,#3C,#22,#22,#22,#3C,#22, #CC,#CC,#3C,#CC,#CC,#CC,#3C,#CC, #33,#33,#33,#33,#33,#33,#33,#33

gfx_floor_fragile   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #77,#77,#77,#77,#77,#77,#77,#77, #78,#88,#88,#88,#88,#88,#88,#87, #78,#77,#77,#77,#77,#77,#77,#87, #78,#77,#88,#77,#77,#88,#77,#87
db #78,#78,#88,#87,#78,#88,#87,#87, #78,#78,#88,#88,#88,#88,#87,#87, #78,#77,#88,#88,#88,#88,#77,#87, #78,#77,#78,#88,#88,#87,#77,#87
db #78,#77,#78,#88,#88,#87,#77,#87, #78,#77,#88,#88,#88,#88,#77,#87, #78,#78,#88,#88,#88,#88,#87,#87, #78,#78,#88,#87,#78,#88,#87,#87
db #78,#77,#88,#77,#77,#88,#77,#87, #78,#77,#77,#77,#77,#77,#77,#87, #78,#88,#88,#88,#88,#88,#88,#87, #77,#77,#77,#77,#77,#77,#77,#77

gfx_floor_hole      db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #87,#87,#87,#87,#87,#87,#87,#88, #88,#88,#88,#88,#88,#88,#88,#78, #87,#87,#77,#77,#77,#77,#78,#88
db #88,#87,#88,#88,#88,#88,#78,#78, #87,#87,#85,#55,#55,#58,#78,#88, #88,#87,#85,#88,#88,#58,#78,#78, #87,#87,#85,#81,#18,#58,#78,#88
db #88,#87,#85,#81,#18,#58,#78,#78, #87,#87,#85,#88,#88,#58,#78,#88, #88,#87,#85,#55,#55,#58,#78,#78, #87,#87,#88,#88,#88,#88,#78,#88
db #88,#87,#77,#77,#77,#77,#78,#78, #87,#88,#88,#88,#88,#88,#88,#88, #88,#78,#78,#78,#78,#78,#78,#78, #88,#88,#88,#88,#88,#88,#88,#88

gfx_floor_stuffed   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #87,#87,#87,#87,#87,#87,#87,#88, #88,#88,#88,#88,#88,#88,#88,#78, #87,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#78, #87,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#78, #87,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#78, #87,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#78, #87,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#78, #87,#88,#88,#88,#88,#88,#88,#88, #88,#78,#78,#78,#78,#78,#78,#78, #88,#88,#88,#88,#88,#88,#88,#88

gfx_floor_dead      db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #87,#87,#87,#87,#87,#87,#87,#88, #88,#88,#88,#DD,#DD,#88,#88,#78, #87,#87,#77,#DD,#DD,#77,#78,#88
db #88,#87,#88,#DD,#DD,#88,#78,#78, #87,#DD,#DD,#DD,#DD,#DD,#DD,#88, #88,#DD,#44,#DD,#4D,#44,#DD,#78, #87,#DD,#4D,#4D,#4D,#4D,#DD,#88
db #88,#DD,#DD,#DD,#DD,#DD,#DD,#78, #87,#87,#85,#DD,#DD,#58,#78,#88, #88,#87,#85,#DD,#DD,#58,#78,#78, #87,#87,#88,#DD,#DD,#88,#78,#88
db #88,#87,#77,#DD,#DD,#77,#78,#78, #87,#88,#88,#DD,#DD,#88,#88,#88, #88,#78,#78,#78,#78,#78,#78,#78, #88,#88,#88,#88,#88,#88,#88,#88


gfx_ice     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #86,#86,#64,#84,#66,#66,#48,#87, #86,#66,#48,#46,#68,#64,#88,#87, #86,#64,#84,#66,#66,#48,#88,#47
db #86,#48,#46,#86,#64,#88,#84,#67, #84,#84,#66,#66,#48,#88,#46,#67, #88,#46,#66,#64,#88,#84,#68,#67, #84,#66,#86,#48,#88,#46,#66,#67
db #86,#66,#64,#88,#84,#66,#66,#47, #88,#66,#48,#88,#46,#68,#64,#87, #86,#64,#88,#84,#66,#66,#48,#47, #86,#48,#88,#46,#86,#64,#84,#67
db #84,#88,#84,#66,#66,#48,#46,#67, #88,#88,#46,#66,#64,#84,#66,#67, #88,#84,#66,#86,#48,#46,#68,#67, #77,#77,#77,#77,#77,#77,#77,#77


gfx_rail_lr     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #11,#11,#11,#11,#11,#11,#11,#11
db #88,#88,#88,#88,#88,#88,#88,#88, #66,#66,#66,#66,#66,#66,#66,#66, #11,#11,#11,#11,#11,#11,#11,#11, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #11,#11,#11,#11,#11,#11,#11,#11, #66,#66,#66,#66,#66,#66,#66,#66, #88,#88,#88,#88,#88,#88,#88,#88
db #11,#11,#11,#11,#11,#11,#11,#11, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
gfx_rail_ud     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
gfx_rail_elu    db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#81,#11,#11,#11,#11,#11,#11
db #88,#81,#88,#88,#88,#88,#88,#88, #88,#81,#86,#66,#66,#66,#66,#66, #88,#81,#86,#11,#11,#11,#11,#11, #88,#81,#86,#18,#88,#88,#88,#88
db #88,#81,#86,#18,#88,#88,#88,#88, #88,#81,#86,#18,#81,#11,#11,#11, #88,#81,#86,#18,#81,#66,#66,#66, #88,#81,#86,#18,#81,#68,#88,#88
db #88,#81,#86,#18,#81,#68,#11,#11, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
gfx_rail_eld    db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#11,#11
db #88,#81,#86,#18,#81,#68,#88,#88, #88,#81,#86,#18,#81,#66,#66,#66, #88,#81,#86,#18,#81,#11,#11,#11, #88,#81,#86,#18,#88,#88,#88,#88
db #88,#81,#86,#18,#88,#88,#88,#88, #88,#81,#86,#11,#11,#11,#11,#11, #88,#81,#86,#66,#66,#66,#66,#66, #88,#81,#88,#88,#88,#88,#88,#88
db #88,#81,#11,#11,#11,#11,#11,#11, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
gfx_rail_eru    db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #11,#11,#11,#11,#11,#11,#18,#88
db #88,#88,#88,#88,#88,#88,#18,#88, #66,#66,#66,#66,#66,#68,#18,#88, #11,#11,#11,#11,#11,#68,#18,#88, #88,#88,#88,#88,#81,#68,#18,#88
db #88,#88,#88,#88,#81,#68,#18,#88, #11,#11,#11,#18,#81,#68,#18,#88, #66,#66,#66,#18,#81,#68,#18,#88, #88,#88,#86,#18,#81,#68,#18,#88
db #11,#11,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
gfx_rail_erd     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #11,#11,#86,#18,#81,#68,#18,#88
db #88,#88,#86,#18,#81,#68,#18,#88, #66,#66,#66,#18,#81,#68,#18,#88, #11,#11,#11,#18,#81,#68,#18,#88, #88,#88,#88,#88,#81,#68,#18,#88
db #88,#88,#88,#88,#81,#68,#18,#88, #11,#11,#11,#11,#11,#68,#18,#88, #66,#66,#66,#66,#66,#68,#18,#88, #88,#88,#88,#88,#88,#88,#18,#88
db #11,#11,#11,#11,#11,#11,#18,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
gfx_rail_tr     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#11,#11
db #88,#81,#86,#18,#81,#68,#88,#88, #88,#81,#86,#18,#81,#66,#66,#66, #88,#81,#86,#18,#81,#11,#11,#11, #88,#81,#86,#18,#88,#88,#88,#88
db #88,#81,#86,#18,#88,#88,#88,#88, #88,#81,#86,#18,#81,#11,#11,#11, #88,#81,#86,#18,#81,#66,#66,#66, #88,#81,#86,#18,#81,#68,#88,#88
db #88,#81,#86,#18,#81,#68,#11,#11, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
gfx_rail_tl     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #11,#11,#86,#18,#81,#68,#18,#88
db #88,#88,#86,#18,#81,#68,#18,#88, #66,#66,#66,#18,#81,#68,#18,#88, #11,#11,#11,#18,#81,#68,#18,#88, #88,#88,#88,#88,#81,#68,#18,#88
db #88,#88,#88,#88,#81,#68,#18,#88, #11,#11,#11,#18,#81,#68,#18,#88, #66,#66,#66,#18,#81,#68,#18,#88, #88,#88,#86,#18,#81,#68,#18,#88
db #11,#11,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88
gfx_rail_tu     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #11,#11,#86,#18,#81,#68,#11,#11
db #88,#88,#86,#18,#81,#68,#88,#88, #66,#66,#66,#18,#81,#66,#66,#66, #11,#11,#11,#18,#81,#11,#11,#11, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #11,#11,#11,#11,#11,#11,#11,#11, #66,#66,#66,#66,#66,#66,#66,#66, #88,#88,#88,#88,#88,#88,#88,#88
db #11,#11,#11,#11,#11,#11,#11,#11, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
gfx_rail_td     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #11,#11,#11,#11,#11,#11,#11,#11
db #88,#88,#88,#88,#88,#88,#88,#88, #66,#66,#66,#66,#66,#66,#66,#66, #11,#11,#11,#11,#11,#11,#11,#11, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #11,#11,#11,#18,#81,#11,#11,#11, #66,#66,#66,#18,#81,#66,#66,#66, #88,#88,#86,#18,#81,#68,#88,#88
db #11,#11,#86,#18,#81,#68,#11,#11, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88, #88,#81,#86,#18,#81,#68,#18,#88

gfx_magnet_up       db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #11,#11,#11,#88,#88,#11,#11,#11, #18,#88,#81,#88,#88,#18,#88,#81, #18,#88,#81,#88,#88,#18,#88,#81, #18,#88,#81,#88,#88,#18,#88,#81
db #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1
db #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#FF,#11,#11,#FF,#FF,#F1
db #12,#FF,#FF,#FF,#FF,#FF,#FF,#21, #81,#2F,#FF,#FF,#FF,#FF,#F2,#18, #88,#12,#22,#22,#22,#22,#21,#88, #88,#81,#11,#11,#11,#11,#18,#88
gfx_magnet_right    db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#11,#11,#11,#11,#11,#11, #88,#12,#FF,#FF,#FF,#FF,#88,#81, #81,#2F,#FF,#FF,#FF,#FF,#88,#81, #12,#FF,#FF,#FF,#FF,#FF,#88,#81
db #12,#FF,#FF,#FF,#FF,#FF,#88,#81, #12,#FF,#F1,#11,#11,#11,#11,#11, #12,#FF,#18,#88,#88,#88,#88,#88, #12,#FF,#18,#88,#88,#88,#88,#88
db #12,#FF,#18,#88,#88,#88,#88,#88, #12,#FF,#18,#88,#88,#88,#88,#88, #12,#FF,#F1,#11,#11,#11,#11,#11, #12,#FF,#FF,#FF,#FF,#FF,#88,#81
db #12,#FF,#FF,#FF,#FF,#FF,#88,#81, #81,#2F,#FF,#FF,#FF,#FF,#88,#81, #88,#12,#FF,#FF,#FF,#FF,#88,#81, #88,#81,#11,#11,#11,#11,#11,#11
gfx_magnet_down     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#81,#11,#11,#11,#11,#18,#88, #88,#12,#22,#22,#22,#22,#21,#88, #81,#2F,#FF,#FF,#FF,#FF,#F2,#18, #12,#FF,#FF,#FF,#FF,#FF,#FF,#21
db #1F,#FF,#FF,#11,#11,#FF,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1
db #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1, #1F,#FF,#F1,#88,#88,#1F,#FF,#F1
db #18,#88,#81,#88,#88,#18,#88,#81, #18,#88,#81,#88,#88,#18,#88,#81, #18,#88,#81,#88,#88,#18,#88,#81, #11,#11,#11,#88,#88,#11,#11,#11
gfx_magnet_left     db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #11,#11,#11,#11,#11,#11,#18,#88, #18,#88,#FF,#FF,#FF,#FF,#21,#88, #18,#88,#FF,#FF,#FF,#FF,#F2,#18, #18,#88,#FF,#FF,#FF,#FF,#FF,#21
db #18,#88,#FF,#FF,#FF,#FF,#FF,#21, #11,#11,#11,#11,#11,#1F,#FF,#21, #88,#88,#88,#88,#88,#81,#FF,#21, #88,#88,#88,#88,#88,#81,#FF,#21
db #88,#88,#88,#88,#88,#81,#FF,#21, #88,#88,#88,#88,#88,#81,#FF,#21, #11,#11,#11,#11,#11,#1F,#FF,#21, #18,#88,#FF,#FF,#FF,#FF,#FF,#21
db #18,#88,#FF,#FF,#FF,#FF,#FF,#21, #18,#88,#FF,#FF,#FF,#FF,#F2,#18, #18,#88,#FF,#FF,#FF,#FF,#21,#88, #11,#11,#11,#11,#11,#11,#18,#88
gfx_magnet_switch   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #3C,#22,#22,#21,#1C,#22,#22,#22, #3C,#22,#22,#1F,#F1,#22,#22,#22, #3C,#CC,#CC,#1F,#F1,#CC,#CC,#CC, #33,#36,#66,#61,#16,#66,#63,#33
db #22,#26,#77,#78,#87,#77,#1C,#22, #22,#26,#77,#78,#87,#77,#1C,#22, #CC,#C6,#77,#78,#87,#77,#1C,#CC, #33,#36,#71,#11,#11,#17,#13,#33
db #3C,#26,#71,#11,#11,#17,#12,#22, #3C,#26,#77,#71,#17,#77,#12,#22, #3C,#C6,#77,#71,#17,#77,#1C,#CC, #33,#36,#77,#77,#77,#77,#13,#33
db #22,#21,#11,#11,#11,#11,#1C,#22, #22,#22,#3C,#22,#22,#22,#3C,#22, #CC,#CC,#3C,#CC,#CC,#CC,#3C,#CC, #33,#33,#33,#33,#33,#33,#33,#33

gfx_portal_1        db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #81,#11,#11,#11,#11,#11,#11,#18, #19,#99,#99,#99,#99,#99,#99,#91, #19,#AA,#AA,#AA,#AA,#AA,#AA,#91, #19,#A9,#99,#99,#99,#99,#9A,#91
db #19,#A9,#AA,#AA,#AA,#AA,#9A,#91, #19,#A9,#A9,#99,#99,#9A,#9A,#91, #19,#A9,#A9,#AA,#AA,#9A,#9A,#91, #19,#A9,#A9,#A9,#9A,#9A,#9A,#91
db #19,#A9,#A9,#A9,#9A,#9A,#9A,#91, #19,#A9,#A9,#AA,#AA,#9A,#9A,#91, #19,#A9,#A9,#99,#99,#9A,#9A,#91, #19,#A9,#AA,#AA,#AA,#AA,#9A,#91
db #19,#A9,#99,#99,#99,#99,#9A,#91, #19,#AA,#AA,#AA,#AA,#AA,#AA,#91, #19,#99,#99,#99,#99,#99,#99,#91, #81,#11,#11,#11,#11,#11,#11,#18
gfx_portal_2        db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #81,#11,#11,#11,#11,#11,#11,#18, #1F,#FF,#FF,#FF,#FF,#FF,#FF,#F1, #1F,#33,#33,#33,#33,#33,#33,#F1, #1F,#32,#22,#22,#22,#22,#23,#F1
db #1F,#32,#33,#33,#33,#33,#23,#F1, #1F,#32,#32,#22,#22,#23,#23,#F1, #1F,#32,#32,#33,#33,#23,#23,#F1, #1F,#32,#32,#32,#23,#23,#23,#F1
db #1F,#32,#32,#32,#23,#23,#23,#F1, #1F,#32,#32,#33,#33,#23,#23,#F1, #1F,#32,#32,#22,#22,#23,#23,#F1, #1F,#32,#33,#33,#33,#33,#23,#F1
db #1F,#32,#22,#22,#22,#22,#23,#F1, #1F,#33,#33,#33,#33,#33,#33,#F1, #1F,#FF,#FF,#FF,#FF,#FF,#FF,#F1, #81,#11,#11,#11,#11,#11,#11,#18
gfx_portal_3        db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #81,#11,#11,#11,#11,#11,#11,#18, #17,#77,#77,#77,#77,#77,#77,#71, #17,#66,#66,#66,#66,#66,#66,#71, #17,#64,#44,#44,#44,#44,#46,#71
db #17,#64,#66,#66,#66,#66,#46,#71, #17,#64,#64,#44,#44,#46,#46,#71, #17,#64,#64,#66,#66,#46,#46,#71, #17,#64,#64,#64,#46,#46,#46,#71
db #17,#64,#64,#64,#46,#46,#46,#71, #17,#64,#64,#66,#66,#46,#46,#71, #17,#64,#64,#44,#44,#46,#46,#71, #17,#64,#66,#66,#66,#66,#46,#71
db #17,#64,#44,#44,#44,#44,#46,#71, #17,#66,#66,#66,#66,#66,#66,#71, #17,#77,#77,#77,#77,#77,#77,#71, #81,#11,#11,#11,#11,#11,#11,#18


;### destinations

gfx_des_0   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#FF,#FF,#88,#88,#88, #88,#88,#8F,#88,#88,#F8,#88,#88, #88,#88,#8F,#8F,#F8,#F8,#88,#88
db #88,#88,#8F,#8F,#F8,#F8,#88,#88, #88,#88,#8F,#88,#88,#F8,#88,#88, #88,#88,#88,#FF,#FF,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_des_1   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#77,#78,#88,#88,#88, #88,#88,#88,#77,#78,#88,#88,#88, #88,#88,#77,#87,#87,#78,#88,#88
db #88,#88,#77,#77,#77,#78,#88,#88, #88,#88,#77,#87,#87,#78,#88,#88, #88,#88,#88,#77,#78,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_des_2   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#8F,#F8,#FF,#88,#88,#88, #88,#88,#FF,#FF,#FF,#F8,#88,#88, #88,#88,#FF,#FF,#FF,#F8,#88,#88
db #88,#88,#8F,#FF,#FF,#88,#88,#88, #88,#88,#88,#FF,#F8,#88,#88,#88, #88,#88,#88,#8F,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_des_3   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#89,#88,#88,#88,#88, #88,#88,#88,#99,#98,#88,#88,#88, #88,#88,#89,#99,#99,#88,#88,#88
db #88,#88,#99,#99,#99,#98,#88,#88, #88,#88,#89,#99,#99,#88,#88,#88, #88,#88,#88,#99,#98,#88,#88,#88, #88,#88,#88,#89,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_des_4   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#77,#78,#88,#88,#88, #88,#88,#87,#77,#77,#88,#88,#88, #88,#88,#77,#77,#77,#78,#88,#88
db #88,#88,#77,#77,#77,#78,#88,#88, #88,#88,#77,#77,#77,#78,#88,#88, #88,#88,#87,#77,#77,#88,#88,#88, #88,#88,#88,#77,#78,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88

gfx_des_5   db 8,16,16:dw $+7:dw $+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#8F,#88,#88,#88,#88, #88,#88,#88,#FF,#F8,#88,#88,#88, #88,#88,#8F,#FF,#FF,#88,#88,#88
db #88,#88,#FF,#FF,#FF,#F8,#88,#88, #88,#88,#FF,#FF,#FF,#F8,#88,#88, #88,#88,#88,#8F,#88,#88,#88,#88, #88,#88,#8F,#FF,#FF,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88, #88,#88,#88,#88,#88,#88,#88,#88


;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns

;### PRGPRZS -> Stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig
prgmsgwin  dw prgmsgwon1,4*1+2,prgmsgwon2,4*1+2,prgmsgwon3,4*1+2,prgicnbig
prgmsgerr  dw prgmsgerr1,4*1+2,prgmsgerr2,4*1+2,prgmsgerr3,4*1+2,prgicnbig


;### MAIN WINDOW ##############################################################

wingam       dw #3501,0,63,3,225,160,0,0,225,160,225,160,225,160,prgicnsml,wintittxt,0,prgwinmen,wingamgrp,0,0:ds 136+14

wingamgrp    db 132,0: dw wingamdat,0,0,0,0,0,0
wingamdat
dw      0,255*256+ 0, 2,                  0,  0,225,160,0
wingam_map_num  equ 1
wingam_map
dw 00,255*256+10,gfx_black,  0,  0,16,16,0, 01,255*256+10,gfx_black, 16,  0,16,16,0, 02,255*256+10,gfx_black, 32,  0,16,16,0, 03,255*256+10,gfx_black, 48,  0,16,16,0, 04,255*256+10,gfx_black, 64,  0,16,16,0
dw 05,255*256+10,gfx_black, 80,  0,16,16,0, 06,255*256+10,gfx_black, 96,  0,16,16,0, 07,255*256+10,gfx_black,112,  0,16,16,0, 08,255*256+10,gfx_black,128,  0,16,16,0, 09,255*256+10,gfx_black,144,  0,16,16,0
dw 10,255*256+10,gfx_black,  0, 16,16,16,0, 11,255*256+10,gfx_black, 16, 16,16,16,0, 12,255*256+10,gfx_black, 32, 16,16,16,0, 13,255*256+10,gfx_black, 48, 16,16,16,0, 14,255*256+10,gfx_black, 64, 16,16,16,0
dw 15,255*256+10,gfx_black, 80, 16,16,16,0, 16,255*256+10,gfx_black, 96, 16,16,16,0, 17,255*256+10,gfx_black,112, 16,16,16,0, 18,255*256+10,gfx_black,128, 16,16,16,0, 19,255*256+10,gfx_black,144, 16,16,16,0
dw 20,255*256+10,gfx_black,  0, 32,16,16,0, 21,255*256+10,gfx_black, 16, 32,16,16,0, 22,255*256+10,gfx_black, 32, 32,16,16,0, 23,255*256+10,gfx_black, 48, 32,16,16,0, 24,255*256+10,gfx_black, 64, 32,16,16,0
dw 25,255*256+10,gfx_black, 80, 32,16,16,0, 26,255*256+10,gfx_black, 96, 32,16,16,0, 27,255*256+10,gfx_black,112, 32,16,16,0, 28,255*256+10,gfx_black,128, 32,16,16,0, 29,255*256+10,gfx_black,144, 32,16,16,0
dw 30,255*256+10,gfx_black,  0, 48,16,16,0, 31,255*256+10,gfx_black, 16, 48,16,16,0, 32,255*256+10,gfx_black, 32, 48,16,16,0, 33,255*256+10,gfx_black, 48, 48,16,16,0, 34,255*256+10,gfx_black, 64, 48,16,16,0
dw 35,255*256+10,gfx_black, 80, 48,16,16,0, 36,255*256+10,gfx_black, 96, 48,16,16,0, 37,255*256+10,gfx_black,112, 48,16,16,0, 38,255*256+10,gfx_black,128, 48,16,16,0, 39,255*256+10,gfx_black,144, 48,16,16,0
dw 40,255*256+10,gfx_black,  0, 64,16,16,0, 41,255*256+10,gfx_black, 16, 64,16,16,0, 42,255*256+10,gfx_black, 32, 64,16,16,0, 43,255*256+10,gfx_black, 48, 64,16,16,0, 44,255*256+10,gfx_black, 64, 64,16,16,0
dw 45,255*256+10,gfx_black, 80, 64,16,16,0, 46,255*256+10,gfx_black, 96, 64,16,16,0, 47,255*256+10,gfx_black,112, 64,16,16,0, 48,255*256+10,gfx_black,128, 64,16,16,0, 49,255*256+10,gfx_black,144, 64,16,16,0
dw 50,255*256+10,gfx_black,  0, 80,16,16,0, 51,255*256+10,gfx_black, 16, 80,16,16,0, 52,255*256+10,gfx_black, 32, 80,16,16,0, 53,255*256+10,gfx_black, 48, 80,16,16,0, 54,255*256+10,gfx_black, 64, 80,16,16,0
dw 55,255*256+10,gfx_black, 80, 80,16,16,0, 56,255*256+10,gfx_black, 96, 80,16,16,0, 57,255*256+10,gfx_black,112, 80,16,16,0, 58,255*256+10,gfx_black,128, 80,16,16,0, 59,255*256+10,gfx_black,144, 80,16,16,0
dw 60,255*256+10,gfx_black,  0, 96,16,16,0, 61,255*256+10,gfx_black, 16, 96,16,16,0, 62,255*256+10,gfx_black, 32, 96,16,16,0, 63,255*256+10,gfx_black, 48, 96,16,16,0, 64,255*256+10,gfx_black, 64, 96,16,16,0
dw 65,255*256+10,gfx_black, 80, 96,16,16,0, 66,255*256+10,gfx_black, 96, 96,16,16,0, 67,255*256+10,gfx_black,112, 96,16,16,0, 68,255*256+10,gfx_black,128, 96,16,16,0, 69,255*256+10,gfx_black,144, 96,16,16,0
dw 70,255*256+10,gfx_black,  0,112,16,16,0, 71,255*256+10,gfx_black, 16,112,16,16,0, 72,255*256+10,gfx_black, 32,112,16,16,0, 73,255*256+10,gfx_black, 48,112,16,16,0, 74,255*256+10,gfx_black, 64,112,16,16,0
dw 75,255*256+10,gfx_black, 80,112,16,16,0, 76,255*256+10,gfx_black, 96,112,16,16,0, 77,255*256+10,gfx_black,112,112,16,16,0, 78,255*256+10,gfx_black,128,112,16,16,0, 79,255*256+10,gfx_black,144,112,16,16,0
dw 80,255*256+10,gfx_black,  0,128,16,16,0, 81,255*256+10,gfx_black, 16,128,16,16,0, 82,255*256+10,gfx_black, 32,128,16,16,0, 83,255*256+10,gfx_black, 48,128,16,16,0, 84,255*256+10,gfx_black, 64,128,16,16,0
dw 85,255*256+10,gfx_black, 80,128,16,16,0, 86,255*256+10,gfx_black, 96,128,16,16,0, 87,255*256+10,gfx_black,112,128,16,16,0, 88,255*256+10,gfx_black,128,128,16,16,0, 89,255*256+10,gfx_black,144,128,16,16,0
dw 90,255*256+10,gfx_black,  0,144,16,16,0, 91,255*256+10,gfx_black, 16,144,16,16,0, 92,255*256+10,gfx_black, 32,144,16,16,0, 93,255*256+10,gfx_black, 48,144,16,16,0, 94,255*256+10,gfx_black, 64,144,16,16,0
dw 95,255*256+10,gfx_black, 80,144,16,16,0, 96,255*256+10,gfx_black, 96,144,16,16,0, 97,255*256+10,gfx_black,112,144,16,16,0, 98,255*256+10,gfx_black,128,144,16,16,0, 99,255*256+10,gfx_black,144,144,16,16,0

wingam_man_num  equ 101
wingam_man
dw      0,255*256+10, gfx_man_lw,         0,  0,til_xlen,til_ylen,0
wingam_box_num  equ 102
wingam_box
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0
dw      0,255*256+10, gfx_box_0u,         0,  0,til_xlen,til_ylen,0

dw      0,255*256+ 0, 1,                160,  0,  1,160,0   ;seperation line
dw      0,255*256+10, gfx_logo,         161,  0, 64,45,0    ;logo

dw      0,255*256+01, ctlobjdat1,    161+ 4, 49, 60, 8,0    ;remaining
wingam_dsp_num  equ 121
dw      0,255*256+01, ctlobjdat2,    161+16, 58, 44, 8,0
dw      0,255*256+01, ctlobjdat3,    161+ 4, 68, 60, 8,0    ;finished
dw      0,255*256+01, ctlobjdat4,    161+16, 77, 44, 8,0
dw      0,255*256+01, ctlobjdat5,    161+ 4, 87, 60, 8,0    ;steps
dw      0,255*256+01, ctlobjdat6,    161+16, 96, 44, 8,0
dw      0,255*256+01, ctlobjdat7,    161+ 4,106, 60, 8,0    ;time
dw      0,255*256+01, ctlobjdat8,    161+16,115, 44, 8,0
dw      0,255*256+01, ctlobjdat9,    161+23,146, 18, 8,0    ;level number

dw butres,255*256+16, txtbutres,     161+ 4,129, 56,12,0    ;button "Restart"
dw butpre,255*256+16, txtbutpre,     161+ 4,144, 17,12,0    ;button "<<"
dw butnxt,255*256+16, txtbutnxt,     161+43,144, 17,12,0    ;button ">>"

ctlobjdat1 dw txtbxrtit:db 2+4,0
ctlobjdat2 dw txtbxrval:db 3+0+128,1
ctlobjdat3 dw txtbxftit:db 2+4,0
ctlobjdat4 dw txtbxfval:db 3+0+128,1
ctlobjdat5 dw txtmovtit:db 2+4,0
ctlobjdat6 dw txtmovval:db 3+0+128,1
ctlobjdat7 dw txttimtit:db 2+4,0
ctlobjdat8 dw txttimval:db 3+0+128,1
ctlobjdat9 dw txtlevval:db 3+0+128,2

prgwinmen  dw 5, 1+4,prgwinmentx1,prgwinmen1,0, 1+4,prgwinmentx2,prgwinmen2,0, 1+4,prgwinmentx3,prgwinmen3,0, 1+4,prgwinmentx4,prgwinmen4,0, 1+4,prgwinmentx5,prgwinmen5,0
prgwinmen1 dw 1, 1,prgwinmen1tx0,prgend,0
prgwinmen2 dw 12, 1,prgwinmen2tx1,level1,0, 1,prgwinmen2tx2,level2,0, 1,prgwinmen2tx3,level3,0, 1+8,0,0,0
           dw     1,prgwinmen2tx4,level4,0, 1,prgwinmen2tx5,level5,0, 1,prgwinmen2tx6,level6,0, 1,prgwinmen2tx7,level7,0, 1,prgwinmen2tx8,level8,0, 1,prgwinmen2tx9,level9,0, 1+8,0,0,0
           dw     1,prgwinmen2txu,levelu
prgwinmen3 dw 4, 0,prgwinmen3tx1,undund,0, 1+8,0,0,0, 1,prgwinmen3tx2,undsav,0, 0,prgwinmen3tx3,undlod,0
prgwinmen4 dw 2, 3,prgwinmen4tx1,optslw,0, 1,prgwinmen4tx2,optfst,0
prgwinmen5 dw 3, 1,prgwinmen5tx1,prghlp,0, 1+8,0,0,0, 1,prgwinmen5tx2,prginf,0


prgtrnend
