; Limited to 16x16 sprites, with mask
; Originally taken from a tutorial by dmsmith, then modified
; Optimized by Spirax (thanks a lot!)

; Input:
;	DE: sprite number
;	B: X position
;	C: Y position

; Required sprite alignment:
;		mask first
;		full X line first
;
; So, in SevenuP terminology, this means: X char, Mask, Char line, Y char, interleave: sprite, mask before sprite

; now, the sprite drawing routine includes a sprite cache, with all required handling


DrawSprite:
		ld a, b
		and 7					; A == rotation required
		ld ixl, a

		rl e
		rl d
		rl e
		rl d
		rl e 
		rl d							 ; sprnum << 3 (Carry was 0)
;		push de							 ; now will not touch DE, no need to preserve
	
		or e                             ; DE = (sprnum << 3) | rotation
		ld l,a
		
		ld a, d
		add a, high SprCacheTable
		ld h, a                          ; HL = SprCacheTable[(sprnum << 3) | rotation]
		ld a, (hl)

;		pop de
		push bc
		ld h, LRU_prev / 256			 ; pointer to the LRU_prev list
		cp 255
		call nz,MoveSpriteToTop                 ; Sprite found in cache, since the value is not 255
							; move to the top of the list and draw
							; we have the cache entry in (LRU_first)
							
		call z,InsertSpriteInCache		; Sprite not found in cache, rotate and move to the top of the list

		; we have the cache entry in
		; Now draw the sprite
        ; First, calculate the target position: SprCacheData+96*LRU_first

        ld a, (LRU_first)
		add a, a
        ld hl, Multiply_by_96+1	;Note Multiply_by_96 is aligned to be on the same high Byte so we only operate with L and also we can do inc L instead inc HL
		add a, l
		ld l, a			; HL points to the value in the array + 1, to the high byte
		ld a, (hl)		
		add a, high SprCacheData	
		dec l
		ld l, (hl)
		ld h, a		; HL = Sprite address , = SprCacheData + 96 * LRU_last  <- the place in the sprite cache to get the rotated sprite from. 
	
        pop bc                  ; get X and Y back!
        ; now calculate screen addresses and stuff, then paint

        ld a, c			; 4
		and $07			; 7  <-the 3 lowest bits are the line within a char
		ld d,a			; 4
		ld a,c			; 4  <- the top 2 bits are the screen third
		rra			    ; 4
		rra			    ; 4
		rra			    ; 4
		and $18			; 7
		or d			; 4
		or $C0			; 7	<- If the start address is 16384, this should be $40
		ld d,a			; 4 (total 53 t-states) D has the high byte of the address 
		
		ld a,b			;4
		rra			    ;4
		rra			    ;4
		rra			    ;4
		and $1f			;7  <- the top 5 bits are the char pos. The low 3 bits are the pixel pos
		ld e,a			;4
		ld a,c			;4
		rla			    ;4
		rla		     	;4
		and $e0			;7
		or e			;4
		ld e,a			;4 (total 54 t-states) E has the low byte of the address
		
		ld bc, $1020		; B= 16 para el bucle C=32 para sumar linea
		;HL = Sprite address , DE = Screen address
lineloop:
        ; NOTE: the byte order in the cached sprite entry is a bit different
        ; We have one byte of mask, then one byte of gfx. This was swapped when
        ; storing in the cache, so keep this in mind if you want to reuse this
        ; routine without the cache
		ld a, (de)			;get what on Screen
		and (hl)			;AND with the Sprite Mask
		inc hl				
		or (hl)				;OR with the Sprite
		inc hl
		ld (de), a 			;store on Screen
		inc e				;next char on Screen

		ld a, (de)			;get what on Screen
		and (hl)			;AND with the Sprite Mask
		inc hl				
		or (hl)				;OR with the Sprite
		inc hl
		ld (de), a 			;store on Screen
		inc e				;next char on Screen

		ld a, (de)			;get what on Screen
		and (hl)			;AND with the Sprite Mask
		inc hl				
		or (hl)				;OR with the Sprite
		inc hl
		ld (de), a 			;store on Screen

		dec e
		dec e				; return to original Column
		inc d				; next line

		ld a, d
		and 7
		jr z, draw_a2		; if the low 3 bits of B are zero, we are changing to a new char line
		djnz lineloop
		ret

draw_a2:
		ld a, e
		add a, c			; we add +32 for the next line
		ld e, a
		jr c, draw_a1		; and C + 32 overflows 
		ld a, d
		sub 8			    ; then we go to the next third of the screen
		ld d, a
draw_a1:
		djnz lineloop
		ret

; Insert sprite in cache. This means
;  1. Allocate cache entry for the combination
;  2. Rotate sprite and move to the appropriate place in memory
;
;  Input: DE: sprnum * 8
;	  IXl: rotation
;
;  Output: IX: pointer to the sprite, already rotated

InsertSpriteInCache:
			push hl
			ld a, (LRU_last)
			add a, a		; MappingTable + 2*LRU_last
			ld hl, MappingTable+1	;we add 1 to point to the high byte of MappingTable
			add a,l
;			inc a			
			ld l, a			; HL points to the high byte of the current entry
			ld a, (hl)
			and a			; If a==0, this entry was unused
			jr z, insert_unused_entry
							; The entry is used, so we need to clean up
			ld b, a
			dec l			; mapping table is aligned on high byte 
			ld c, (hl)		; BC points to the sprnum | rotation entry. It should be LRU_last now, we will reset to 255
			ld a, 255
			ld (bc), a

insert_unused_entry:

			ld a, e
			or ixl
			ld c, a			; DC = sprnum <<3 | rotation
			ld a, d
			add a, high SprCacheTable
			ld b, a			; BC has now the address

			ld a, (LRU_last)
			add a, a		; MappingTable + 2*LRU_last
			ld hl, MappingTable
			add a,l
			ld l, a
			ld (hl), c
			inc l			; mapping table is aligned
			ld (hl), b		; store the address back
			pop hl

            ; ld h, LRU_prev / 256   <- this is already true when entering
			ld a, (LRU_last)
			ld l, a
			ld a, (hl)		; A == LRU_newlast,  LRU_newlast = LRU_prev[LRU_last];
			ld (hl), LRU_LASTENTRY  ;  LRU_prev[LRU_last] = LRU_LASTENTRY;
			ld l, LRU_LASTENTRY
			ld (hl), a		;  LRU_prev[LRU_LASTENTRY] = LRU_newlast;

			ex af, af'			
			ld a, (LRU_first)
			ld l, a
			ld a, (LRU_last)			
			ld c, a
			ld (hl), c		; LRU_prev[LRU_first] = LRU_last;
			inc h			; pointer to the LRU_next list, clear carry flag
			ld l, c			; c == LRU_last
			ld a, (LRU_first)
			ld b, a
			ex af, af'

			ld (hl), b		; LRU_next[LRU_last] = LRU_first;
			ld l, a
			ld (hl), LRU_LASTENTRY  ; LRU_next[LRU_newlast] = LRU_LASTENTRY;
			ld l, LRU_LASTENTRY
			ld (hl), c		; LRU_next[LRU_LASTENTRY] = LRU_last;

			ex af, af'		; use alternate A
			ld a, ixl		; A' = rotation
			or e
			ld l, a			; DL = sprnum <<3 | rotation
			ld a, d
			add a, high SprCacheTable
			ld h, a			; HL has now the address SprCacheTable[value]
			ex af, af'		; normal A again
			ld (hl), c		; SprCacheTable[value]=LRU_last

			ld b, a			; save LRU_newlast			
			ld a, (LRU_last)
			ld (LRU_first), a		; LRU_first = LRU_last;
			ld a, b		
			ld (LRU_last),a			;  LRU_last = LRU_newlast, A is still LRU_newlast

			; Now we should rotate the sprite and really write it there
			; First, calculate the target position: SprCacheData+96*SprCacheTable[value]
			; C is LRU_last == SprCacheTable[value]
			
			ld hl, Multiply_by_96+1	;Note Multiply_by_96 is aligned to be on the same high Byte so we only operate with L and also we can do inc L instead inc HL
			ld a, c	
			add a, a		;LRU_LAST * 2
			add a, l		;
			ld l, a			;HL points to the high byte value in the array 

			ld a, (hl)		; AC = 96 * LRU_last
			add a, high SprCacheData
			dec l
			ld l, (hl)
			ld h, a			; HL = SprCacheData + 96 * LRU_last  <- the place in the sprite cache to store the rotated sprite

			
			; The target position is HL, now rotate!
			ld (SCRADD_rotate),HL		; save the target address for cache in SCRADD		

			ld hl, $C000    ; $C000 is the address of the first sprite
			and a			; clear carry flag
			rl e
			rl d
			rl e
			rl d
			rl e
			rl d			; DE = sprnum *64
			add hl, de		; HL = first position for the sprite
            ; Move to the sprite RAM bank
            ld b, 1
            call setrambank_with_di

			ld a, 16		; 16-line sprite
			ld (LINECOUNT), a
			jp insert_lineloop+3		; as we already have HL we jump
			;	note: sprites are 64 bytes each, so as they are aligned on source memory we can use inc L instead of inc HL
			;	take care if change the sprites size ....
insert_lineloop:
			ld hl, 0		; this source address will be modified later
			ld e, (hl)		;mask 1
			inc l			
			ld d, (hl)		;mask 2
			inc l
			ld c, (hl)		;sprite 1
			inc l
			ld b, (hl)		;sprite 2
			inc l
			ld (insert_lineloop+1), hl		;update source address for the next loop

			ld a, $ff		; a will be shifted to the mask. 1 means transparent
			scf			; transparent

			ex af, af'		; a' will be used for the bit rotating loop
			ld a, ixl
			or a
			jr z, insert_skiprotate	; if no rotation is needed, skip this

			ld l,a			; l= loop counter
			xor a			; clear carry flag, clear a',since if will be shifted to the image

insert_rotateloop:	
			ex af, af'		; a ==mask
			rr e
			rr d
			rra
			ex af, af'		; a== sprite data
			rr c
			rr b
			rra	
			dec l
			jp nz, 	insert_rotateloop		; at the end, we have DEa with the rotated mask, BCa' with the rotated sprite

SCRADD_rotate: EQU $+1
insert_skiprotate:	ld hl, 0	; get store cache address in HL
			ld (hl), e		; mask byte 1
			inc hl
			ld (hl), c		; byte sprite 1
			inc hl
			ld (hl), d		; mask byte 2
			inc hl
			ld (hl), b		; byte sprite 2
			inc hl
			ex af, af'
			ld (hl), a		; write A' (last byte of mask) in cache
			inc hl
			ex af, af'
			ld (hl), a		; write A (last byte of sprite) in cache
			inc hl

			ld (SCRADD_rotate),hl		; store the cache write address again
			ld hl, LINECOUNT	
			dec (hl)
			jp nz, insert_lineloop		; go to next line
			
            ; Get back to the normal RAM bank
            ld a, (current_screen_bank)
            ld b, a
            call setrambank_with_di
			ret

; Move sprite to top of the cache. 
;  Input: A: entry to move to the top of the cache
;
MoveSpriteToTop:
        ld e, a
        ld a, (LRU_first)
        cp e
        jr nz, checklast
        inc a                  ; sets flag to not zero
        ret           
checklast:
        ld a, (LRU_last)
        cp e
        jr nz, moveactually    
        ; If we are moving the last entry to the top of the cache
        ; we need to adjust LRU_last, or we will screw the cache
movinglast:
        ld l, a
        ld a, (hl)              ; A == LRU_prev[LRU_last], or the new LRU_last
        ld (LRU_last),a         ; and now, continue with the movement
moveactually:
        ld a, e
		ld l, a			; A ==entry
		ld c, (hl)		; C == prev = LRU_prev[entry];
		ld (hl), LRU_LASTENTRY  ; LRU_prev[entry] = LRU_LASTENTRY; 
		ld a, (LRU_first)
		ld l, a
		ld (hl), e		;   LRU_prev[LRU_first] = entry;  50
	
		inc h			; pointer to the LRU_next list
		ld l, e			
		ld b, (hl)		; B== next = LRU_next[entry];
		ld (hl),a		; LRU_next[entry] = LRU_first;
		ld l, LRU_LASTENTRY
		ld (hl), e		; LRU_next[LRU_LASTENTRY] = entry;
		ld l, c
		ld (hl), b		; LRU_next[prev] = next; 54

		dec h			; pointer to the LRU_prev list, flags not zero
		ld l,b
		ld (hl),c		;   LRU_prev[next] = prev; 
	
		ld a, e
		ld (LRU_first),a	;    LRU_first = entry;
 		ret			; Total: 143 T-states for a cache hit	 

; Initialize sprite cache list
; No entry, no output
; Modifies: BC, DE, HL, A

InitSprCacheList:
                 ; First, initialize the Sprite Cache Table with 255
                 ld hl, SprCacheTable
                 ld de, SprCacheTable+1
                 ld (hl),255
                 ld bc, 1023
                 ldir
        		 ; Initialize the mapping table with zeroes
                 ld hl, MappingTable
                 ld de, MappingTable+1
                 ld (hl),0
                 ld bc, 85
                 ldir
                 ; Second, pre-populate the LRU_next and LRU_prev arrays
                 ;unsigned char LRU_next[43]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,0};
                 ;unsigned char LRU_prev[43]={42,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41};
                 ld hl, LRU_prev+1
                 ld de, LRU_next
                 ld b, 42
                 ld a, 1
                 ld c, 0

loop_InitSprCache:
                  ld (hl), c
                  ld (de), a
                  inc hl
                  inc de
                  inc a
                  inc c
                  djnz loop_InitSprCache
                  ; The final touches
                  dec a                 ; A is already 43, so decrement to make it 42
                  ld (LRU_prev), a
                  xor a
                  ld (de), a
                  ; Finally, set LRU_first and LRU_last to their proper values
                  ld (LRU_first), a
                  ld a, 41
                  ld (LRU_last), a
                  ret



; Definitions for sprite cache addresses

SprCacheData    EQU $B000       ; sprite cache data, 4K
SprCacheTable 	EQU $9C00		; sprite cache table, 1K
MappingTable    EQU $9B80       ; mapping from cache entries to the sprnum | rotation used. 86 bytes (some bytes wasted)
LRU_next      	EQU $9B00		; cache list next pointers, 43 bytes used (some bytes wasted!)
LRU_prev      	EQU $9A00		; cache list prev pointers, 43 bytes used (some bytes wasted!)
LRU_first	db 0
LRU_last	db 41			; pointers to the first and last entry in the cache
LINECOUNT       db 0
LRU_LASTENTRY   EQU 42
