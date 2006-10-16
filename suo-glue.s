	.section	".text"
	.align 2
	.globl gc_glue

/* GC_GLUE - invoke the GC

   This function is invoked when r16, the pointer to free memory, is
   advanced past r17, the pointer to the end of the current semi-space.
   At that point r4 points to the start of the object that failed to
   be allocated.  Thus, r16-r4 is the number of bytes that must be
   free after the GC, at least.

   When GC_GLUE returns, the allocation must have succeeded.

   Steps:
    - save LR,r4,r15,r16,r17
    - invoke gc
    - restore LR,r4,r15,r16,r17
    - blr
 */
    
gc_glue:
    lis  3,gc_buf@ha
    la   3,gc_buf@l(3)
    mflr 0
    stw  0,0(3)
    stw  4,4(3)
    stw  15,8(3)
    stw  16,12(3)
    stw  17,16(3)
    bl   gc
    lwz  0,0(3)
    lwz  4,4(3)
    lwz  15,8(3)
    lwz  16,12(3)
    lwz  17,16(3)
    mtlr 0
    blr
    
    .comm gc_buf,20,4

/* ADJUST_CALL_SIG - converts between the way a function was
   called and the way it expects its arguments.

   The call signature is in reg[0] (call_sig), and the real arguments
   are in reg[1], reg[2], ...  The function signature is in reg[-5]
   (func_sig).  This function is only called when call_sig is not
   equal to func_sig.

   The approach is to first make sure that there are the right number
   of fixed arguments, if possible, and then to take care of the rest
   argument.
*/
    
	.section	".text"
	.align 2
	.globl adjust_call_sig

adjust_call_sig:
    /* if call_sig.n < func_sig.n: take_one_down */
    /* if call_sig.n > func_sig.n: push_one up */
    /* else check_restp */
    lwz  	3,0(14)
    rlwinm	3,3,0,0,28
    lwz  	4,-20(14)
    rlwinm	4,4,0,0,28
    cmpw 	3,4
    blt         take_one_down
    cmpw 	3,4
    bgt		push_one_up

check_restp:
    /* Now we have the right number of non-rest args.  If the func
       requires one but the caller didn't provide it, we create a rest
       arg with '().  If the func doesn't take a rest arg but the
       caller provided one, it must be '().
    */
    lwz  	4,-20(14)
    andi.	0,4,4
    bne         require_rest_arg
must_have_no_rest_arg:	
    lwz  	3,0(14)
    andi.	0,3,4
    beq		done
    rlwinm  	3,3,31,1,29
    add         3,3,14
    lwz         0,4(3)
    andi.	0,0,3
    beq		wrong_num_args
require_rest_arg:
    lwz  	3,0(14)
    andi.	0,3,4
    bne		done
    rlwinm  	4,4,31,1,29
    add         4,4,14
    li		0,2
    stw         0,4(4)
done:
    /* Everything allright, return to the real function.
    */
    blr
    
take_one_down:
    /* Check that call_sig.restp is true.  If not, we don't have
       enough arguments.
    */
    lwz  	3,0(14)
    andi.	0,3,4
    beq         wrong_num_args
    /* Check that reg[call_sig.n+1] is a non-immediate.  we assume
       that it is a pair if it is non-immediate.
    */
    rlwinm  	3,3,31,1,29
    add         3,3,14
    lwz         0,4(3)
    andi.       0,0,3
    bne         wrong_num_args
    /* Copy the cdr of reg[call_sig.n+1] to reg[call_sig.n+2] and the
       car to reg[call_sig.n+1].
    */
    lwz         4,4(3)
    lwz         0,4(4)
    stw         0,8(3)
    lwz         0,0(4)
    stw         0,4(3)
    /* Increase call_sig.n and try again.
    */
    lwz  	3,0(14)
    addi	3,3,8
    stw  	3,0(14)
    b		adjust_call_sig
    
push_one_up:
    /* Check that func_sig.restp is true.  Otherwise there is no point
       in creating a rest arg.
    */
    lwz  	4,-20(14)
    andi.	0,4,4
    beq         wrong_num_args

    /* Create the rest arg if necessary
    */
    lwz  	3,0(14)
    andi.	0,3,4
    bne		have_rest_arg
    ori		3,3,4
    stw  	3,0(14)
    rlwinm  	3,3,31,1,29
    add		3,3,14
    li		0,2
    stw		0,4(3)
have_rest_arg:
    /* Allocate a new pair into r4
    */
    mr		4,16
    addi	16,16,8
    cmpw	16,17
    blt		have_pair
    lis  	3,gc_buf@ha
    la   	3,gc_buf@l(3)
    mflr 	0
    stw  	0,0(3)
    stw  	4,4(3)
    stw  	15,8(3)
    stw  	16,12(3)
    stw  	17,16(3)
    bl   	gc
    lwz  	0,0(3)
    lwz  	4,4(3)
    lwz  	15,8(3)
    lwz  	16,12(3)
    lwz  	17,16(3)
    mtlr 	0
have_pair:  
    /* Store old rest arg in cdr of pair and the last fixed arg into
       the car, then store the pair as the last fixed arg and decrease
       call_sig.n.
    */
    lwz  	3,0(14)
    rlwinm  	3,3,31,1,29
    add		3,3,14
    lwz		0,4(3)
    stw		0,4(4)
    lwz		0,0(3)
    stw		0,0(4)
    stw         4,0(3)
    lwz  	3,0(14)
    subi	3,3,8
    stw  	3,0(14)
    b		adjust_call_sig
    
wrong_num_args:
    /* Check that regs[-6] is a non-immediate.  If it isn't, do a
       sys:panic call.
    */
    lwz		3,-24(14)
    andi.	0,3,3
    beq		have_wrong_num_args_closure
    lwz		3,-4(14)
    mtctr	3
    li		3,0
    bctr
have_wrong_num_args_closure:	
    /* We pass two args, the closure and continuation.  The closure
       comes from reg[-6], but we don't touch the continuation.
    */
    li         	3,(2<<3)|1
    stw         3,0(14)
    li		3,18
    lwz		3,-24(14)
    stw         3,4(14)
    lwz         3,4(3)
    mr 		15,3
    addi 	3,3,4
    mtctr 	3
    bctr
