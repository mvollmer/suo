	.file	"suo-glue.c"
	.section	".text"
	.align 2
	.globl gc_glue
	.type	gc_glue, @function

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
    - restore LR,r4,r15,r16
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
