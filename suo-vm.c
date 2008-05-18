
/* suo-vm -- virtual machine
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>

#define DEBUG 0

int trace_gc       = 0;
int trace_syscalls = 0;
int trace_insns    = 0;
int trace_go       = 0;
int trace_source   = 0;

void
breakpoint ()
{
}

void
perror_exit (const char *msg)
{
  perror (msg);
  exit (1);
}

void
error_exit (const char *msg)
{
  fputs (msg, stderr);
  fputs ("\n", stderr);
  exit (1);
}

/* Representation of objects
 */

typedef unsigned int word;
typedef   signed int sword;
typedef         word val;

#define MAKE_IMM(i)        (((i)<<3)+2)

#define NIL                MAKE_IMM(0)
#define BOOL_T             MAKE_IMM(1)
#define BOOL_F             MAKE_IMM(2)
#define UNSPEC             MAKE_IMM(3)

#define MIN_FIXNUM_VAL     -536870912
#define MAX_FIXNUM_VAL      536870911

#define FIXNUM_P(v)        (((v)&3)==1)
#define FIXNUM_VAL(v)      (((sword)(v))>>2)
#define MAKE_FIXNUM(n)     (((word)(n))<<2|1)

#define CHAR_P(v)          (((v)&7)==6)
#define CHAR_VAL(v)        (((word)(v))>>3)
#define MAKE_CHAR(n)       (((word)(n))<<3|6)

#define HEAP_P(v)          (((v)&3)==0)
#define FIELD_LOC(o,i)     (((val *)(o))+i)
#define FIELD_REF(o,i)     (((val *)(o))[i])
#define UNCHECKED_FIELD_SET(o,i,v)   (((val *)(o))[i]=(v))

#if 1
#define FIELD_SET(o,i,v)   UNCHECKED_FIELD_SET(o,i,v)
#else
#define FIELD_SET(o,i,v)   checking_field_set ((val)o, i, v)
#endif

#define HEADER_PAIR_P(v)   (((v)&3)!=3)

#define PAIR_P(v)          (HEAP_P(v) && HEADER_PAIR_P(FIELD_REF(v,0)))
#define CAR(p)             FIELD_REF(p,0)
#define CDR(p)             FIELD_REF(p,1)
#define CDR_LOC(p)         FIELD_LOC(p,1)
#define SET_CAR(p,v)       UNCHECKED_FIELD_SET(p,0,v)
#define SET_CDR(p,v)       FIELD_SET(p,1,v)

#define HEADER_RECORD_P(v)       (((v)&0x80000003)==0x00000003)
#define HEADER_RECORD_DESC(v)    ((v)&~3)
#define MAKE_HEADER_RECORD(desc) ((desc)|3)

#define RECORD_P(v)        (HEAP_P(v) && HEADER_RECORD_P(FIELD_REF(v,0)))
#define RECORD_DESC(v)     (HEADER_RECORD_DESC(FIELD_REF(v,0)))
#define RECORD_LENGTH(v)   FIXNUM_VAL(RECORD_REF(RECORD_DESC(v),0))
#define RECORD_LOC(v,i)    FIELD_LOC(v,(i)+1)
#define RECORD_REF(v,i)    FIELD_REF(v,(i)+1)
#define RECORD_SET(v,i,e)  FIELD_SET(v,(i)+1,e)

#define HEADER_VECTOR_P(v)      (((v)&0x8000000F)==0x80000003)
#define HEADER_VECTOR_LENGTH(v) (((v)&~0x80000000)>>4)

#define VECTOR_P(v)        (HEAP_P(v) && HEADER_VECTOR_P(FIELD_REF(v,0)))
#define VECTOR_LENGTH(v)   (HEADER_VECTOR_LENGTH(FIELD_REF(v,0)))
#define VECTOR_LOC(v,i)    FIELD_LOC(v,(i)+1)
#define VECTOR_REF(v,i)    FIELD_REF(v,(i)+1)
#define VECTOR_SET(v,i,e)  FIELD_SET(v,(i)+1,e)

#define HEADER_BYTEVEC_P(v)      (((v)&0x8000000F)==0x8000000B)
#define HEADER_BYTEVEC_LENGTH(v) (((v)&~0x80000000)>>4)

#define BYTEVEC_P(v)       (HEAP_P(v) && HEADER_BYTEVEC_P(FIELD_REF(v,0)))
#define BYTEVEC_LENGTH(v)  (HEADER_BYTEVEC_LENGTH(FIELD_REF(v,0)))
#define BYTEVEC_BYTES(v)   ((char *)FIELD_LOC(v,1))

#define HEADER_CODE_P(v)           (((v)&0x8000000F)==0x8000000F)
#define HEADER_CODE_INSN_LENGTH(v) (((v)>>12)&0x7FFFF)
#define HEADER_CODE_LIT_LENGTH(v)  (((v)>>4)&0xFF)

#define CODE_P(v)           (HEAP_P(v) && HEADER_CODE_P(FIELD_REF(v,0)))
#define CODE_INSN_LENGTH(v) (HEADER_CODE_INSN_LENGTH(FIELD_REF(v,0)))
#define CODE_LIT_LENGTH(v)  (HEADER_CODE_LIT_LENGTH(FIELD_REF(v,0)))


void
checking_field_set (val obj, int idx, val v)
{
  int start, len;

  if (PAIR_P (obj))
    {
      start = 0;
      len = 2;
    }
  else if (VECTOR_P (obj))
    {
      start = 1;
      len = VECTOR_LENGTH (obj);
    }
  else if (RECORD_P (obj))
    {
      start = 1;
      len = RECORD_LENGTH (obj);
    }
  else if (CODE_P (obj))
    {
      start = 1 + CODE_INSN_LENGTH (obj);
      len = CODE_LIT_LENGTH (obj);
    }
  else
    {
      fprintf (stderr, "FIELD_SET on non fielded object\n");
      abort ();
    }

  if (idx >= start && idx < start + len)
    (((val *)(obj))[idx]=(v));
  else
    {
      fprintf (stderr, "FIELD_SET %d out of bounds %d %d\n", idx, start, len);
      abort ();
    }
}

#define HEAP_SIZE  (32*1024*1024)
#define SPACE_SIZE (HEAP_SIZE/2)

val *spaces[2];
int active_space;

void
init_heap ()
{
  // spaces[0] = malloc (HEAP_SIZE * sizeof(val));

  spaces[0] = mmap ((void *)0x10000000, HEAP_SIZE * sizeof(val),
		    PROT_READ | PROT_WRITE,
		    MAP_PRIVATE | MAP_ANON,
		    0, 0);
  
  if (((val)spaces[0]) >= 0x80000000)
    perror_exit ("above 2 GiB");
  if (spaces[0] == NULL)
    perror_exit ("malloc");
  spaces[1] = spaces[0] + SPACE_SIZE;
  active_space = 0;

  if (trace_gc)
    fprintf (stderr, "heap: %p %p %p\n",
	     spaces[0], spaces[1], spaces[1] + SPACE_SIZE);
}


#define UNSWIZZLE(p,b)   (((word)p)+((word)b))

/* Correct all pointers in MEM from offsets into proper pointers by
   adding OFF to them.
*/
void
unswizzle_objects (val *mem, size_t off, word n)
{
  int count = 0;
  int n_pairs = 0, n_vectors = 0, n_records = 0;
  int n_bytevecs = 0, n_codes = 0, n_odds = 0;

  val *ptr;
  for (ptr = mem; ptr < mem + n;)
    {
      sword size;
      char *type;
      val header = *ptr;

#if DEBUG      
      fprintf (stderr, "on %d %08x\n", ptr-mem, header);
#endif

      count++;

      if (HEADER_PAIR_P (header))
	{
	  type = "pair";
	  n_pairs++;
	  size = 2;
	}
      else if (HEADER_RECORD_P (header))
	{
	  *ptr = UNSWIZZLE (*ptr, off);
	  val desc = HEADER_RECORD_DESC (*ptr);
	  type = "record";
	  n_records++;
	  size = FIXNUM_VAL(RECORD_REF(desc,0));
	  ptr++;
	}
      else if (HEADER_VECTOR_P (header))
	{
	  type = "vector";
	  n_vectors++;
	  size = HEADER_VECTOR_LENGTH (header);
	  ptr++;
	}
      else if (HEADER_BYTEVEC_P (header))
	{
	  type = "bytevec";
	  n_bytevecs++;
	  size = - ((HEADER_BYTEVEC_LENGTH(header) + 3) / 4);
	  ptr++;
	}
      else if (HEADER_CODE_P (header))
	{
	  type = "code";
	  n_codes++;
	  size = HEADER_CODE_LIT_LENGTH(header);
	  ptr += HEADER_CODE_INSN_LENGTH(header) + 1;
	}
      else
	abort ();

#if DEBUG
      fprintf (stderr, " %s, %d words\n", type, size);
#endif

      if (size > 0)
	{
	  if (size % 2)
	    n_odds++;

	  while (size > 0)
	    {
	      val v = *ptr;
	      if (HEAP_P(v))
		*ptr = UNSWIZZLE (v, off);
	      ptr++;
	      size--;
	    }
	}
      else
	ptr += -size;
    }

  if (trace_gc)
    {
      fprintf (stderr, "booting with %d objects, %d words.\n",
	       count, ptr - mem);
      fprintf (stderr, "(%g words per object average)\n",
	       ((double)(ptr - mem)) / count);
      fprintf (stderr, "(%d pairs, %d vectors, %d records,\n",
	       n_pairs, n_vectors, n_records);
      fprintf (stderr, " %d bytevecs, %d codes, %d odd sized)\n",
	       n_bytevecs, n_codes, n_odds);
    }
}

/* The machine.

   There are 256 general registers that hold valid suo values at all
   times.

   There are also a number of special registers that are used in
   special ways by certain instructions.  They don't generally hold
   valid suo values.  They are described together with the
   instructions that use them.

   Instructions consist of four bytes, called OP, X, Y, and Z.  OP
   determines the kind of instruction and X, Y, and Z are its
   parameters.  They might contain a register number, or a small
   immediate constant, for example.  Sometimes, Y and Z are combined
   into a 16-bit quatity, which is called YZ.  Some instructions also
   deal with the 24-bit quantity XYZ.

   The instructions have a 'riscy' feeling to it: many of them operate
   on two registers and store the result in a third.  But in addition
   to registers, these instructions can also access literals in the
   current code object as the source operands.  Because of the uniform
   encoding, this means that there can be at most 256 literals.

   For example, the ADD16 instruction exists in the variants ADD16RR,
   ADD16RL, ADD16LR, and ADD16LL.  (Of course, it is not very useful
   to have an instruction that adds two literals, the compiler should
   perform constant-folding on them.  But the machine offers it anyway
   for completeness and so that the compiler can be very simple,
   regular, and still correct.)

   Most instructions either operate on registers and literals, or do a
   single reference to the heap.  This is done to keep the illusion
   that this is a machine that could be implemented in hardware using
   a standard RISC pipeline.  However, some instructions contain a
   very simple implicit loop (which could be executed by the
   load/store unit), and some are so complicated that they would be
   implemented as traps in a real system.

   There are instructions that know about the tagging scheme.  For
   example, INIT_LENGTH knows how the header word of vectors, etc is
   constructed.


   In the instruction descriptions, reg[i] refers to general register
   I, and lit[I] refers to literal number I in the code object pointed
   to by CODE.  "reglit[I]" is a shorthand to say that either a
   register or a literal can be used.

   Unless explicitly mentioned, instructions don't do type checking,
   so if you try to GO to a fixnum, the machine will attempt to do so
   and will likely crash.

   When a instruction is described with three arguments, each of them
   is 8 bit wide.  When it has two parameters, the first is 8 and the
   second 16 bit wide.  A single parameter is 24 bit wide.

   ** Simple instructions

   - HALT XYZ

   Halt execution.  XYZ can be used to indicate the reason for
   halting.

   - GO C                  == MISC 0 C MISCOP_GO

   code = reglit[C]
   pc = first instruction of code
   lit = first literal of code

   - MOVE D S Z

   reg[D] = reglit[S].

   - MOVEI D YZ

   reg[D] = YZ

   - REF D S I

   reg[D] = reglit[S][FIXNUM_VAL(reglit[I])+1]

   (Note the "+ 1" in the index calculation.)

   - REFI D S I

   reg[D] = reglit[S][I]

   - SET D S I

   reg[D][FIXNUM_VAL(reglit[I])+1] = reglit[S]

   - SETI D S I

   reg[D][I] = reglit[S]

   - SETL D S I

   lit[D][FIXNUM_VAL(reglit[I])+1] = reglit[S]

   - ALLOC D S                == MISC D S MISCOP_ALLOC

   count = FIXNUM_VAL (reglit[S]) + 1
   reg[D] = free
   dst = free
   free += count

   - ALLOCI D I

   count = I
   reg[D] = free
   dst = free
   free += count

   - INIT S                   == MISC 0 S MISCOP_INIT

   dst[0] = reglit[S]
   dst += 1
   count -= 1

   - INITI I

   dst[0] = I
   dst += 1
   count -= 1

   - INIT_REC S               == MISC 0 S MISCOP_INIT_REC

   dst[0] = reglit[S] | 3
   dst += 1
   count -= 1

   - INIT_VEC T S             == MISC T S MISCOP_INIT_VEC

   dst[0] = (FIXNUM_VAL(reglit[S]) << 4) | 0x80000000 | T
   dst += 1
   count -= 1

   - INIT_VECI L

   dst[0] = 0x80000000 | L;
   dst += 1
   count -= 1

   - FILL S                   == MISC 0 S MISCOP_FILL

   f = reglit[S]
   while (count > 0)
     {
       dst[0] = f
       dst += 1
       count -= 1
     }

   - FILLI I

   f = I
   while (count > 0)
     {
       dst[0] = f
       dst += 1
       count -= 1
     }

   - COPY                     == MISC 0 0 MISCOP_COPY

   while (count > 0)
     {
       dst[0] = src[0]
       dst += 1
       src += 1
       count -= 1
     }

   - LOAD_DESC D S            == MISC D S MISCOP_LOAD_DESC

   reg[D] = reglit[S][0] & ~3

   - LOAD_LENGTH D S          == MISC D S MISCOP_LOAD_LENGTH

   reg[D] = MAKE_FIXNUM (reglit[S][0] & ~0x8000000F)


   ** Testing and branching

   - CMP T A B

   count = (T is true about A and B)

   T can be
   
      CMPOP_EQ:  A == B

   - TEST_REC S               == MISC 0 S MISCOP_TEST_REC

   if (HEAP_P (reglit[S]))
     {
       dst = reglit[S][0];
       count = (dst & 0x80000003) == 3;
     }
   else
     count = 0;

   - TEST_VEC S               == MISC T S MISCOP_TEST_VEC

   if (HEAP_P (reglit[S]))
     {
       dst = reglit[S][0];
       count = (dst & 0x8000000F) == 0x80000000 | T;
     }
   else
     count = 0;

   - TEST_DESC S              == MISC 0 S MISCOP_TEST_DESC

   if (count && dst == reglit[S] | 3):
     count = 1
   else
     count = 0

   - TEST_PAIR S              == MISC 0 S MISCOP_TEST_PAIR

   if (HEAP_P (reglit[S]))
     {
       dst = reglit[S][0];
       count = HEADER_PAIR_P (dst);
     }
   else
     count = 0

   - BRANCH C O

   If condition C is true, pc += O.

   Conditions are
   
     XXX

   ** Trapping

   A TRAP instruction is an alternate way to invoke a procedure.  (The
   usual way is to use a GO instruction.)  Trapping is considerably
   more expensive than going, and you need to prepare the procedures
   that a trap can invoke in a special.  On the plus side, a trap can
   be inserted into the middle of a instruction stream: you don't need
   to construct a continuation and all the register values are intact
   when the trap returns.  Thus, traps are useful when the procedure
   is normally not called, such as when signalling errors or when a
   fixnum overflows into a bignum.

   You can think of traps as 256 special instructions that are
   implemented as Suo procedures.

   The arguments for the invoked procedure are taken from the current
   registers or literals.  The result (only one) is placed in a
   register.

   - TRAP R A T

   Invoke trap number T.  The result is placed in register R.  A
   describes the arguments.  It is of the form

      N*16 + RL3*8 + RL2*4 + RL1*2 + RL0

   where 0 <= N <= 4 is the number of arguments, and RLi is 0 when
   that argument is in a register, and 1 when it is a literal.  The
   trap instruction is always followed by a second word that contains
   the four argument indices, one per byte.

   Some traps have special significance:

   Trap number 0 is a syscall and implemented by the runtime.  It
   performs functions such as I/0 and things related to the GC.
   Setting a handler for trap 0 has no effect.

   Trap number 1 is used for interrupts.  It is invoked automatically
   by the runtime when an interrupt is received.

   Trap number 2 is the 'wrong number of arguments' trap.  It is
   invoked by the CHECK_CALLSIG instruction.

   The handler for trap T is found by looking at index T of the 'trap
   vector'.  Special register -9 points to this vector.  The handler
   should be a closure record.  The corresponding procedure receives
   the 'trap context' as its first argument, followed by the arguments
   indicated in the trap instruction.  The trap context object should
   be used with the TRAPRET instruction.  The handler is invoked with
   an unusable continuation.  I.e., it must not return.

   - TRAPRET S V

   Return from a trap.  S is the state passed to the handler, and V is
   the value that should be placed into the return register.

   ** Complex instructions

   These instructions do complicated things by conditionally branching
   out to subroutines.  You can imagine the subroutines being
   implemented somewhere in ROM, using an instruction set from above
   plus whatever is needed in addition.

   - SYSCALL N V            == TRAP2 V N TRAPOP_SYSCALL

   Trap into the operation system to perform a syscall.  The next N
   words of the instruction stream are used as parameters for the
   syscall.  Each word is either of the form

       R    or   0x80000000 + L

   where R means that reg[R] is to be used, and the second form refers
   to lit[L].

   The result is delivered in register V.

   - CHECK_ALLOC S            == TRAP2 0 S TRAPOP_CHECK_ALLOC
   - CHECK_ALLOCI S

   If FREE + S >= END, run the garbage collector.  If there still
   isn't enough free space afterwards, halt the machine.

   - CHECK_CALLSIG C S        == TRAP2 C S TRAPOP_CHECK_CALLSIG

   reg[C] is the signature of the parameters in the general registers
   and S is the signature that is expected.  If reg[C] and S are not
   equal, a 'signature adjustement routine' is executed.

   The adjustement subroutine knows about the calling convention and
   will reshuffle the general registers to make the two signatures
   equal if possible.  This might run the GC.  When it is not possible
   to reshuffle the registers, a GO to WRONG_CALLSIG is performed, if
   that register points to the heap.  If it doesn't, a HALT with FF,
   reg[C], and S is performed instead.

   A 'call signature' is a integer that encodes the number of
   parameters and whether or not there is a 'rest' parameter.  It is
   computed as
   
       2*n + r

   where N is the number of parameters, excluding any rest parameter
   if there is one, and R is 0 when there is no rest parameter and 1
   when there is.

   For example, a function defined as

       (define (foo a b c . rest) ...)

   has a call signature of 2*3 + 1 = 7 and expects parameters A, B, C,
   and REST in registers 1, 2, 3, and 4, respectively.  When using
   apply to call it like so

       (apply foo 1 2 '(3 4)),

   the call itself has a signature of 2*2 + 1 = 5 with reg[1] = 1,
   reg[2] = 2, and reg[3] = '(3 4).  The adjustement subroutine will
   reshuffle the register by moving the cdr of reg[3] into reg[4] and
   the car of reg[3] into reg[3].  If the call would be

       (foo 1 2 3 4),

   its call signature would be 2*5 + 0 = 10, with reg[1] = 1, reg[2] =
   2, reg[3] = 3, and reg[4] = 4.  The adjustement subroutine will
   allocate a new pair, put reg[4] into its car (and nil in its cdr)
   and will point reg[4] to it.
   

 */

val specials_and_regs[9+256];
val *regs = specials_and_regs + 9;

val reg_code;

word *reg_pc;
word *reg_lit;
word *reg_src;
word *reg_dst;
word  reg_count;
word *reg_free;
word *reg_end;

#define RR 0
#define RL 1
#define LR 2
#define LL 3

#define HALT           0
#define MISC           4
#define MOVE           8
#define REF           12
#define REFI          16
#define SET           20
#define SETL          24
#define SETI          28
#define CMP           32
#define TRAP2          36
#define REFU8         40
#define SETU8         44
#define REFU16        48
#define SETU16        52
#define ADD           56
#define SUB           60
#define MUL           64
#define DIV           68
#define REM           72
#define ADD16         76
#define SUB16         80
#define MUL16         84
#define DIV16         88
#define ADD16I        92
#define TRAPRET       96

#define MOVEI        128
#define ALLOCI       129
#define INITI        130
#define FILLI        131
#define CHECK_ALLOCI 132
#define INIT_VECI    133
#define TRAP         134
#define TRAPR        135
#define ABORT        136
#define ABORTR       137

#define BRANCH       255

#define MISCOP_GO            0
#define MISCOP_ALLOC         1
#define MISCOP_INIT          2
#define MISCOP_INIT_REC      3
#define MISCOP_INIT_VEC      4
#define MISCOP_FILL          5
#define MISCOP_COPY          6
#define MISCOP_LOAD_DESC     7
#define MISCOP_LOAD_LENGTH   8
#define MISCOP_TEST_REC      9
#define MISCOP_TEST_VEC     10
#define MISCOP_TEST_DESC    11
#define MISCOP_TEST_PAIR    12
#define MISCOP_LOAD_LENGTH_HALF 13
#define MISCOP_ALLOC_BYTES  14
#define MISCOP_TEST_CHAR    15
#define MISCOP_MAKE_CHAR    16
#define MISCOP_CHAR_VALUE   17
#define MISCOP_TEST_FIXNUM  18
#define MISCOP_SET_COUNT    19
#define MISCOP_SET_COUNT_HI 20
#define MISCOP_GET_COUNT    21
#define MISCOP_GET_COUNT_HI 22
#define MISCOP_LOAD_INSN_LENGTH 23
#define MISCOP_LOAD_LIT_LENGTH 24
#define MISCOP_ALLOC_CODE   25
#define MISCOP_INIT_CODE    26

#define CMPOP_EQ             0
#define CMPOP_FIXNUMS        1

/* XXX - These are for TRAP2, not TRAP.  Confusing.
 */
#define TRAPOP_SYSCALL       0
#define TRAPOP_CHECK_CALLSIG 1
#define TRAPOP_CHECK_ALLOC   2
#define TRAPOP_CHECK_ALLOC_BYTES 3
#define TRAPOP_CHECK_ALLOC_CODE  4

#define IF_FALSE             0
#define IF_GTE               1


#define IMM_UNSPEC 26

#define TAG_VECTOR 3

#define rlop(op) op+RR: case op+RL: case op+LR: case op+LL

void dissource (FILE *f, val code);
void discode (FILE *f, val code);

void
do_GO_with_offset (val code, int offset)
{
  if (trace_source)
    dissource (stderr, code);

  if (trace_go)
    {
      discode (stderr, code);
      fprintf (stderr, "\n");
    }

  reg_code = code;
  reg_pc = ((word *)reg_code) + offset;
  reg_lit = ((word *)reg_code) + 1 + CODE_INSN_LENGTH (reg_code);
}

void
do_GO (val code)
{
  do_GO_with_offset (code, 1);
}

void do_syscall (word n_args, word return_register);
void check_callsig (word expected_sig, int caller_sig_reg);
void gc (word size);

val *disinsn (FILE *f, val code, val *pc, int reg_hints);

void trap (int trap_number, int return_reg, int arg_head, word arg_desc);
void return_from_trap (val state, val return_value);

void
run_cpu ()
{
  while (1)
    {
      /* Fetch
       */
      word insn = *reg_pc++;

      /* Decode
       */
      word op = insn >> 24;
      word x = (insn >> 16) & 0xFF;
      word y = (insn >> 8) & 0xFF;
      word z = insn & 0xFF;
      word yz = insn & 0xFFFF;
      word xyz = insn & 0xFFFFFF;
      word *yp = (op & 2)? reg_lit : regs;
      word *zp = (op & 1)? reg_lit : regs;

      if (trace_insns)
	{
	  disinsn (stderr, reg_code, reg_pc - 1, 1);
	  fprintf (stderr, "\n");
	}

      /* Execute
       */
      switch (op)
	{
	case rlop(HALT):
	  fprintf (stderr, "HALT %02x%02x%02x\n", x, y, z);
	  exit (0);

	case rlop(MISC):
	  switch (z)
	    {
	    case MISCOP_GO:
	      do_GO (yp[y]);
	      break;

	    case MISCOP_ALLOC:
	      reg_count = FIXNUM_VAL (yp[y]) + 1;
	      regs[x] = (val) reg_free;
	      reg_dst = reg_free;
	      reg_free += reg_count;
	      break;

	    case MISCOP_INIT:
	      reg_dst[0] = yp[y];
	      reg_dst++;
	      reg_count--;
	      break;

	    case MISCOP_INIT_REC:
	      reg_dst[0] = yp[y] | 3;
	      reg_dst++;
	      reg_count--;
	      break;

	    case MISCOP_INIT_VEC:
	      reg_dst[0] = (FIXNUM_VAL (yp[y]) << 4) | 0x80000000 | x;
	      reg_dst++;
	      reg_count--;
	      break;

	    case MISCOP_FILL:
	      while (reg_count > 0)
		{
		  reg_dst[0] = yp[y];
		  reg_dst++;
		  reg_count--;
		}
	      break;

	    case MISCOP_COPY:
	      while (reg_count > 0)
		{
		  reg_dst[0] = reg_src[0];
		  reg_dst++;
		  reg_src++;
		  reg_count--;
		}
	      break;

	    case MISCOP_LOAD_DESC:
	      regs[x] = FIELD_REF (yp[y], 0) & ~3;
	      break;

	    case MISCOP_LOAD_LENGTH:
	      regs[x] = MAKE_FIXNUM ((FIELD_REF(yp[y], 0)&~0x80000000) >> 4);
	      break;

	    case MISCOP_TEST_REC:
	      if (HEAP_P (yp[y]))
		{
		  reg_dst = (val *)FIELD_REF (yp[y], 0);
		  reg_count = HEADER_RECORD_P ((val)reg_dst);
		}
	      else
		reg_count = 0;
	      break;

	    case MISCOP_TEST_VEC:
	      if (HEAP_P (yp[y]))
		{
		  reg_dst = (val *)FIELD_REF (yp[y], 0);
		  reg_count = (((val)reg_dst) & 0x8000000F) == (0x80000000 | x);
		}
	      else
		reg_count = 0;
	      break;

	    case MISCOP_TEST_DESC:
	      if (reg_count && ((val)reg_dst) == (yp[y] | 3))
		reg_count = 1;
	      else
		reg_count = 0;
	      break;

	    case MISCOP_TEST_PAIR:
	      if (HEAP_P (yp[y]))
		{
		  reg_dst = (val *)FIELD_REF (yp[y], 0);
		  reg_count = HEADER_PAIR_P ((val)reg_dst);
		}
	      else
		reg_count = 0;
	      break;

	    case MISCOP_LOAD_LENGTH_HALF:
	      regs[x] = MAKE_FIXNUM ((FIELD_REF(yp[y], 0)&~0x80000000) >> 5);
	      break;

	    case MISCOP_ALLOC_BYTES:
	      reg_count = (FIXNUM_VAL (yp[y]) + 3) / 4 + 1;
	      regs[x] = (val) reg_free;
	      reg_dst = reg_free;
	      reg_free += reg_count;
	      break;

	    case MISCOP_TEST_CHAR:
	      reg_count = CHAR_P (yp[y]);
	      break;

	    case MISCOP_MAKE_CHAR:
	      regs[x] = MAKE_CHAR (FIXNUM_VAL (yp[y]));
	      break;

	    case MISCOP_CHAR_VALUE:
	      regs[x] = MAKE_FIXNUM (CHAR_VAL (yp[y]));
	      break;

	    case MISCOP_TEST_FIXNUM:
	      reg_count = FIXNUM_P (yp[y]);
	      break;

	    case MISCOP_SET_COUNT:
	      reg_count = FIXNUM_VAL (yp[y]);
	      break;

	    case MISCOP_SET_COUNT_HI:
	      reg_count = (reg_count & 0xFFFF) | (FIXNUM_VAL (yp[y]) << 16);
	      break;

	    case MISCOP_GET_COUNT:
	      regs[x] = MAKE_FIXNUM (reg_count);
	      break;

	    case MISCOP_GET_COUNT_HI:
	      regs[x] = MAKE_FIXNUM (reg_count >> 16);
	      break;

	    case MISCOP_LOAD_INSN_LENGTH:
	      regs[x] =
		MAKE_FIXNUM (HEADER_CODE_INSN_LENGTH (FIELD_REF(yp[y], 0)));
	      break;

	    case MISCOP_LOAD_LIT_LENGTH:
	      regs[x] =
		MAKE_FIXNUM (HEADER_CODE_LIT_LENGTH (FIELD_REF(yp[y], 0)));
	      break;

	    case MISCOP_ALLOC_CODE:
	      reg_count = reg_count + FIXNUM_VAL (yp[y]) + 1;
	      regs[x] = (val) reg_free;
	      reg_dst = reg_free;
	      reg_free += reg_count;
	      break;

	    case MISCOP_INIT_CODE:
	      reg_dst[0] =
		0x8000000F | (reg_count << 12) | (FIXNUM_VAL (yp[y]) << 4);
	      reg_dst += 1 + reg_count;
	      reg_count = FIXNUM_VAL (yp[y]);
	      break;

	    default:
	      disinsn (stderr, reg_code, reg_pc - 1, 1);
	      fprintf (stderr, "\n");
	      abort ();
	    }
	  break;

	case rlop(MOVE):
	  regs[x] = yp[y];
	  break;

	case rlop(REF):
	  regs[x] = FIELD_REF (yp[y], FIXNUM_VAL(zp[z])+1);
	  break;

	case rlop(REFI):
	  regs[x] = FIELD_REF (yp[y], z);
	  break;	  

	case rlop(SET):
	  FIELD_SET (regs[x], FIXNUM_VAL(zp[z])+1, yp[y]);
	  break;

	case rlop(SETL):
	  FIELD_SET (reg_lit[x], FIXNUM_VAL(zp[z])+1, yp[y]);
	  break;

	case rlop(SETI):
	  FIELD_SET (regs[x], z, yp[y]);
	  break;

	case rlop(CMP):
	  switch (x)
	    {
	    case CMPOP_EQ:
	      reg_count = (yp[y] == zp[z]);
	      break;
	    case CMPOP_FIXNUMS:
	      reg_count = (yp[y] - zp[z]);
	      break;
	    default:
	      disinsn (stderr, reg_code, reg_pc - 1, 1);
	      fprintf (stderr, "\n");
	      abort ();
	    }
	  break;

	case rlop(TRAP2):
	  switch (z)
	    {
	    case TRAPOP_SYSCALL:
	      do_syscall (y, x);
	      break;
	    case TRAPOP_CHECK_CALLSIG:
	      if (x != FIXNUM_VAL (regs[y]))
		check_callsig (x, y);
	      break;
	    case TRAPOP_CHECK_ALLOC:
	      if (reg_free + FIXNUM_VAL(yp[y]) + 1 >= reg_end)
		gc (FIXNUM_VAL(yp[y]) + 1);
	      break;
	    case TRAPOP_CHECK_ALLOC_BYTES:
	      {
		int n = (FIXNUM_VAL(yp[y]) + 3) / 4 + 1;
		if (reg_free + n >= reg_end)
		  gc (n);
	      }
	    case TRAPOP_CHECK_ALLOC_CODE:
	      {
		int n = reg_count + FIXNUM_VAL(yp[y]) + 1;
		if (reg_free + n >= reg_end)
		  gc (n);
	      }
	      break;
	    default:
	      disinsn (stderr, reg_code, reg_pc - 1, 1);
	      fprintf (stderr, "\n");
	      abort ();
	    }
	  break;
	  
	case rlop(TRAPRET):
	  return_from_trap (yp[y], zp[z]);
	  break;

	case TRAP:
	  {
	    word a = *reg_pc++;
	    trap (z, x, y, a);
	  }
	  break;
	  
	case TRAPR:
	  {
	    word a = *reg_pc++;
	    trap (FIXNUM_VAL (regs[z]), x, y, a);
	  }
	  break;

	case ABORT:
	  {
	    word a = *reg_pc++;
	    trap (z, -2, y, a);
	  }
	  break;
	  
	case ABORTR:
	  {
	    word a = *reg_pc++;
	    trap (FIXNUM_VAL (regs[z]), -2, y, a);
	  }
	  break;

	case REFU8:
	  {
	    unsigned char *bytes = BYTEVEC_BYTES(yp[y]);
	    word idx = FIXNUM_VAL(zp[z]);
	    regs[x] = MAKE_FIXNUM (bytes[idx]);
	  }
	  break;

	case SETU8:
	  {
	    unsigned char *bytes = BYTEVEC_BYTES(regs[x]);
	    word idx = FIXNUM_VAL(zp[z]);
	    bytes[idx] = FIXNUM_VAL(yp[y]);
	  }
	  break;

	case REFU16:
	  {
	    unsigned short *wydes = (unsigned short *)BYTEVEC_BYTES(yp[y]);
	    word idx = FIXNUM_VAL(zp[z]);
	    regs[x] = MAKE_FIXNUM (wydes[idx]);
	  }
	  break;

	case SETU16:
	  {
	    unsigned short *wydes = (unsigned short *)BYTEVEC_BYTES(regs[x]);
	    word idx = FIXNUM_VAL(zp[z]);
	    wydes[idx] = FIXNUM_VAL(yp[y]);
	  }
	  break;

	case ADD:
	  {
	    sword a = FIXNUM_VAL(yp[y]);
	    sword b = FIXNUM_VAL(zp[z]);
	    sword c = a + b;
	    regs[x] = MAKE_FIXNUM (c);
	    reg_count = MIN_FIXNUM_VAL <= c && c <= MAX_FIXNUM_VAL;
	  }
	  break;

	case SUB:
	  {
	    sword a = FIXNUM_VAL(yp[y]);
	    sword b = FIXNUM_VAL(zp[z]);
	    sword c = a - b;
	    regs[x] = MAKE_FIXNUM (c);
	    reg_count = MIN_FIXNUM_VAL <= c && c <= MAX_FIXNUM_VAL;
	  }
	  break;

	case MUL:
	  {
	    signed long long a = FIXNUM_VAL(yp[y]);
	    signed long long b = FIXNUM_VAL(zp[z]);
	    signed long long c = a * b;
	    regs[x] = MAKE_FIXNUM (c);
	    reg_count = MIN_FIXNUM_VAL <= c && c <= MAX_FIXNUM_VAL;
	  }
	  break;

	case DIV:
	  {
	    sword a = FIXNUM_VAL(yp[y]);
	    sword b = FIXNUM_VAL(zp[z]);
	    sword c = a / b;
	    regs[x] = MAKE_FIXNUM (c);
	    reg_count = MIN_FIXNUM_VAL <= c && c <= MAX_FIXNUM_VAL;
	  }
	  break;

	case REM:
	  {
	    sword a = FIXNUM_VAL(yp[y]);
	    sword b = FIXNUM_VAL(zp[z]);
	    sword c = a % b;
	    regs[x] = MAKE_FIXNUM (c);
	    reg_count = MIN_FIXNUM_VAL <= c && c <= MAX_FIXNUM_VAL;
	  }
	  break;

	case ADD16:
	  reg_count += FIXNUM_VAL (yp[y]) + FIXNUM_VAL (zp[z]);
	  regs[x] = MAKE_FIXNUM (reg_count & 0xFFFF);
	  break;

	case SUB16:
	  reg_count = (FIXNUM_VAL (yp[y]) - FIXNUM_VAL (zp[z]) 
		       + (((sword)(reg_count << 16)) >> 16));
	  regs[x] = MAKE_FIXNUM (reg_count & 0xFFFF);
	  break;

	case MUL16:
	  reg_count += FIXNUM_VAL (yp[y]) * FIXNUM_VAL (zp[z]);
	  regs[x] = MAKE_FIXNUM (reg_count & 0xFFFF);
	  break;

	case DIV16:
	  reg_count = reg_count / FIXNUM_VAL (yp[y]);
	  regs[x] = MAKE_FIXNUM (reg_count & 0xFFFF);
	  break;

	case ADD16I:
	  reg_count += FIXNUM_VAL (yp[y]) + z;
	  regs[x] = MAKE_FIXNUM (reg_count & 0xFFFF);
	  break;

	case MOVEI:
	  regs[x] = yz;
	  break;	  

	case ALLOCI:
	  reg_count = yz;
	  regs[x] = (val) reg_free;
	  reg_dst = reg_free;
	  reg_free += reg_count;
	  break;	  

	case INITI:
	  reg_dst[0] = xyz;
	  reg_dst++;
	  reg_count--;
	  break;	  

	case FILLI:
	  while (reg_count > 0)
	    {
	      reg_dst[0] = xyz;
	      reg_dst++;
	      reg_count--;
	    }
	  break;	  

	case CHECK_ALLOCI:
	  if (reg_free + xyz >= reg_end)
	    gc (xyz);
	  break;  

	case INIT_VECI:
	  reg_dst[0] = 0x80000000 | xyz;
	  reg_dst++;
	  reg_count--;
	  break;	  

	case BRANCH:
	  switch (x)
	    {
	    case IF_FALSE:
	      if (reg_count == 0)
		reg_pc += (sword)yz;
	      break;
	    case IF_GTE:
	      if (((sword)reg_count) >= 0)
		reg_pc += (sword)yz;
	      break;
	    default:
	      disinsn (stderr, reg_code, reg_pc - 1, 1);
	      fprintf (stderr, "\n");
	      abort ();
	    }
	  break;

	default:
	  disinsn (stderr, reg_code, reg_pc - 1, 1);
	  fprintf (stderr, "\n");
	  abort ();
	}
    }
}

/* Disassembler
 */

char
type_code (val header)
{
  if (HEADER_PAIR_P (header))
    return 'p';
  else if (HEADER_RECORD_P (header))
    return 'r';
  else if (HEADER_VECTOR_P (header))
    return 'v';
  else if (HEADER_BYTEVEC_P (header))
    return 'b';
  else if (HEADER_CODE_P (header))
    return 'c';
  else
    abort ();
}

void
dumpf (FILE *f, val v)
{
  if (FIXNUM_P(v))
    fprintf (f, "%d", FIXNUM_VAL (v));
  else if (CHAR_P(v))
    fprintf (f, "#\\%c", CHAR_VAL (v));
  else if (v == NIL)
    fprintf (f, "nil");
  else if (v == BOOL_T)
    fprintf (f, "#t");
  else if (v == BOOL_F)
    fprintf (f, "#f");
  else if (v == UNSPEC)
    fprintf (f, "unspec");
  else if (BYTEVEC_P(v))
    fprintf (f, "/%.*s/", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
#if 0
  else if (RECORD_P(v) 
	   && RECORD_LENGTH(v) > 0
	   && BYTEVEC_P (RECORD_REF (v, 0)))
    {
      /* Might be a string.
       */
      val s = RECORD_REF (v, 0);
      fprintf (f, "\"%.*s\"", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
    }
  else if (RECORD_P(v)
	   && RECORD_LENGTH(v) > 0
	   && RECORD_P (RECORD_REF (v, 0))
	   && RECORD_LENGTH (RECORD_REF (v, 0)) > 0
	   && BYTEVEC_P (RECORD_REF (RECORD_REF (v, 0), 0)))
    {
      /* Might be a symbol
       */
      val s = RECORD_REF (RECORD_REF (v, 0), 0);
      fprintf (f, "'%.*s", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
    }
#endif
  else
    fprintf (f, "%c", type_code (FIELD_REF(v,0)));
}

int
disstring (FILE *f, val v, int written)
{
  /* String: #S(bytes) */
  if (RECORD_P (v)
      && RECORD_LENGTH (v) == 1
      && BYTEVEC_P (RECORD_REF (v, 0)))
    {
      val bytes = RECORD_REF (v, 0);
      fprintf (f, (written ? "\"%.*s\"" : "%.*s"),
	       BYTEVEC_LENGTH (bytes), BYTEVEC_BYTES (bytes));
      return 1;
    }

  return 0;
}

int
maybesym (val v)
{
  return (RECORD_P (v)
	  && RECORD_LENGTH (v) == 2
	  && (BOOL_T == RECORD_REF (v, 0)
	      || FIXNUM_P (RECORD_REF (v, 0)))
	  && PAIR_P (RECORD_REF (v, 1)));
}

int
dissym (FILE *f, val v)
{
  /* Symbols == #S(offset (comps...))
     Offset == fixnum or #t
     Comps == list of strings
   */
  if (maybesym (v))
    {
      val offset = RECORD_REF (v, 0);
      val comps = RECORD_REF (v, 1);
      
      if (offset == BOOL_T)
	fprintf (f, "/");
      else
	{
	  int o = FIXNUM_VAL (offset);
	  while (o-- > 0)
	    fprintf (f, "../");
	}
      
      while (PAIR_P (comps))
	{
	  if (!disstring (f, CAR (comps), 0))
	    fprintf (f, "?");
	  comps = CDR (comps);
	}
      
      return 1;
    }
      
  return 0;
}

int
diskey (FILE *f, val v)
{
  if (RECORD_P (v)
      && RECORD_LENGTH (v) == 1
      && maybesym (RECORD_REF (v, 0)))
    {
      fprintf (f, ":");
      dissym (f, RECORD_REF (v, 0));
      return 1;
    }

  return 0;
}

void
disval (FILE *f, val v, int level)
{
  if (level > 20)
    {
      fprintf (f, "#");
      return;
    }

  if (FIXNUM_P(v))
    fprintf (f, "%d", FIXNUM_VAL (v));
  else if (CHAR_P(v))
    fprintf (f, "#\\%c", CHAR_VAL (v));
  else if (v == NIL)
    fprintf (f, "nil");
  else if (v == BOOL_T)
    fprintf (f, "#t");
  else if (v == BOOL_F)
    fprintf (f, "#f");
  else if (v == UNSPEC)
    fprintf (f, "unspec");
  else if (BYTEVEC_P(v))
    fprintf (f, "/%.*s/", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
  else if (PAIR_P(v))
    {
      fprintf (f, "(");
      disval (f, CAR(v), level+1);
      v = CDR(v);
      while (PAIR_P(v))
	{
	  fprintf (f, " ");
	  disval (f, CAR(v), level+1);
	  v = CDR(v);
	}
      if (v != NIL)
	{
	  fprintf (f, " . ");
	  disval (f, v, level+1);
	}
      fprintf (f, ")");
    }
  else if (VECTOR_P(v))
    {
      int i, n = VECTOR_LENGTH(v);
      fprintf (f, "#(");
      for (i = 0; i < n; i++)
	{
	  disval (f, VECTOR_REF(v,i), level+1);
	  if (i < n-1)
	    fprintf (f, " ");
	}
      fprintf (f, ")");
    }
  else if (diskey (f, v))
    ;
  else if (dissym (f, v))
    ;
  else if (disstring (f, v, 1))
    ;
  else if (RECORD_P(v))
    {
      val name = RECORD_REF(RECORD_DESC(v),1);
      int i, n = RECORD_LENGTH(v);
      fprintf (f, "#S(");
      if (!dissym (f, name))
	fprintf (f, "?");
      fprintf (f, " ");
      for (i = 0; i < n; i++)
	{
	  disval (f, RECORD_REF(v,i), level+1);
	  if (i < n-1)
	    fprintf (f, " ");
	}
      fprintf (f, ")");
    }
  else
    fprintf (f, "?");
}

void
disfmt (FILE *f, const char *fmt, word insn, val *lit, int reg_hints)
{
  const char *p;
  
  word op = insn >> 24;
  word x = (insn >> 16) & 0xFF;
  word y = (insn >> 8) & 0xFF;
  word z = insn & 0xFF;
  word yz = insn & 0xFFFF;
  word xyz = insn & 0xFFFFFF;

  if (fmt == NULL)
    fmt = "OP %o %x %y %z";

  for (p = fmt; *p; p++)
    {
      if (*p == '%')
	{
	  p++;
	  switch (*p)
	    {
	    case 'o':
	      fprintf (f, "%d", op);
	      break;
	    case 'x':
	      fprintf (f, "%d", x);
	      break;
	    case 'y':
	      fprintf (f, "%d", y);
	      break;
	    case 'z':
	      fprintf (f, "%d", z);
	      break;
	    case 'v':
	      fprintf (f, "%d", yz);
	      break;
	    case 'w':
	      fprintf (f, "%d", xyz);
	      break;
	    case 'X':
	      fprintf (f, "r%d", x);
	      break;
	    case 'L':
	      dumpf (f, lit[x]);
	      break;
	    case 'Y':
	      if (op & 2)
		dumpf (f, lit[y]);
	      else
		{
		  fprintf (f, "r%d", y);
		  if (reg_hints)
		    {
		      fprintf (f, "=");
		      dumpf (f, regs[y]);
		    }
		}
	      break;
	    case 'Z':
	      if (op & 2)
		dumpf (f, lit[z]);
	      else
		{
		  fprintf (f, "r%d", z);
		  if (reg_hints)
		    {
		      fprintf (f, "=");
		      dumpf (f, regs[y]);
		    }
		}
	      break;
	    case 'd':
	      if (reg_hints)
		fprintf (f, "dst=%8x", reg_dst);
	      break;
	    case 'D':
	      if (reg_hints)
		{
		  fprintf (f, "dst=", reg_dst);
		  dumpf (f, (val)reg_dst);
		}
	      break;
	    case 'c':
	      if (reg_hints)
		fprintf (f, "count=%d", reg_count);
	      break;
	    default:
	      fprintf (f, "?");
	      break;
	    }
	}
      else
	fputc (*p, f);
    }
}

const char *reglitop_fmt[128] = {
  [HALT]    = "HALT %x %y %z",
  [MISC]    = NULL,
  [MOVE]    = "MOVE %X %Y",
  [REF]     = "REF %X %Y %Z", 
  [REFI]    = "REFI %X %Y %z",
  [SET]     = "SET %X %Y %Z", 
  [SETL]    = "SETL %L %Y %Z",
  [SETI]    = "SETI %X %Y %z",
  [CMP]     = "CMP %x %Y %Z", 
  [TRAP2]   = NULL,
  [REFU8]   = "REFU8 %X %Y %Z",
  [SETU8]   = "SETU8 %X %Y %Z",
  [REFU16]  = "REFU16 %X %Y %Z",
  [SETU16]  = "SETU16 %X %Y %Z",
  [ADD]     = "ADD %X %Y %Z",
  [SUB]     = "SUB %X %Y %Z",
  [MUL]     = "MUL %X %Y %Z",
  [DIV]     = "DIV %X %Y %Z",
  [REM]     = "REM %X %Y %Z",
  [ADD16]   = "ADD16 %X %Y %Z %c",
  [SUB16]   = "SUB16 %X %Y %Z %c",
  [MUL16]   = "MUL16 %X %Y %Z %c",
  [DIV16]   = "DIV16 %X %Y %Z %c",
  [ADD16I]  = "ADD16I %X %Y %z %c",
  [TRAPRET] = "TRAPRET %Y %Z",
};

const char *op_fmt[256] = {
  [MOVEI]        = "MOVEI %X %v",        
  [ALLOCI]       = "ALLOCI %X %v",       
  [INITI]        = "INITI %w %d",        
  [FILLI]        = "FILLI %w %d %c",        
  [CHECK_ALLOCI] = "CHECK_ALLOCI %w",
  [INIT_VECI]    = "INIT_VECI %w %d",
  [TRAP]         = NULL,
  [TRAPR]        = NULL,
  [ABORT]        = NULL,
  [ABORTR]       = NULL,
  [BRANCH]       = "BRANCH %x %v %c",
};

const char *miscop_fmt[256] = {
  [MISCOP_GO]               = "GO %Y",
  [MISCOP_ALLOC]            = "ALLOC %X %Y",
  [MISCOP_INIT]             = "INIT %Y %d",
  [MISCOP_INIT_REC]         = "INIT_REC %Y %d",
  [MISCOP_INIT_VEC]         = "INIT_VEC %Y %d",
  [MISCOP_FILL]             = "FILL %Y %d %c",
  [MISCOP_COPY]             = "COPY %d %c",
  [MISCOP_LOAD_DESC]        = "LOAD_DESC %X %Y",
  [MISCOP_LOAD_LENGTH]      = "LOAD_LENGTH %X %Y",
  [MISCOP_TEST_REC]         = "TEST_REC %Y",
  [MISCOP_TEST_VEC]         = "TEST_VEC %Y",
  [MISCOP_TEST_DESC]        = "TEST_DESC %Y %d",
  [MISCOP_TEST_PAIR]        = "TEST_PAIR %Y",
  [MISCOP_LOAD_LENGTH_HALF] = "LOAD_LENGTH_HALF %X %Y",
  [MISCOP_ALLOC_BYTES]      = "ALLOC_BYTES %Y",
  [MISCOP_TEST_CHAR]        = "TEST_CHAR %Y",
  [MISCOP_MAKE_CHAR]        = "MAKE_CHAR %Y",
  [MISCOP_CHAR_VALUE]       = "CHAR_VALUE %Y",
  [MISCOP_TEST_FIXNUM]      = "TEST_FIXNUM %Y",
  [MISCOP_SET_COUNT]        = "SET_COUNT %Y",
  [MISCOP_SET_COUNT_HI]     = "SET_COUNT_HI %Y %c",
  [MISCOP_GET_COUNT]        = "GET_COUNT %X",
  [MISCOP_GET_COUNT_HI]     = "GET_COUNT_HI %X",
  [MISCOP_LOAD_INSN_LENGTH] = "LOAD_INSN_LENGTH %X %Y",
  [MISCOP_LOAD_LIT_LENGTH]  = "LOAD_LIT_LENGTH %X %Y",
  [MISCOP_ALLOC_CODE]       = "ALLOC_CODE %X %Y %c",
  [MISCOP_INIT_CODE]        = "INIT_CODE %Y %c",
};

const char *trapop_fmt[256] = {
  [TRAPOP_SYSCALL]           = NULL,
  [TRAPOP_CHECK_CALLSIG]     = "CHECK_CALLSIG %x %Y",
  [TRAPOP_CHECK_ALLOC]       = "CHECK_ALLOC %Y",
  [TRAPOP_CHECK_ALLOC_BYTES] = "CHECK_ALLOC_BYTES %Y",
  [TRAPOP_CHECK_ALLOC_CODE]  = "CHECK_ALLOC_CODE %Y %c"
};

val *
disinsn (FILE *f, val code, val *pc, int reg_hints)
{
  val *lit = ((val *)code) + 1 + CODE_INSN_LENGTH (code);
  word insn = *pc++;
  word op = insn >> 24;
  word x = (insn >> 16) & 0xFF;
  word y = (insn >> 8) & 0xFF;
  word z = insn & 0xFF;
  
  fprintf (f, "%3d: %08x ", pc - ((val *)code) - 2, insn);
 
  if ((op&~3) == MISC)
    disfmt (f, miscop_fmt[z], insn, lit, reg_hints);
  else if ((op&~3) == TRAP2)
    {
      if (z == TRAPOP_SYSCALL)
	{
	  word n = y;

	  disfmt (f, "SYSCALL %X", insn, lit, reg_hints);
	  while (n > 0)
	    {
	      word a = *pc++;
	      fprintf (f, " ");
	      if (a & 0x80000000)
		dumpf (f, lit[a & 0xFF]);
	      else
		fprintf (f, "r%d", a & 0xFF);
	      n--;
	    }
	}
      else
	disfmt (f, trapop_fmt[z], insn, lit, reg_hints);
    }
  else if (op == TRAP || op == ABORT)
    {
      int i;
      int arg_head = y;
      word arg_desc = *pc++;

      int n_args = arg_head >> 4;

      if (op == TRAP)
	disfmt (f, "TRAP %z %X", insn, lit, reg_hints);
      else
	disfmt (f, "ABORT %z %X", insn, lit, reg_hints);

      for (i = 0; i < n_args; i++)
	{
	  int idx = arg_desc & 0xff;
	  fprintf (f, " ");
	  if ((arg_head & 1) == 0)
	    fprintf (f, "r%d", idx);
	  else
	    dumpf (f, lit[idx]);
	  arg_head >>= 1;
	  arg_desc >>= 8;
	}
    }
  else if (op < 128)
    disfmt (f, reglitop_fmt[op & ~3], insn, lit, reg_hints);
  else
    disfmt (f, op_fmt[op], insn, lit, reg_hints);

  return pc;
}

void
dissource (FILE *f, val code)
{
  int count = CODE_INSN_LENGTH (code);
  val *pc = ((val *)code) + 1;
  val source = pc[count];

  if (PAIR_P (source))
    {
      while (PAIR_P (source))
	{
	  disval (f, CAR (source), 0);
	  fprintf (f, "\n");
	  source = CDR (source);
	}
    }
  else
    {
      disval (f, source, 0);
      fprintf (f, "\n");
    }
}

void
discode (FILE *f, val code)
{
  int count = CODE_INSN_LENGTH (code);
  val *pc = ((val *)code) + 1;

  while (count > 0)
    {
      pc = disinsn (f, code, pc, 0);
      fprintf (f, "\n");
      count--;
    }
}


/* The subroutines for the complex instructions.
 */

void
check_callsig (word expected_sig, int caller_sig_reg)
{
  word caller_sig = FIXNUM_VAL (regs[caller_sig_reg]);

  int n_expected = expected_sig >> 1, rest_expected = expected_sig & 1;
  int n_caller = caller_sig >> 1, rest_caller = caller_sig & 1;
  
  if (n_caller < n_expected)
    {
      if (!rest_caller)
	goto wrong_num_args;

      while (n_caller < n_expected)
	{
	  if (!HEAP_P (regs[n_caller+1]))
	    goto wrong_num_args;

	  regs[n_caller+2] = CDR (regs[n_caller+1]);
	  regs[n_caller+1] = CAR (regs[n_caller+1]);
	  n_caller++;
	}
    }
  else if (n_expected < n_caller)
    {
      if (!rest_expected)
	goto wrong_num_args;

      if (!rest_caller)
	{
	  regs[n_caller+1] = NIL;
	  rest_caller = 1;
	}

      if (reg_free + (2*(n_caller - n_expected)) >= reg_end)
	gc (2*(n_caller - n_expected));

      while (n_expected < n_caller)
	{
	  val p = (val)reg_free;
	  reg_free += 2;
	  SET_CAR (p, regs[n_caller]);
	  SET_CDR (p, regs[n_caller+1]);
	  regs[n_caller] = p;
	  n_caller--;
	}
    }

  if (rest_expected && !rest_caller)
    regs[n_caller+1] = NIL;
  else if (!rest_expected && rest_caller && regs[n_caller+1] != NIL)
    goto wrong_num_args;

  return;

 wrong_num_args:
  if (HEAP_P (regs[-9]))
    trap (2, -2, 0, 0);
  else
    {
      int n_expected = expected_sig >> 1, rest_expected = expected_sig & 1;
      int n_caller = caller_sig >> 1, rest_caller = caller_sig & 1;
      
      fprintf (stderr, "ABORT: wrong number of arguments.\n");
      fprintf (stderr, "Expected %d+%d, got %d+%d\n",
	       n_expected, rest_expected, n_caller, rest_caller);
      dissource (stderr, reg_code);
      discode (stderr, reg_code);
      fprintf (stderr, "\n");
      abort ();
    }
}

/* The GC.

   It must use only a constant amount of space in addition to
   the semi spaces.  Thus, it must not be recursive.

   There are two basic operations: copy and scan.

   COPY simply copies an object from from_space to to_space and leaves
   a forwarding pointer in the from_space.  COPY does not modify
   pointers while copying; SCAN will do that.

   SCAN iterates over all words in an object in to_space, COPYing the
   referenced objects as appropriate and modifying the words to point
   to the new location of the referenced object.

   The whole GC then consists of scanning the registers followed by
   scanning all objects in to_space.
*/ 

val *to_space, *to_ptr;

val
snap_pointer (val ptr)
{
  val header = FIELD_REF(ptr,0);
  if (HEAP_P(header) &&
      (val *)header >= to_space && (val *)header < to_space + SPACE_SIZE)
    return header;
  else
    return ptr;
}

/* Copy the object at PTR to the to_space if it isn't already there.
*/
val
copy (val ptr)
{
  sword size;
  char *type;
  val header, ptr2;

  /* check for a forwarding pointer
   */
  ptr2 = snap_pointer (ptr);
  if (ptr2 != ptr)
    {
#if DEBUG
      fprintf (stderr, "copy forward %p -> %p\n", ptr, ptr2);
#endif
      return ptr2;
    }

  if ((val *)ptr < spaces[1 - active_space]
      || (val *)ptr >= spaces[1 - active_space] + SPACE_SIZE)
    {
      fprintf (stderr, "rogue pointer: %p\n", ptr);
      breakpoint ();
    }
  
  header = FIELD_REF(ptr,0);

#if DEBUG
  fprintf (stderr, "copy %p (%08x) -> %p\n", ptr, header, to_ptr);
#endif

  if (HEADER_PAIR_P (header))
    {
      type = "pair";
      size = 2;
    }
  else if (HEADER_RECORD_P (header))
    {
      val desc = snap_pointer (HEADER_RECORD_DESC (header));
#if 0
      if (!RECORD_P (desc))
	{
	  fprintf (stderr, "desc not record (copy)\n");
	  abort ();
	}
#endif
      type = "record";
      size = FIXNUM_VAL(RECORD_REF(desc,0)) + 1;
    }
  else if (HEADER_VECTOR_P (header))
    {
      type = "vector";
      size = HEADER_VECTOR_LENGTH (header) + 1;
    }
  else if (HEADER_BYTEVEC_P (header))
    {
      type = "bytevec";
      size = (((HEADER_BYTEVEC_LENGTH(header) + 3) / 4) + 1);
    }
  else if (HEADER_CODE_P (header))
    {
      type = "code";
      size = (HEADER_CODE_LIT_LENGTH(header) + 
	      HEADER_CODE_INSN_LENGTH(header) + 1);
    }
  else
    abort ();
  
#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  memcpy (to_ptr, (val *)ptr, size*sizeof(val));
  UNCHECKED_FIELD_SET (ptr, 0, (val)to_ptr);
  to_ptr += size;

  return (val)(to_ptr - size);
}

void
scan_words (val *ptr, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++)
    {
      if (HEAP_P (ptr[i]))
	ptr[i] = copy (ptr[i]);
    }
}

val *
scan (val *ptr)
{
  sword size;
  char *type;
  val header = *ptr;
  
#if DEBUG
  fprintf (stderr, "scan %08x (%08x)\n", ptr, header);
#endif

  if (HEADER_PAIR_P (header))
    {
      type = "pair";
      size = 2;
    }
  else if (HEADER_RECORD_P (header))
    {
      val desc = copy (HEADER_RECORD_DESC (header));
#if 0
      if (!RECORD_P (desc))
	{
	  fprintf (stderr, "desc not record\n");
	  abort ();
	}
#endif
      UNCHECKED_FIELD_SET (ptr, 0, MAKE_HEADER_RECORD (desc));
      type = "record";
      size = FIXNUM_VAL(RECORD_REF(desc,0));
      ptr++;
    }
  else if (HEADER_VECTOR_P (header))
    {
      type = "vector";
      size = HEADER_VECTOR_LENGTH (header);
      ptr++;
    }
  else if (HEADER_BYTEVEC_P (header))
    {
      type = "bytevec";
      size = - ((HEADER_BYTEVEC_LENGTH(header) + 3) / 4);
      ptr++;
    }
  else if (HEADER_CODE_P (header))
    {
      type = "code";
      size = HEADER_CODE_LIT_LENGTH(header);
      ptr += HEADER_CODE_INSN_LENGTH(header) + 1;
    }
  else
    abort ();
  
#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  if (size > 0)
    {
      scan_words (ptr, size);
      return ptr + size;
    }
  else
    return ptr - size;
}

void rehash_hashq_vectors (val list);
void invoke_interrupt_handler ();

void
gc (word size)
{
  val *ptr;
  size_t count;
  word pc_off = reg_pc - (word *)reg_code;

  if (reg_end == 0)
    {
      /* Interrupt
       */
      reg_end = spaces[active_space] + SPACE_SIZE;
      invoke_interrupt_handler ();
      return;
    }

  active_space = 1 - active_space;
  to_space = spaces[active_space];

  // fprintf (stderr, "alloc of %d bytes failed, off %d\n", size, pc_off);

  to_ptr = to_space;
  scan_words (regs, 256);
  scan_words (&regs[-3], 1);
  scan_words (&regs[-6], 1);
  scan_words (&regs[-7], 1);
  scan_words (&regs[-8], 1);
  scan_words (&regs[-9], 1);
  scan_words (&reg_code, 1);
  for (ptr = to_space, count = 0; ptr < to_ptr; count++)
    ptr = scan (ptr);

  if (trace_gc)
    fprintf (stderr, "GC: copied %d objects, %d words (%02f%%)\n",
	     count, to_ptr - to_space, (to_ptr - to_space)*100.0/SPACE_SIZE);

  reg_pc = (word *)reg_code + pc_off;
  reg_lit = ((word *)reg_code) + 1 + CODE_INSN_LENGTH (reg_code);
  reg_free = to_ptr;
  reg_end = to_space + SPACE_SIZE;

  if (reg_free > reg_end)
    {
      fprintf (stderr, "FULL\n");
      exit (1);
    }

  rehash_hashq_vectors (regs[-3]);
}

struct image_header {
  word magic;
  word origin;
  word start;
};

#define MAGIC_1 0xABCD0002

char *suspend_image;

void
suspend (val cont)
{
  val *ptr;
  size_t count;

  int fd;
  struct image_header head;
  size_t image_size;

  if (suspend_image == NULL)
    return;

  // XXX - Once we do the GC, we can't return to Scheme.  So let's
  //       hope the saving succeeds.  We could simply restart with CONT,
  //       tho.

  active_space = 1 - active_space;
  to_space = spaces[active_space];

  to_ptr = to_space;
  scan_words (&regs[-3], 1);
  scan_words (&cont, 1);
  for (ptr = to_space, count = 0; ptr < to_ptr; count++)
    ptr = scan (ptr);

  rehash_hashq_vectors (regs[-3]);

  fd = open (suspend_image, O_WRONLY|O_CREAT|O_TRUNC, 0666);
  if (fd < 0)
    perror_exit (suspend_image);

  head.magic = MAGIC_1;
  head.origin = (word)to_space;
  head.start = (word)cont;
  if (write (fd, &head, sizeof (head)) != sizeof (head))
    perror_exit (suspend_image);
  
  image_size = (to_ptr - to_space) * sizeof(word);
  if (write (fd, to_space, image_size) != image_size)
    perror_exit (suspend_image);

  if (close (fd) < 0)
    perror_exit (suspend_image);
    
  if (trace_gc)
    fprintf (stderr, "SUSPEND: wrote %s, %d objects, %d words\n",
	     suspend_image, count, image_size / sizeof(word));
  exit (0);
}

/* Hashq tables

   Since hashq tables use object addresses in the hash function, and
   the GC moves objects around, hashq tables are implemented in the
   run-time.

   The following operations are provided as syscalls; none of them
   allocates memory.

   - alist_to_hashq_vector alist vec

   Destructively put the cells of a alist into a vector.  No checks
   for duplicate keys are done.  The original elements of the vector
   will form the tails of the new overflow lists.

   - hashq_vector_to_alist vec

   Collect the overflow lists from a vector into one alist.  The new
   elements of the vector will be the tails of the old overflow lists.

   - hashq_vector_ref vec key new-pair

   Return the cell with the given key if it exists in vec.  If not,
   return #f.  If new-pair is a pair, use it to create a new entry if
   none is found.  new-pair must have a pair in its car that in turn
   must have key in its car.  new-pair will be used as a new element
   of the overflow list.  (This should probably not be a syscall.)

   - hashq_vector_del vec key

   Remove the cell with the given key if it exists and return it.
   Return #f if it doesn't exist.  (This should probably not be a
   syscall.)
*/

val
hashq_vector_to_alist (val vec)
{
  int n = VECTOR_LENGTH (vec);
  int i;
  val result = NIL;

  for (i = 0; i < n; i++)
    {
      val a = NIL, b = VECTOR_REF (vec, i);
      while (PAIR_P (b))
	{
	  a = b;
	  b = CDR (b);
	}
      if (PAIR_P (a))
	{
	  SET_CDR (a, result);
	  result = VECTOR_REF (vec, i);
	  VECTOR_SET (vec, i, b);
	}
    }

  return result;
}

val
alist_to_hashq_vector (val alist, val vec)
{
  int n = VECTOR_LENGTH (vec);

  while (PAIR_P (alist))
    {
      val s = alist;
      val c = CAR (s);

      alist = CDR (alist);

      if (PAIR_P (c))
	{
	  val key = CAR (c);
	  int h = key % n;
	  SET_CDR (s, VECTOR_REF (vec, h));
	  VECTOR_SET (vec, h, s);
	}
    }

  return BOOL_F;
}

val
hashq_vector_ref (val vec, val key, val new_pair)
{
  int n = VECTOR_LENGTH (vec);
  int h = key % n;
  val s;

  for (s = VECTOR_REF (vec, h); PAIR_P (s); s = CDR (s))
    {
      val c = CAR (s);
      if (PAIR_P (c) && CAR (c) == key)
	return c;
    }

  if (PAIR_P (new_pair))
    {
      SET_CDR (new_pair, VECTOR_REF (vec, h));
      VECTOR_SET (vec, h, new_pair);
    }

  return BOOL_F;
}

val
hashq_vector_del (val vec, val key)
{
  int n = VECTOR_LENGTH (vec);
  int h = key % n;
  val *s;

  for (s = VECTOR_LOC (vec, h); PAIR_P (*s); s = CDR_LOC (*s))
    {
      val c = CAR (*s);
      if (PAIR_P (c) && CAR (c) == key)
	{
	  *s = CDR (*s);
	  return BOOL_T;
	}
    }

  return BOOL_F;
}

void
rehashq_vector (val vec)
{
  alist_to_hashq_vector (hashq_vector_to_alist (vec), vec);
}

void
rehash_hashq_vectors (val list)
{
  while (PAIR_P (list))
    {
      val vec = CAR (list);
      if (VECTOR_P (vec))
	rehashq_vector (vec);
      list = CDR (list);
    }
}

/* Marks

   This is used when scanning all live objects.
*/

void
clear_marks ()
{
  val *mark_space = spaces[1 - active_space];
  memset (mark_space, 0, SPACE_SIZE * sizeof (val));
}

int
mark (val *obj)
{
  val *obj_space = spaces[active_space];
  val *mark_space = spaces[1 - active_space];

  mark_space[obj - obj_space] = 1;
}

int
is_marked (val *obj)
{
  val *obj_space = spaces[active_space];
  val *mark_space = spaces[1 - active_space];

  if (obj < obj_space || obj > obj_space + SPACE_SIZE)
    abort ();

  return mark_space[obj - obj_space];
}

/* Reverse scanning - this is expensive, of course.

   Right now, we use a recursive graph visit, but we should combine
   this with running a GC in order to be able to run with constant
   stack.  Syscalls can't run a GC, unfortunately.
*/

size_t scan_for_referrers (val obj, val vec, size_t count, val referrer);

size_t
scan_words_for_referrers (val obj, val vec, size_t count,
			  val referrer, val *ptr, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++)
    {
      if (ptr[i] == obj)
	{
	  if (HEAP_P (referrer) && referrer != vec)
	    {
	      if (VECTOR_P (vec) && count < VECTOR_LENGTH (vec))
		VECTOR_SET (vec, count, referrer);
	      count++;
	    }
	}

      if (HEAP_P (ptr[i]))
	count = scan_for_referrers (obj, vec, count, ptr[i]);
    }
  return count;
}

size_t
scan_for_referrers (val obj, val vec, size_t count, val referrer)
{
  sword size;
  char *type;
  val *ptr = (val *)referrer;
  val header = *ptr;
  
  if (is_marked (ptr))
    return count;
  
  mark (ptr);

#if DEBUG
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, header);
#endif

  if (HEADER_PAIR_P (header))
    {
      type = "pair";
      size = 2;
    }
  else if (HEADER_RECORD_P (header))
    {
      val desc = HEADER_RECORD_DESC (header);
      type = "record";
      size = FIXNUM_VAL(RECORD_REF(desc,0));
      ptr++;
    }
  else if (HEADER_VECTOR_P (header))
    {
      type = "vector";
      size = HEADER_VECTOR_LENGTH (header);
      ptr++;
    }
  else if (HEADER_BYTEVEC_P (header))
    {
      type = "bytevec";
      size = - ((HEADER_BYTEVEC_LENGTH(header) + 3) / 4);
      ptr++;
    }
  else if (HEADER_CODE_P (header))
    {
      type = "code";
      size = HEADER_CODE_LIT_LENGTH(header);
      ptr += HEADER_CODE_INSN_LENGTH(header) + 1;
    }
  else
    abort ();
  
#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  if (size > 0)
    return scan_words_for_referrers (obj, vec, count,
				     referrer, ptr, size);
  else
    return count;
  
}

size_t
find_referrers (val obj, val vec)
{
  size_t count;

  clear_marks ();
  count = scan_words_for_referrers (obj, vec, 0, BOOL_F, regs, 256);
  
  return (count << 2)|1;
}

/* Finding record type instances
 */

size_t scan_for_instances (val desc, val vec, size_t count, val obj);

size_t
scan_words_for_instances (val desc, val vec, size_t count,
			  val *ptr, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++)
    {
      if (HEAP_P (ptr[i]))
	count = scan_for_instances (desc, vec, count, ptr[i]);
    }
  return count;
}

size_t
scan_for_instances (val desc, val vec, size_t count, val obj)
{
  sword size;
  char *type;
  val *ptr = (val *)obj;
  val header = *ptr;
  
  if (is_marked (ptr))
    return count;
  
  mark (ptr);

#if DEBUG
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, header);
#endif

  if (HEADER_PAIR_P (header))
    {
      type = "pair";
      size = 2;
    }
  else if (HEADER_RECORD_P (header))
    {
      val d = HEADER_RECORD_DESC (header);
      type = "record";
      size = FIXNUM_VAL(RECORD_REF(d,0));

      if (d == desc)
	{
	  if (VECTOR_P (vec) && count < VECTOR_LENGTH (vec))
	    VECTOR_SET (vec, count, obj);
	  count++;
	}

      ptr++;
    }
  else if (HEADER_VECTOR_P (header))
    {
      type = "vector";
      size = HEADER_VECTOR_LENGTH (header);
      ptr++;
    }
  else if (HEADER_BYTEVEC_P (header))
    {
      type = "bytevec";
      size = - ((HEADER_BYTEVEC_LENGTH(header) + 3) / 4);
      ptr++;
    }
  else if (HEADER_CODE_P (header))
    {
      type = "code";
      size = HEADER_CODE_LIT_LENGTH(header);
      ptr += HEADER_CODE_INSN_LENGTH(header) + 1;
    }
  else
    abort ();
  
#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  if (size > 0)
    return scan_words_for_instances (desc, vec, count, ptr, size);
  else
    return count;
}

size_t
find_instances (val desc, val vec)
{
  size_t count;

  clear_marks ();
  count = scan_words_for_instances (desc, vec, 0, regs, 256);
  
  return (count << 2)|1;
}

/* Transmogrifying objects
*/

void scan_for_transmogrify (val from, val to, val obj);

val
find_replacement (val from, val to, val obj)
{
  size_t i, n;
  n = VECTOR_LENGTH (from);

  for (i = 0; i < n; i++)
    {
      if (obj == VECTOR_REF (from, i))
	return VECTOR_REF (to, i);
    }

  return obj;
}

void
scan_words_for_transmogrify (val from, val to, val *ptr, size_t n)
{
  size_t i;

  for (i = 0; i < n; i++)
    {
      ptr[i] = find_replacement (from, to, ptr[i]);
      if (HEAP_P (ptr[i]))
	scan_for_transmogrify (from, to, ptr[i]);
    }
}

void
scan_for_transmogrify (val from, val to, val obj)
{
  sword size;
  char *type;
  val *ptr = (val *)obj;
  val header = *ptr;

  if (obj == from)
    {
      /* Never transmmogrify inside the FROM vector since we need it,
	 of course.
      */
      return;
    }

  if (is_marked (ptr))
    return;
  
  mark (ptr);

#if DEBUG
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, header);
#endif

  if (HEADER_PAIR_P (header))
    {
      type = "pair";
      size = 2;
    }
  else if (HEADER_RECORD_P (header))
    {
      val desc = HEADER_RECORD_DESC (header);
      val new_desc = find_replacement (from, to, desc);

      if (new_desc != desc)
	{
	  if (RECORD_REF(new_desc,0) != RECORD_REF(desc,0))
	    fprintf (stderr,
		     "ERROR: new descriptor has wrong size,"
		     " not transmogrifying\n");
	  else
	    FIELD_SET (ptr, 0, MAKE_HEADER_RECORD (new_desc));
	}

      type = "record";
      size = FIXNUM_VAL(RECORD_REF(desc,0));
      ptr++;
    }
  else if (HEADER_VECTOR_P (header))
    {
      type = "vector";
      size = HEADER_VECTOR_LENGTH (header);
      ptr++;
    }
  else if (HEADER_BYTEVEC_P (header))
    {
      type = "bytevec";
      size = - ((HEADER_BYTEVEC_LENGTH(header) + 3) / 4);
      ptr++;
    }
  else if (HEADER_CODE_P (header))
    {
      type = "code";
      size = HEADER_CODE_LIT_LENGTH(header);
      ptr += HEADER_CODE_INSN_LENGTH(header) + 1;
    }
  else
    abort ();
  
#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  if (size > 0)
    scan_words_for_transmogrify (from, to, ptr, size);
}

void
transmogrify_objects (val from, val to)
{
  if (trace_gc)
    {
      size_t j, n;
      n = VECTOR_LENGTH (from);

      for (j = 0; j < n; j++)
	{
	  fprintf (stderr, "%p -> %p\n",
		   VECTOR_REF (from, j),
		   VECTOR_REF (to, j));
	}
    }

  clear_marks ();
  scan_words_for_transmogrify (from, to, regs, 256);
}

/* Debugging
 */

void
dump (val v)
{
  fprintf (stderr, " ");
  dumpf (stderr, v);
}

int
is_string (val v, char *str)
{
  return (BYTEVEC_P (v)
	  && BYTEVEC_LENGTH (v) == strlen (str)
	  && memcmp (BYTEVEC_BYTES (v), str, BYTEVEC_LENGTH (v)) == 0);
}

void
dump_regs ()
{
  int i;

  for (i = 0; i < 10; i++)
    dump (regs[i]);
}

/* Syscalls
 */

#define MAX_ARGS 16

void syscall_trap (int n_args, val *args, int return_register);

void
do_syscall (word n_args, word return_register)
{
  val arg[MAX_ARGS];
  int i;

  for (i = 0; i < n_args; i++)
    {
      val a = *reg_pc++;
      if (a & 0x80000000)
	arg[i] = reg_lit[a&0xFF];
      else
	arg[i] = regs[a&0xFF];
    }

  syscall_trap (n_args, arg, return_register);
}

void
syscall_trap (int n_args, val *arg, int return_register)
{
  val res;
  int i;

  if (trace_syscalls)
    {
      fprintf (stderr, "syscall");

      for (i = 0; i < n_args; i++)
	dump (arg[i]);
      fprintf (stderr, " |");
      dump_regs ();
      fprintf (stderr, "\n");
    }

  if (n_args == 0)
    {
      fprintf (stderr, "PANIC\n");
      exit (0);
    }

  if (arg[0] == MAKE_FIXNUM(2))
    {
      /* write (fd, buf, start, end) */
      word fd = FIXNUM_VAL (arg[1]);
      char *buf = BYTEVEC_BYTES (arg[2]);
      word start = FIXNUM_VAL (arg[3]);
      word end = FIXNUM_VAL (arg[4]);
      word n;

      // fprintf (stderr, "writing %d %p %d %d\n", fd, buf, start, end);
      n = write (fd, buf + start, end - start);

      res = MAKE_FIXNUM (n);
    }
  else if (arg[0] == MAKE_FIXNUM(3))
    {
      /* read (fd, buf, start, end) */
      word fd = FIXNUM_VAL (arg[1]);
      char *buf = BYTEVEC_BYTES (arg[2]);
      word start = FIXNUM_VAL (arg[3]);
      word end = FIXNUM_VAL (arg[4]);
      word n;

      // fprintf (stderr, "reading %d %p %d %d\n", fd, buf, start, end);
      n = read (fd, buf + start, end - start);

      res = MAKE_FIXNUM (n);
    }
  else if (arg[0] == MAKE_FIXNUM(4))
    {
      /* hashq_vector_ref (vec, key, new_pair) */
      res = hashq_vector_ref (arg[1], arg[2], arg[3]);
    }
  else if (arg[0] == MAKE_FIXNUM(5))
    {
      /* hashq_vector_del (vec, key) */
      res = hashq_vector_del (arg[1], arg[2]);
    }
  else if (arg[0] == MAKE_FIXNUM(6))
    {
      /* hashq_vector_to_alist (vec) */
      res = hashq_vector_to_alist (arg[1]);
    }
  else if (arg[0] == MAKE_FIXNUM(7))
    {
      /* alist_to_hashq_vector (alist, vec) */
      res = alist_to_hashq_vector (arg[1], arg[2]);
    }
  else if (arg[0] == MAKE_FIXNUM(8))
    {
      /* get_reg (i) */
      res = regs[FIXNUM_VAL(arg[1])];
    }
  else if (arg[0] == MAKE_FIXNUM(9))
    {
      /* set_reg (i, v) */
      int i = FIXNUM_VAL(arg[1]);
      regs[i] = arg[2];
      if (i == -3)
	rehash_hashq_vectors (regs[-3]);
      res = regs[i];
    }
  else if (arg[0] == MAKE_FIXNUM(10))
    {
      /* suspend (cont) */
      suspend (arg[1]);
      res = BOOL_F;
    }
  else if (arg[0] == MAKE_FIXNUM(11))
    {
      /* argdump */
      int i, n;

      n = (FIXNUM_VAL(regs[0])+1) >> 1;
      fprintf (stderr, "ARGS:");
      for (i = 1; i < n+1; i++)
	dump (regs[i]);
      fprintf (stderr, "\n");
      res = BOOL_F;
    }
  else if (arg[0] == MAKE_FIXNUM(12))
    {
      /* find_referrers */
      res = find_referrers (arg[1], arg[2]);
    }
  else if (arg[0] == MAKE_FIXNUM(13))
    {
      /* find_instances */
      res = find_instances (arg[1], arg[2]);
    }
  else if (arg[0] == MAKE_FIXNUM(14))
    {
      /* transmogrify_objects */
      transmogrify_objects (arg[1], arg[2]);
      res = BOOL_T;
    }
  else if (arg[0] == MAKE_FIXNUM(15))
    {
      res = regs[-7] = arg[1];
    }
  else if(arg[0] == MAKE_FIXNUM(16))
    {
      /* connect (host, port) */

      char *hostname;
      int port = FIXNUM_VAL (arg[2]);

      int fd;
      struct sockaddr_in name;
      struct hostent *hostinfo;
      
      fd = socket (PF_INET, SOCK_STREAM, 0);
      if (fd < 0)
	goto syscall_err;
      
      hostname = strndup (BYTEVEC_BYTES (arg[1]),
			  BYTEVEC_LENGTH (arg[1]));
      hostinfo = gethostbyname (hostname);
      free (hostname);

      if (hostinfo == NULL)
	{
	  close (fd);
	  errno = ENOENT;
	  goto syscall_err;
	}

      name.sin_family = AF_INET;
      name.sin_port = htons (port);
      name.sin_addr = *(struct in_addr *) hostinfo->h_addr;

      if (connect (fd, (struct sockaddr *) &name, sizeof (name)) < 0)
	{
	  int err = errno;
	  close (fd);
	  errno = err;
	  goto syscall_err;
	}
	
      res = MAKE_FIXNUM (fd);
    }
  else if (arg[0] == MAKE_FIXNUM(17))
    {
      /* close (fd) */
      if (close (FIXNUM_VAL (arg[1])) < 0)
	goto syscall_err;
      res = BOOL_T;
    }
  else if (arg[0] == MAKE_FIXNUM(18))
    {
      /* open-archive-for-read (name) */

      char *name;
      int fd;

      asprintf (&name, "%.*s.arch",
		BYTEVEC_LENGTH (arg[1]),
		BYTEVEC_BYTES (arg[1]));
      fd = open (name, O_RDONLY);
      free (name);

      res = MAKE_FIXNUM (fd >= 0? fd : -errno);
    }
  else if (arg[0] == MAKE_FIXNUM(19))
    {
      /* open-archive-for-write () */
      char template[] = "tmp.XXXXXX";
      int fd = mkstemp (template);
      if (fd < 0)
	res = MAKE_FIXNUM (-errno);
      else
	{
	  char *name;
	  asprintf (&name, "tmp.%d", fd);
	  rename (template, name);
	  free (name);

	  res = MAKE_FIXNUM (fd);
	}
    }
  else if (arg[0] == MAKE_FIXNUM(20))
    {
      /* commit-archive (fd, name) */
      
      int fd = FIXNUM_VAL (arg[1]);

      if (close (fd) < 0)
	res = MAKE_FIXNUM (-errno);
      else
	{
	  char *name, *tmp_name;
	  asprintf (&name, "%.*s.arch",
		    BYTEVEC_LENGTH (arg[2]),
		    BYTEVEC_BYTES (arg[2]));
	  asprintf (&tmp_name, "tmp.%d", fd);
	  
	  rename (tmp_name, name);
	  free (tmp_name);
	  free (name);
      
	  res = BOOL_T;
	}
    }
  else if (arg[0] == MAKE_FIXNUM(21))
    {
      /* abort-archive (fd) */
      
      int fd = FIXNUM_VAL (arg[1]);

      close (fd);
      res = BOOL_T;
    }
  else
    res = BOOL_T;

  regs[return_register] = res;
  return;

 syscall_err:
  regs[return_register] = MAKE_FIXNUM (-errno);
  return;
}

void
boot (val closure, val *free, val *end)
{
  int i;

  for (i = 0; i < 256; i++)
    regs[i] = UNSPEC;

  regs[-9] = BOOL_F; // trap vector
  regs[-8] = BOOL_F; // interrupt val
  regs[-7] = BOOL_F; // interrupt flag
  regs[-6] = BOOL_F; // error:wrong-num-args code
  regs[-5] = BOOL_F; // target_sig for adjust_call_sig
  regs[-4] = BOOL_F; // adjust_call_sig
  regs[-3] = NIL;    // hashq vectors
  regs[-2] = BOOL_F; // gc_glue
  regs[-1] = BOOL_F; // sys

  regs[0] = MAKE_FIXNUM(2*3+0);
  regs[1] = BOOL_F;  // closure vector of continuation
  regs[2] = closure; // closure vector of boot procedure
  regs[3] = BOOL_F;  // continuation of boot procedure
  
  reg_src = reg_dst = NULL;
  reg_count = 0;

  reg_free = free;
  reg_end = end;
  
  do_GO (RECORD_REF(closure,0));

  run_cpu ();
}

void
invoke_interrupt_handler ()
{
  /* Interrupt is trap 1 with no arguments and no return value.
   */
  if (regs[-7] != BOOL_F)
    {
      regs[-7] = BOOL_F;
      trap (1, -1, 0, 0);
    }
}

void
trap (int trap_number, int return_reg, int arg_head, word arg_desc)
{
  /* We create an object that stores the current state.  This object
     can be used with the return-from-trap instruction.  The object
     contains the living registers and the current code object.
  */
  
  int n_args = arg_head >> 4;
  val args[4];

  int n_regs = 256;
  int n_elts = 3 + n_regs;
  int i;
  val state;
  val trap_vector;
  val handler;

  for (i = 0; i < n_args; i++)
    {
      if ((arg_head & 1) == 0)
	args[i] = regs[arg_desc & 0xff];
      else
	args[i] = reg_lit[arg_desc & 0xff];
      arg_head >>= 1;
      arg_desc >>= 8;
    }

  if (trap_number == 0)
    {
      syscall_trap (n_args, args, return_reg);
      return;
    }

  trap_vector = regs[-9];
  if (!VECTOR_P (trap_vector)
      || VECTOR_LENGTH (trap_vector) <= trap_number
      || !HEAP_P (handler = VECTOR_REF (trap_vector, trap_number)))
    {
      fprintf (stderr, "HALT: Unhandled trap %d\n", trap_number);
      if (return_reg >= 0)
	regs[return_reg] = UNSPEC;
      return;
    }
  
  /* Allocate vector
   */
  if (reg_free + n_elts + 1 > reg_end)
    gc (n_elts + 1);
  state = (val)reg_free;
  reg_dst = reg_free;
  reg_free += n_elts + 1;
  reg_dst[0] = (n_elts << 4) | 0x80000003;
  
  /* Fill with state
   */
  VECTOR_SET (state, 0, reg_code);
  VECTOR_SET (state, 1, MAKE_FIXNUM (reg_pc - (val *)reg_code));
  VECTOR_SET (state, 2, MAKE_FIXNUM (return_reg));
  for (i = 0; i < n_regs; i++)
    VECTOR_SET (state, 3+i, regs[i]);

  /* Setup arguments for handler
   */
  {
    regs[0] = MAKE_FIXNUM (2*(n_args + 4) + 0);
    regs[1] = BOOL_F;   // closure vector of continuation
    regs[2] = handler;  // closure record of procedure
    regs[3] = BOOL_F;   // continuation
    regs[4] = state;    // first argument, the state
    for (i = 0; i < n_args; i++)
      regs[5+i] = args[i];
  }

  do_GO (RECORD_REF (handler, 0));
}

void
return_from_trap (val state, val return_value)
{
  int i, n_regs, return_reg;
  
  n_regs = 256;
  for (i = 0; i < n_regs; i++)
    regs[i] = VECTOR_REF (state, 3+i);

  return_reg = FIXNUM_VAL(VECTOR_REF (state, 2));
  if (return_reg >= 0)
    regs[return_reg] = return_value;
  
  if (return_reg == -2)
    {
      fprintf (stderr, "ABORT\n");
      exit (1);
    }

  do_GO_with_offset (VECTOR_REF (state, 0),
		     FIXNUM_VAL (VECTOR_REF (state, 1)));
}

void
sighandler (int sig, struct sigcontext *ctxt)
{
  reg_end = 0;
  signal (sig, (void (*) (int))sighandler);
}

void
find_markers (val *mem, size_t n)
{
  int i;
  for (i = 0; i < n; i++)
    if ((mem[i]) == 0x14015140)
      printf ("marker: %p\n", mem+i);
}

int
main (int argc, char **argv)
{
  ssize_t n;
  val *space;
  struct image_header head;
  size_t image_size;

  if (argc > 1 && !strcmp (argv[1], "-v"))
    {
      trace_gc = 1;
      argv++;
      argc--;
    }

  if (argc > 1 && !strcmp (argv[1], "-V"))
    {
      trace_gc = 1;
      trace_syscalls = 1;
      argv++;
      argc--;
    }

  if (argc > 1 && !strcmp (argv[1], "-VV"))
    {
      trace_gc = 1;
      trace_syscalls = 1;
      trace_insns = 1;
      argv++;
      argc--;
    }

  if (argc > 1 && !strcmp (argv[1], "-ts"))
    {
      trace_source = 1;
      argv++;
      argc--;
    }

  if (argc > 1 && !strcmp (argv[1], "-tg"))
    {
      trace_source = 1;
      trace_go = 1;
      argv++;
      argc--;
    }

  if (argc != 2 && argc != 3)
    {
      fprintf (stderr, "usage: suo-vm <image> [<suspend-image>]\n");
      exit (1);
    }

  if (argc > 2)
    suspend_image = argv[2];
  else
    suspend_image = argv[1];

  init_heap ();
  space = spaces[active_space];

  int fd = open (argv[1], O_RDONLY);
  if (fd < 0)
    perror_exit (argv[1]);

  struct stat buf;
  if (fstat (fd, &buf) < 0)
    perror_exit (argv[1]);

  if (buf.st_size < sizeof(head))
    error_exit ("too small");

  image_size = buf.st_size - sizeof(head);

  if (image_size > SPACE_SIZE * sizeof(val))
    error_exit ("too big");

  n = read (fd, &head, sizeof (head));
  if (n < 0 || n != sizeof (head))
    perror_exit (argv[1]);
  
  if (head.magic != MAGIC_1)
    error_exit ("wrong magic");

  n = read (fd, space, image_size);
  if (n < 0 || n != image_size)
    perror_exit (argv[1]);
  close (fd);
  
  unswizzle_objects (space, ((size_t)space) - head.origin, n/4);

  find_markers (space, n/4);

  signal (SIGPIPE, SIG_IGN);
  signal (SIGINT, (void (*) (int))sighandler);

  boot (((val)space) + head.start - head.origin,
	space + n/4, space + SPACE_SIZE);

  return 0;
}
