/* suo-vm -- virtual machine
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>

#define DEBUG 0

int verbose = 0;
int super_verbose = 0;

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

#define FIXNUM_P(v)        (((v)&3)==1)
#define FIXNUM_VAL(v)      (((sword)(v))>>2)
#define MAKE_FIXNUM(n)     (((word)(n))<<2|1)

#define CHAR_P(v)          (((v)&7)==6)
#define CHAR_VAL(v)        (((word)(v))>>3)
#define MAKE_CHAR(n)       (((word)(n))<<3|6)

#define HEAP_P(v)          (((v)&3)==0)
#define LOC(o,i)           (((val *)(o))+i)
#define REF(o,i)           (((val *)(o))[i])
#define SET(o,i,v)         (((val *)(o))[i]=(v))

#define HEADER_PAIR_P(v)   (((v)&3)!=3)

#define PAIR_P(v)          (HEAP_P(v) && HEADER_PAIR_P(REF(v,0)))
#define CAR(p)             REF(p,0)
#define CDR(p)             REF(p,1)
#define CDR_LOC(p)         LOC(p,1)
#define SET_CDR(p,v)       SET(p,1,v)

#define HEADER_RECORD_P(v)       (((v)&0x80000003)==0x00000003)
#define HEADER_RECORD_DESC(v)    ((v)&~3)
#define MAKE_HEADER_RECORD(desc) ((desc)|3)

#define RECORD_P(v)        (HEAP_P(v) && HEADER_RECORD_P(REF(v,0)))
#define RECORD_DESC(v)     (HEADER_RECORD_DESC(REF(v,0)))
#define RECORD_LENGTH(v)   FIXNUM_VAL(RECORD_REF(RECORD_DESC(v),0))
#define RECORD_LOC(v,i)    LOC(v,(i)+1)
#define RECORD_REF(v,i)    REF(v,(i)+1)
#define RECORD_SET(v,i,e)  SET(v,(i)+1,e)

#define HEADER_VECTOR_P(v)      (((v)&0x8000000F)==0x80000003)
#define HEADER_VECTOR_LENGTH(v) (((v)&~0x80000000)>>4)

#define VECTOR_P(v)        (HEAP_P(v) && HEADER_VECTOR_P(REF(v,0)))
#define VECTOR_LENGTH(v)   (HEADER_VECTOR_LENGTH(REF(v,0)))
#define VECTOR_LOC(v,i)    LOC(v,(i)+1)
#define VECTOR_REF(v,i)    REF(v,(i)+1)
#define VECTOR_SET(v,i,e)  SET(v,(i)+1,e)

#define HEADER_BYTEVEC_P(v)      (((v)&0x8000000F)==0x8000000B)
#define HEADER_BYTEVEC_LENGTH(v) (((v)&~0x80000000)>>4)

#define BYTEVEC_P(v)       (HEAP_P(v) && HEADER_BYTEVEC_P(REF(v,0)))
#define BYTEVEC_LENGTH(v)  (HEADER_BYTEVEC_LENGTH(REF(v,0)))
#define BYTEVEC_BYTES(v)   ((char *)LOC(v,1))

#define HEADER_CODE_P(v)           (((v)&0x8000000F)==0x8000000F)
#define HEADER_CODE_INSN_LENGTH(v) (((v)>>12)&0x7FFFF)
#define HEADER_CODE_LIT_LENGTH(v)  (((v)>>4)&0xFF)

#define CODE_P(v)           (HEAP_P(v) && HEADER_CODE_P(REF(v,0)))
#define CODE_INSN_LENGTH(v) (HEADER_CODE_INSN_LENGTH(REF(v,0)))
#define CODE_LIT_LENGTH(v)  (HEADER_CODE_LIT_LENGTH(REF(v,0)))


#define HEAP_SIZE  (32*1024*1024)
#define SPACE_SIZE (HEAP_SIZE/2)

val *spaces[2];
int active_space;

void
init_heap ()
{
  spaces[0] = malloc (HEAP_SIZE * sizeof(val));
  if (spaces[0] == NULL)
    perror_exit ("malloc");
  spaces[1] = spaces[0] + SPACE_SIZE;
  active_space = 0;
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

  if (verbose)
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

   reglit[D][I] = reglit[S]

   - SETL D S I

   lit[D][FIXNUM_VAL(reglit[I])+1] = reglit[S]

   - ALLOC D S                == MISC D S MISCOP_ALLOC

   count = FIXNUM_VAL (reglit[S])
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

   - BRANCH C O

   If condition C is true, pc += O.

   Conditions are
   
     IF_TRUE:    count != 0
     IF_RECORD:  dst & 0x80000003 == 3

     IFNOT variants also exist.


   ** Complex instructions

   These instructions do complicated things by conditionally branching
   out to subroutines.  You can imagine the subroutines being
   implemented somewhere in ROM, using an instruction set from above
   plus whatever is needed in addition.

   - SYSCALL N              == TRAP 0 N TRAPOP_SYSCALL

   Trap into the operation system to perform syscall number S.  The
   next N words of the instruction stream are used as parameters for
   the syscall.  Each word is either of the form

       R    or   0x80000000 + L

   where R means that reg[R] is to be used, and the second form refers
   to lit[L].

   - CHECK_ALLOCI S

   If FREE + S >= END, run the garbage collector.  If there still
   isn't enough free space afterwards, halt the machine.

   - CHECK_CALLSIG C S        == TRAP C S TRAPOP_CHECK_CALLSIG

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
   and REST in registers 1, 2, 4, and 4, respectively.  When using
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

val specials_and_regs[8+256];
val *regs = specials_and_regs + 8;

val reg_code;

word *reg_pc;
word *reg_src;
word *reg_dst;
word  reg_count;
word *reg_free;
word *reg_end;

void
run_cpu ()
{
  while (1)
    {
      word insn = *reg_pc;
      word op = insn >> 24;
      word x = (insn >> 16) & 0xFF;
      word y = (insn >> 8) & 0xFF;
      word z = insn & 0xFF;

      switch (op)
	{
	case 0:
	  fprintf (stderr, "HALT %02x%02x%02x\n", x, y, z);
	  exit (0);
	}
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
  val header = REF(ptr,0);
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

  header = REF(ptr,0);

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
  SET(ptr, 0, (val)to_ptr);
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
      SET (ptr, 0, MAKE_HEADER_RECORD (desc));
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

void
gc (word size)
{
  val *ptr;
  size_t count;
  word pc_off = reg_pc - (word *)reg_code;

  if (reg_end == 0)
    {
      fprintf (stderr, "INTERRUPT\n");
      reg_end = spaces[active_space] + SPACE_SIZE;
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
  scan_words (&reg_code, 1);
  for (ptr = to_space, count = 0; ptr < to_ptr; count++)
    ptr = scan (ptr);

  if (verbose)
    fprintf (stderr, "GC: copied %d objects, %d words (%02f%%)\n",
	     count, to_ptr - to_space, (to_ptr - to_space)*100.0/SPACE_SIZE);

  reg_pc = (word *)reg_code + pc_off;
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
    
  if (verbose)
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
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, v);
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
	    SET (ptr, 0, MAKE_HEADER_RECORD (new_desc));
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
  if (verbose)
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
dump (val v)
{
  if (FIXNUM_P(v))
    fprintf (stderr, " %d", FIXNUM_VAL (v));
  else if (CHAR_P(v))
    fprintf (stderr, " #\\%c", CHAR_VAL (v));
  else if (v == NIL)
    fprintf (stderr, " nil");
  else if (v == BOOL_T)
    fprintf (stderr, " #t");
  else if (v == BOOL_F)
    fprintf (stderr, " #f");
  else if (v == UNSPEC)
    fprintf (stderr, " unspec");
  else if (BYTEVEC_P(v))
    fprintf (stderr, " /%.*s/", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
#if 0
  else if (RECORD_P(v) 
	   && RECORD_LENGTH(v) > 0
	   && BYTEVEC_P (RECORD_REF (v, 0)))
    {
      /* Might be a string.
       */
      val s = RECORD_REF (v, 0);
      fprintf (stderr, " \"%.*s\"", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
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
      fprintf (stderr, " '%.*s", BYTEVEC_LENGTH(v), BYTEVEC_BYTES(v));
    }
#endif
  else
    fprintf (stderr, " %c", type_code (REF(v,0)));
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

val
sys (int n_args,
     val arg1, val arg2, val arg3, val arg4, val arg5, val arg6, val arg7)
{
  if (n_args == 0)
    {
      if (verbose)
	fprintf (stderr, "PANIC\n");
      exit (0);
    }

  if (arg1 == MAKE_FIXNUM(2))
    {
      /* write (fd, buf, start, end) */
      word fd = FIXNUM_VAL (arg2);
      char *buf = BYTEVEC_BYTES (arg3);
      word start = FIXNUM_VAL (arg4);
      word end = FIXNUM_VAL (arg5);
      word res;

      // fprintf (stderr, "writing %d %p %d %d\n", fd, buf, start, end);
      res = write (fd, buf + start, end - start);

      return MAKE_FIXNUM (res);
    }
  else if (arg1 == MAKE_FIXNUM(3))
    {
      /* read (fd, buf, start, end) */
      word fd = FIXNUM_VAL (arg2);
      char *buf = BYTEVEC_BYTES (arg3);
      word start = FIXNUM_VAL (arg4);
      word end = FIXNUM_VAL (arg5);
      word res;

      // fprintf (stderr, "reading %d %p %d %d\n", fd, buf, start, end);
      res = read (fd, buf + start, end - start);

      return MAKE_FIXNUM (res);
    }
  else if (arg1 == MAKE_FIXNUM(4))
    {
      /* hashq_vector_ref (vec, key, new_pair) */
      return hashq_vector_ref (arg2, arg3, arg4);
    }
  else if (arg1 == MAKE_FIXNUM(5))
    {
      /* hashq_vector_del (vec, key) */
      return hashq_vector_del (arg2, arg3);
    }
  else if (arg1 == MAKE_FIXNUM(6))
    {
      /* hashq_vector_to_alist (vec) */
      return hashq_vector_to_alist (arg2);
    }
  else if (arg1 == MAKE_FIXNUM(7))
    {
      /* alist_to_hashq_vector (alist, vec) */
      return alist_to_hashq_vector (arg2, arg3);
    }
  else if (arg1 == MAKE_FIXNUM(8))
    {
      /* get_reg (i) */
      return regs[FIXNUM_VAL(arg2)];
    }
  else if (arg1 == MAKE_FIXNUM(9))
    {
      /* set_reg (i, v) */
      int i = FIXNUM_VAL(arg2);
      regs[i] = arg3;
      if (i == -3)
	rehash_hashq_vectors (regs[-3]);
      return regs[i];
    }
  else if (arg1 == MAKE_FIXNUM(10))
    {
      /* suspend (cont) */
      suspend (arg2);
      return BOOL_F;
    }
  else if (arg1 == MAKE_FIXNUM(11))
    {
      /* argdump */
      int i, n;

      n = (FIXNUM_VAL(regs[0])+1) >> 1;
      fprintf (stderr, "ARGS:");
      for (i = 1; i < n+1; i++)
	dump (regs[i]);
      fprintf (stderr, "\n");
      return BOOL_F;
    }
  else if (arg1 == MAKE_FIXNUM(12))
    {
      /* find_referrers */
      return find_referrers (arg2, arg3);
    }
  else if (arg1 == MAKE_FIXNUM(13))
    {
      /* find_instances */
      return find_instances (arg2, arg3);
    }
  else if (arg1 == MAKE_FIXNUM(14))
    {
      /* transmogrify_objects */
      transmogrify_objects (arg2, arg3);
      return BOOL_T;
    }

  if (super_verbose)
    {
      fprintf (stderr, "syscall");
      if (n_args > 0)
	dump (arg1);
      if (n_args > 1)
	dump (arg2);
      if (n_args > 2)
	dump (arg3);
      if (n_args > 3)
	dump (arg4);
      if (n_args > 4)
	dump (arg5);
      if (n_args > 5)
	dump (arg6);
      if (n_args > 6)
	dump (arg7);
      if (n_args > 7)
	fprintf (stderr, " ...");
      fprintf (stderr, " |");
      dump_regs ();
      fprintf (stderr, "\n");
    }

  return BOOL_T;
}

void
boot (val closure, val *free, val *end)
{
  int i;

  for (i = 0; i < 256; i++)
    regs[i] = UNSPEC;

  regs[-8] = BOOL_F; // interrupt val
  regs[-7] = BOOL_F; // interrupt flag
  regs[-6] = BOOL_F; // error:wrong-num-args code
  regs[-5] = BOOL_F; // target_sig for adjust_call_sig
  regs[-4] = BOOL_F; // adjust_call_sig
  regs[-3] = NIL;    // hashq vectors
  regs[-2] = BOOL_F; // gc_glue
  regs[-1] = BOOL_F; // sys
  regs[0] = MAKE_FIXNUM(4);
  regs[1] = closure;
  regs[2] = BOOL_F;  // cont of boot procedure

  reg_code = RECORD_REF(closure,0);
  reg_free = free;
  reg_end = end;
  
  reg_pc = ((word *)reg_code) + 1;

  run_cpu ();
}

void
sighandler (int sig, struct sigcontext *ctxt)
{
  printf ("INT\n");
  reg_end = 0;
  signal (sig, (void (*) (int))sighandler);
}

void
find_markers (val *mem, size_t n)
{
  int i;
  for (i = 0; i < n; i++)
    if ((mem[i] & 0xFFFF) == 0xDEAD)
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
      verbose = 1;
      argv++;
      argc--;
    }

  if (argc > 1 && !strcmp (argv[1], "-V"))
    {
      super_verbose = verbose = 1;
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

  signal (SIGINT, (void (*) (int))sighandler);

  boot (((val)space) + head.start - head.origin,
	space + n/4, space + SPACE_SIZE);

  return 0;
}
