/* suo-runtime -- lowest level runtime 
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#define DEBUG 0

int verbose = 0;

void
perror_exit (const char *msg)
{
  perror (msg);
  exit (1);
}

typedef unsigned int word;
typedef   signed int sword;
typedef         word val;

val specials_and_regs[2+256];
val *regs = specials_and_regs + 2;

#define HEAP_SIZE  (1*1024*1024)
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
   adding MEM to them.
*/
void
unswizzle_objects (val *mem, word n)
{
  int count = 0;

  val *ptr;
  for (ptr = mem; ptr < mem + n;)
    {
      sword size;
      char *type;
      val v = *ptr;
      
      // fprintf (stderr, "on %d %08x\n", ptr-mem, v);
      count++;

      if ((v & 3) == 3)
	{
	  /* non-pair header */
	  if (((sword)(v)) >= 0)
	    {
	      /* record header */
	      *ptr = UNSWIZZLE (*ptr, mem);
	      val *desc = (val *)(*ptr & ~3);
	      type = "record";
	      size = ((sword)desc[1]) >> 2;
	    }
	  else
	    {
	      v &= ~ 0x80000000;
	      if ((v & 15) == 3)
		{
		  type = "vector";
		  size = v >> 4;
		}
	      else if ((v & 15) == 11)
		{
		  /* bytevec header */
		  type = "bytevec";
		  size = -(((v >> 4) + 3) / 4);
		}
	      else if ((v & 15) == 15)
		{
		  /* code header */
		  type = "code";
		  size = (v >> 4) & 0xFF;
		  ptr += (v >> 12);
		}
	      else
		abort ();
	    }
	  /* size does not include header */
	  ptr++;
	}
      else
	{
	  /* pair */
	  type = "pair";
	  size = 2;
	}

      // fprintf (stderr, " %s, %d words\n", type, size);

      if (size > 0)
	{
	  while (size > 0)
	    {
	      val v = *ptr;
	      if ((v & 3) == 0)
		*ptr = UNSWIZZLE (v, mem);
	      ptr++;
	      size--;
	    }
	}
      else
	ptr += -size;
    }

  if (verbose)
    fprintf (stderr, "booting with %d objects, %d words.\n",
	     count, ptr - mem);
}

/* The GC.  It must use only a constant amount of space in addition to
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

val *
snap_pointer (val *ptr)
{
  val v = *ptr;
  if ((v & 3) == 0 &&
      (val *)v >= to_space && (val *)v < to_space + SPACE_SIZE)
    return (val *)v;
  else
    return ptr;
}

/* Copy the object at PTR to the to_space if it isn't already there.
*/
val *
copy (val *ptr)
{
  sword size;
  char *type;
  val v, *ptr2;

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

  v = *ptr;

#if DEBUG
  fprintf (stderr, "copy %p (%08x) -> %p\n", ptr, v, to_ptr);
#endif

  if ((v & 3) == 3)
    {
      /* non-pair header */
      if (((sword)(v)) >= 0)
	{
	  /* record header */
	  val *desc = (val *)(*ptr & ~3);
	  desc = snap_pointer (desc);
	  type = "record";
	  size = ((sword)desc[1]) >> 2;
	}
      else
	{
	  v &= ~ 0x80000000;
	  if ((v & 15) == 3)
	    {
	      type = "vector";
	      size = v >> 4;
	    }
	  else if ((v & 15) == 11)
	    {
	      /* bytevec header */
	      type = "bytevec";
	      size = -(((v >> 4) + 3) / 4);
	    }
	  else if ((v & 15) == 15)
	    {
	      /* code header */
	      type = "code";
	      size = ((v >> 4) & 0xFF) + (v >> 12);
	    }
	  else
	    abort ();
	}
      /* size does not include header */
      size++;
    }
  else
    {
      /* pair */
      type = "pair";
      size = 2;
    }

#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  if (size < 0)
    size = -size;

  memcpy (to_ptr, ptr, size*sizeof(val));
  *ptr = (val)to_ptr;
  to_ptr += size;

  return to_ptr - size;
}

void
scan_words (val *ptr, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++)
    {
      if ((ptr[i] & 3) == 0)
	ptr[i] = (val) copy ((val *)ptr[i]);
    }
}

val *
scan (val *ptr)
{
  sword size;
  char *type;
  val v = *ptr;
  
#if DEBUG
  fprintf (stderr, "scan %08x (%08x)\n", ptr, v);
#endif

  if ((v & 3) == 3)
    {
      /* non-pair header */
      if (((sword)(v)) >= 0)
	{
	  /* record header */
	  val *desc = copy ((val *)(*ptr & ~3));
	  *ptr = ((val)desc) | 3;
	  type = "record";
	  size = ((sword)desc[1]) >> 2;
	}
      else
	{
	  v &= ~ 0x80000000;
	  if ((v & 15) == 3)
	    {
	      type = "vector";
	      size = v >> 4;
	    }
	  else if ((v & 15) == 11)
	    {
	      /* bytevec header */
	      type = "bytevec";
	      size = -(((v >> 4) + 3) / 4);
	    }
	  else if ((v & 15) == 15)
	    {
	      /* code header */
	      type = "code";
	      size = (v >> 4) & 0xFF;
	      ptr += (v >> 12);
	    }
	  else
	    abort ();
	}
      /* size does not include header */
      ptr++;
    }
  else
    {
      /* pair */
      type = "pair";
      size = 2;
    }
  
#if DEBUG
  fprintf (stderr, " %s, %d words\n", type, size);
#endif
  
  if (size > 0)
    scan_words (ptr, size);
  return ptr + size;
}

int gc_count = 0;

#define GCPAR_LR  0
#define GCPAR_R4  1
#define GCPAR_R15 2
#define GCPAR_R16 3
#define GCPAR_R17 4

void gc_glue (void);

#define PPC_DCBST(where) asm volatile ("dcbst 0,%0" : : "r"(where) : "memory")
#define PPC_SYNC asm volatile ("sync" : : : "memory")
#define PPC_ISYNC asm volatile ("sync; isync" : : : "memory")
#define PPC_ICBI(where) asm volatile ("icbi 0,%0" : : "r"(where) : "memory")

word *
gc (word *params)
{
  val *ptr;
  size_t count;
  word lr_off = params[GCPAR_LR] - params[GCPAR_R15];
  word size = params[GCPAR_R16] - params[GCPAR_R4];

  gc_count++;

  active_space = 1 - active_space;
  to_space = spaces[active_space];

  // fprintf (stderr, "alloc of %d bytes failed, off %d\n", size, lr_off);

  to_ptr = to_space;
  scan_words (regs, 256);
  scan_words (&params[GCPAR_R15], 1);
  for (ptr = to_space, count = 0; ptr < to_ptr; count++)
    ptr = scan (ptr);

  if (verbose)
    fprintf (stderr, "GC: copied %d objects, %d words (%02f%%)\n",
	     count, to_ptr - to_space, (to_ptr - to_space)*100.0/SPACE_SIZE);

  params[GCPAR_LR] = params[GCPAR_R15] + lr_off;
  params[GCPAR_R4] = (word)to_ptr;
  params[GCPAR_R16] = (word)(to_ptr + size);
  params[GCPAR_R17] = (word)(to_space + SPACE_SIZE);

  if (params[GCPAR_R16] > params[GCPAR_R17])
    {
      fprintf (stderr, "FULL\n");
      exit (1);
    }

  /* Flush new semi space out of data cache and into instruction
   * cache.  We assume a cache line size of 32 bytes, which might not
   * be appropriate for all PPC...
   *
   * XXX - we don't need to flush the whole semi-space, only those
   *       parts that are actually code.
   */
  for (ptr = to_space; ptr < to_ptr; ptr += 8)
    PPC_DCBST (ptr);
  PPC_DCBST (ptr - 1);
  PPC_SYNC;
  
  for (ptr = to_space; ptr < to_ptr; ptr += 8)
    PPC_ICBI (ptr);
  PPC_ICBI (ptr - 1);
  PPC_ISYNC;

  return params;
}

void
dump (val v)
{
  fprintf (stderr, " %08x", v);
  if ((v & 3) == 1)
    fprintf (stderr, " (%d)", ((sword)v) >> 2);
  else if ((v & 3) == 0)
    fprintf (stderr, " [%08x]", *(val *)v);
}

val
sys (int n_args,
     val arg1, val arg2, val arg3, val arg4, val arg5, val arg6, val arg7)
{
  if (n_args == 0)
    {
      if (verbose)
	fprintf (stderr, "HALT after %d GCs.\n", gc_count);
      exit (0);
    }

  if (arg1 == ((2<<2)|1))
    {
      /* write (fd, buf, start, end) */
      word fd = ((word)arg2) >> 2;
      val *buf = (val *)arg3;
      word start = ((word)arg4) >> 2;
      word end = ((word)arg5) >> 2;
      char *bytes = (char*)(buf+1);
      word res;

      // fprintf (stderr, "writing %d %p %d %d\n", fd, buf, start, end);
      res = write (fd, ((char *)(buf+1)) + start, end-start);

      return (val)((res << 2) | 1);
    }

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
  fprintf (stderr, "\n");

  return 0x0000000a;
}

void
go (val closure, val arglist, val *free, val *end)
{
  int i;
  register val *r14 asm ("r14") = regs;
  register val *r15 asm ("r15") = (val *)((val *)closure)[1];
  register val *r16 asm ("r16") = free;
  register val *r17 asm ("r17") = end;

  for (i = 0; i < 256; i++)
    regs[i] = 1; /* fixnum zero */

  regs[-2] = (val)gc_glue;
  regs[-1] = (val)sys;
  regs[0] = closure;
  regs[1] = arglist;

  asm ("mr 3,15\n\t addi 3,3,4\n\t mtctr 3\n\t bctr"
       :
       : "r" (r14), "r" (r15), "r" (r16), "r" (r17));
}

int
main (int argc, char **argv)
{
  val *space;

  if (argc > 1 && !strcmp (argv[1], "-v"))
    {
      verbose = 1;
      argv++;
      argc--;
    }

  if (argc != 2)
    {
      write (2, "usage: suo <image>\n", 20);
      exit (1);
    }

  init_heap ();
  space = spaces[active_space];

  int fd = open (argv[1], O_RDONLY);
  if (fd < 0)
    perror_exit (argv[1]);

  struct stat buf;
  if (fstat (fd, &buf) < 0)
    perror_exit (argv[1]);

  if (buf.st_size > SPACE_SIZE * sizeof(val))
    perror_exit ("too big");

  ssize_t n = read (fd, space, buf.st_size);
  if (n < 0)
    perror_exit (argv[1]);

  unswizzle_objects (space, n/4);
  
  go (space[0], space[1], space + n/4, space + SPACE_SIZE);

  return 0;
}

void
foo ()
{
  ((val (*)())regs[-1]) (1, 2, 3);
}
