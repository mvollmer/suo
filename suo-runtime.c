/* suo-runtime -- lowest level runtime 
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

typedef unsigned int word;
typedef   signed int sword;
typedef         word val;

val specials_and_regs[8+256];
val *regs = specials_and_regs + 8;

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
      val v = *ptr;
      
      // fprintf (stderr, "on %d %08x\n", ptr-mem, v);
      count++;

      if ((v & 3) == 3)
	{
	  /* non-pair header */
	  if (((sword)(v)) >= 0)
	    {
	      /* record header */
	      *ptr = UNSWIZZLE (*ptr, off);
	      val *desc = (val *)(*ptr & ~3);
	      type = "record";
	      n_records++;
	      size = ((sword)desc[1]) >> 2;
	    }
	  else
	    {
	      v &= ~ 0x80000000;
	      if ((v & 15) == 3)
		{
		  type = "vector";
		  n_vectors++;
		  size = v >> 4;
		}
	      else if ((v & 15) == 11)
		{
		  /* bytevec header */
		  type = "bytevec";
		  n_bytevecs++;
		  size = -(((v >> 4) + 3) / 4);
		}
	      else if ((v & 15) == 15)
		{
		  /* code header */
		  type = "code";
		  n_codes++;
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
	  n_pairs++;
	  size = 2;
	}

      // fprintf (stderr, " %s, %d words\n", type, size);

      if (size > 0)
	{
	  if (size % 2)
	    n_odds++;

	  while (size > 0)
	    {
	      val v = *ptr;
	      if ((v & 3) == 0)
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
	      size = (((v >> 4) + 3) / 4);
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
    {
      scan_words (ptr, size);
      return ptr + size;
    }
  else
    return ptr - size;
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

void rehash_hashq_vectors (val list);

word *
gc (word *params)
{
  val *ptr;
  size_t count;
  word lr_off = params[GCPAR_LR] - params[GCPAR_R15];
  word size = params[GCPAR_R4] - params[GCPAR_R16];

  if (params[GCPAR_R17] == 0)
    {
      fprintf (stderr, "INTERRUPT\n");
      params[GCPAR_R17] = spaces[active_space] + SPACE_SIZE;
      return params;
    }

  gc_count++;

  active_space = 1 - active_space;
  to_space = spaces[active_space];

  // fprintf (stderr, "alloc of %d bytes failed, off %d\n", size, lr_off);

  to_ptr = to_space;
  scan_words (regs, 256);
  scan_words (&regs[-3], 1);
  scan_words (&regs[-6], 1);
  scan_words (&regs[-7], 1);
  scan_words (&regs[-8], 1);
  scan_words (&params[GCPAR_R15], 1);
  for (ptr = to_space, count = 0; ptr < to_ptr; count++)
    ptr = scan (ptr);

  if (verbose)
    fprintf (stderr, "GC: copied %d objects, %d words (%02f%%)\n",
	     count, to_ptr - to_space, (to_ptr - to_space)*100.0/SPACE_SIZE);

  params[GCPAR_LR] = params[GCPAR_R15] + lr_off;
  params[GCPAR_R4] = (word)(to_ptr + size);
  params[GCPAR_R16] = (word)to_ptr;
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

  rehash_hashq_vectors (regs[-3]);
  
  return params;
}

struct image_header {
  word magic;
  word origin;
  word start;
};

#define MAGIC_1 0xABCD0001

char *suspend_image;

void
suspend (word cont)
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

#define HEAP_P(v)          ((v&3)==0)
#define LOC(o,i)           (((val *)(o))+i)
#define REF(o,i)           (((val *)(o))[i])
#define SET(o,i,v)         (((val *)(o))[i]=(v))

#define VECTOR_P(v)        (HEAP_P(v) && (REF(v,0)&0x8000000F) == 0x80000003)
#define VECTOR_LENGTH(v)   ((REF(v,0)&~0x80000000)>>4)
#define VECTOR_LOC(v,i)    LOC(v,(i)+1)
#define VECTOR_REF(v,i)    REF(v,(i)+1)
#define VECTOR_SET(v,i,e)  SET(v,(i)+1,e)

#define PAIR_P(v)          (HEAP_P(v) && (REF(v,0)&3)!=3)
#define CAR(p)             REF(p,0)
#define CDR(p)             REF(p,1)
#define CDR_LOC(p)         LOC(p,1)
#define SET_CDR(p,v)       SET(p,1,v)

#define EOL                2
#define BOOL_T             10
#define BOOL_F             18

val
hashq_vector_to_alist (val vec)
{
  int n = VECTOR_LENGTH (vec);
  int i;
  val result = EOL;

  for (i = 0; i < n; i++)
    {
      val a = EOL, b = VECTOR_REF (vec, i);
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
  val v = *ptr;
  
  if (is_marked (ptr))
    return count;
  
  mark (ptr);

#if DEBUG
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, v);
#endif

  if ((v & 3) == 3)
    {
      /* non-pair header */
      if (((sword)(v)) >= 0)
	{
	  /* record header */
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
  val v = *ptr;
  
  if (is_marked (ptr))
    return count;
  
  mark (ptr);

#if DEBUG
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, v);
#endif

  if ((v & 3) == 3)
    {
      /* non-pair header */
      if (((sword)(v)) >= 0)
	{
	  /* record header */
	  val *d = (val *)(*ptr & ~3);
	  type = "record";
	  size = ((sword)d[1]) >> 2;

	  if (((val)d) == desc)
	    {
	      if (VECTOR_P (vec) && count < VECTOR_LENGTH (vec))
		VECTOR_SET (vec, count, obj);
	      count++;
	    }
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

   XXX - no need to scan only life objects, transmogrifying dead ones
         is harmless and doesn't need to recurse.
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
  val v = *ptr;

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
  fprintf (stderr, "Scan %08x (%08x)\n", ptr, v);
#endif

  if ((v & 3) == 3)
    {
      /* non-pair header */
      if (((sword)(v)) >= 0)
	{
	  /* record header */
	  val desc = (*ptr & ~3);
	  val new_desc = find_replacement (from, to, desc);

	  if (new_desc != desc)
	    {
	      if (((val *)new_desc)[1] != ((val *)desc)[1])
		fprintf (stderr,
			 "ERROR: new descriptor has wrong size,"
			 " not transmogrifying\n");
	      else
		*ptr = new_desc | 3;
	    }
	  type = "record";
	  size = ((sword)((val *)desc)[1]) >> 2;
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

void
dump (val v)
{
  fprintf (stderr, " %08x", v);
  if ((v & 3) == 1)
    fprintf (stderr, " (%d)", ((sword)v) >> 2);
  else if ((v & 7) == 6)
    fprintf (stderr, " (#\\%c)", ((sword)v) >> 3);
  else if (v == 2)
    fprintf (stderr, " (nil)");
  else if (v == 10)
    fprintf (stderr, " (#t)");
  else if (v == 18)
    fprintf (stderr, " (#f)");
  else if (v == 26)
    fprintf (stderr, " (unspec)");
  else if ((v & 3) == 0)
    {
      val t = *(val *)v;
      if ((t & 0x8000000f) == 0x8000000b)
	{
	  val l = (t & ~0x80000000) >> 4;
	  fprintf (stderr, " %d \"%.*s\"", l, l, ((val *)v)+1);
	}
      else
	fprintf (stderr, " [%08x]", t);
    }
}

char
type_code (val tag_word)
{
  if ((tag_word & 3) == 3)
    {
      /* non-pair header */
      if (((sword)(tag_word)) >= 0)
	{
	  /* record header */
	  return 'r';
	}
      else
	{
	  tag_word &= ~ 0x80000000;
	  if ((tag_word & 15) == 3)
	    return 'v';
	  else if ((tag_word & 15) == 11)
	    return 'b';
	  else if ((tag_word & 15) == 15)
	    return 'c';
	  else
	    abort ();
	}
    }
  else
    return 'p';
}

void
dump_arg (val v)
{
  if ((v & 3) == 1)
    fprintf (stderr, " %d", ((sword)v) >> 2);
  else if ((v & 7) == 6)
    fprintf (stderr, " #\\%c", ((sword)v) >> 3);
  else if (v == 2)
    fprintf (stderr, " nil");
  else if (v == 10)
    fprintf (stderr, " #t");
  else if (v == 18)
    fprintf (stderr, " #f");
  else if (v == 26)
    fprintf (stderr, " unspec");
  else if ((v & 3) == 0)
    {
      val t = *(val *)v;
      if ((t & 0x8000000f) == 0x8000000b)
	{
	  val l = (t & ~0x80000000) >> 4;
	  fprintf (stderr, " \"%.*s\"", l, ((val *)v)+1);
	}
      else
	fprintf (stderr, " %c", type_code (t));
    }
  else
    fprintf (stderr, " ?");
}

int
is_string (val v, char *str)
{
  if ((v & 3) == 0)
    {
      val t = *(val *)v;
      if ((t & 0x8000000f) == 0x8000000b)
	{
	  val l = (t & ~0x80000000) >> 4;
	  return (l == strlen (str)
		  && memcmp (((val *)v)+1, str, l) == 0);
	}
    }
  return 0;
}

void
dump_regs ()
{
  int i;

  for (i = 0; i < 10; i++)
    dump_arg (regs[i]);
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
	fprintf (stderr, "PANIC after %d GCs.\n", gc_count);
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
  else if (arg1 == ((3<<2)|1))
    {
      /* read (fd, buf, start, end) */
      word fd = ((word)arg2) >> 2;
      val *buf = (val *)arg3;
      word start = ((word)arg4) >> 2;
      word end = ((word)arg5) >> 2;
      char *bytes = (char*)(buf+1);
      word res;

      // fprintf (stderr, "reading %d %p %d %d\n", fd, buf, start, end);
      res = read (fd, ((char *)(buf+1)) + start, end-start);

      return (val)((res << 2) | 1);
    }
  else if (arg1 == ((4<<2)|1))
    {
      /* hashq_vector_ref (vec, key, new_pair) */
      return hashq_vector_ref (arg2, arg3, arg4);
    }
  else if (arg1 == ((5<<2)|1))
    {
      /* hashq_vector_del (vec, key) */
      return hashq_vector_del (arg2, arg3);
    }
  else if (arg1 == ((6<<2)|1))
    {
      /* hashq_vector_to_alist (vec) */
      return hashq_vector_to_alist (arg2);
    }
  else if (arg1 == ((7<<2)|1))
    {
      /* alist_to_hashq_vector (alist, vec) */
      return alist_to_hashq_vector (arg2, arg3);
    }
  else if (arg1 == ((8<<2)|1))
    {
      /* get_reg (i) */
      return regs[((sword)arg2) >> 2];
    }
  else if (arg1 == ((9<<2)|1))
    {
      /* set_reg (i, v) */
      int i = ((sword)arg2) >> 2;
      regs[i] = arg3;
      if (i == -3)
	rehash_hashq_vectors (regs[-3]);
      return regs[i];
    }
  else if (arg1 == ((10<<2)|1))
    {
      /* suspend (cont) */
      suspend (arg2);
      return 10;
    }
  else if (arg1 == ((11<<2)|1))
    {
      /* argdump */
      int i, n;

      n = ((regs[0]>>2)+1)>>1;
      fprintf (stderr, "ARGS:");
      for (i = 1; i < n+1; i++)
	dump_arg (regs[i]);
      fprintf (stderr, "\n");
      return 10;
    }
  else if (arg1 == ((12<<2)|1))
    {
      /* find_referrers */
      return find_referrers (arg2, arg3);
    }
  else if (arg1 == ((13<<2)|1))
    {
      /* find_instances */
      return find_instances (arg2, arg3);
    }
  else if (arg1 == ((14<<2)|1))
    {
      /* transmogrify_objects */
      transmogrify_objects (arg2, arg3);
      return BOOL_T;
    }

  if (n_args > 1
      && is_string (arg2, "directory-enter"))
    breakpoint ();

  if (super_verbose)
    {
      fprintf (stderr, "syscall");
      if (n_args > 0)
	dump_arg (arg1);
      if (n_args > 1)
	dump_arg (arg2);
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

  return 0x0000000a;
}

void adjust_call_sig ();

void
go (val closure, val *free, val *end)
{
  int i;
  register val *r14 asm ("r14");
  register val *r15 asm ("r15");
  register val *r16 asm ("r16");
  register val *r17 asm ("r17");

  for (i = 0; i < 256; i++)
    regs[i] = 1; /* fixnum zero */

  regs[-8] = BOOL_F; // interrupt val
  regs[-7] = BOOL_F; // interrupt flag
  regs[-6] = BOOL_F; // error:wrong-num-args code
  regs[-5] = BOOL_F; // target_sig for adjust_call_sig
  regs[-4] = (val)adjust_call_sig;
  regs[-3] = EOL; // hashq vectors
  regs[-2] = (val)gc_glue;
  regs[-1] = (val)sys;
  regs[0] = (2<<3)|1;
  regs[1] = closure;
  regs[2] = BOOL_F;  // cont of boot procedure

  r14 = regs;
  r15 = (val *)((val *)closure)[1];
  r16 = free;
  r17 = end;
  asm ("mr 3,15\n\t addi 3,3,4\n\t mtctr 3\n\t bctr"
       :
       : "r" (r14), "r" (r15), "r" (r16), "r" (r17));
}

void
sighandler (int sig, struct sigcontext *ctxt)
{
  printf ("INT\n");
  ctxt->regs->gpr[17] = 0;
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
      write (2, "usage: suo <image> [<suspend-image>]\n", 20);
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

  go (((val)space) + head.start - head.origin,
      space + n/4, space + SPACE_SIZE);

  return 0;
}
