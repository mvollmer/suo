/*
 * Copyright (C) 2010 Marius Vollmer <marius.vollmer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/.
 */

/* Welcome.

   Suo is little programming environment that is meant to be fun to
   use, fun to write, and fun to learn about.
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <string.h>
#include <ctype.h>

#ifdef DEBUG
#define dbg printf
#else
void dbg (char *fmt, ...) { }
#endif

/* Data types and representation.
 
   Suo knows about the following kinds of values: small integers,
   characters, booleans, vectors, byte vectors, records, pairs, code
   blocks, the empty list, and the 'unspecified' value.

   A small integer is an integer between -536870912 and 536870911,
   inclusive.

   A character is a Unicode code point between 0 and 16777217,
   inclusive.

   A boolean is either the 'true' value, or the 'false' value.

   A vector can store an arbitrary number of values in contigous
   memory.

   A byte vector can store an arbitrary number of raw bytes.  Instead
   of using byte vectors, one might store small integers in normal
   vectors, but that would be much less efficient.  Byte vectors are
   good for storing text, images, or multi precision integers, for
   example.

   A record is much like a vector or a byte vector, but instead of
   carrying its own length with it like vectors and byte vectors do,
   each record points to a 'descriptor' that contains the length, a
   flag that tells whether it contains values or raw bytes, and maybe
   other information.

   A pair can store exactly two values, and is thus the same as a
   two-element vector-like record.  Pairs are used so frequently,
   however, that it is worthwhile to represent them specially.

   A code block contains machine instructions and the constant values
   used by them.
   
   Finally, they are two special values: the empty list (or 'nil')
   that is used to terminate a chain of pairs that forms a linear
   linked list; and the 'unspecified value', which is used to
   initialize fresh storage locations.

   All of these values are represented as 32 bit words.  Some of them
   can be stored completely in 32 bits (like characters), and some of
   them are pointers into a big heap of more words (like vectors).

   What kind of value a word represent can be determined by looking at
   some of its lower bits.  These bits are called the 'tag' of a word.
   With three bits we get 8 different tags, and we use them like this:

   000 - even integers
   100 - odd integers
   001 - pairs
   010 - vectors
   011 - records
   101 - byte vectors and code blocks
   110 - record descriptors
   111 - characters, booleans, empty list, unspecified, headers

   (The significance of record descriptors and headers will be
   explained later.  Just ignore them for now.)

   With three bits used for the tag, only 29 remain for the actual
   value.  For integers, we cleverly use the leftmost bit of the tag
   as part of the value, and we can thus represent any 30 bit integer.

   For values that contain a pointer into the heap, 29 bits allow us
   to point to 512M words.  (If you need more, you need the 64bit
   version of Suo.) 

   To keep things straightforward and efficient, we just zero out the
   tag bits when converting a value to the heap pointer that it
   contains.  This means that we can only point at addresses that are
   a multiple of 8.  This leads to some gaps of unused words between
   objects as we will see.

   One additional complication needs to be considered: some parts of
   the runtime (such as the garbage collector) need to be able to scan
   the big heap of words from start to finish and identify the objects
   that are stored in it.  Thus, it must be possible to tell the type
   of an object without having the three tag bits at hand.

   The heap stores vectors, records, byte vectors, pairs, and code
   blocks.  Pairs use only two words, so there is no space to store
   anything in a pair object to identify it.  Instead, we make sure
   that the first words of all the other kinds can be distinguished
   from a normal value word.

   Vectors, byte vectors, and code blocks use a header word that also
   contains their length.  Records use a record descriptor as their
   first word.  As is explained below in more detail, a record
   descriptor is just a pointer to another record.

   In the following, the details for each kind of value are explained
   together with code for dealing with it.  The code might not look
   particularily efficient, but a modern compiler will have no problem
   to produce good machine instructions for it.

   The functions that are defined here are only used by the low-level
   run-time defined in this file, especially the bootstrap
   interpreter.  The machine code generated by the compiler will not
   call out to these functions of course; it will contain suitable
   code inline.

*/

/* In the beginning was the word.

   We use the types 'word' and 'sword' when working with raw bits, and
   the type 'val' when working with the values that those bits
   represent.
*/

typedef unsigned int word;
typedef   signed int sword;
typedef         word val;

val
val_make (word payload, int shift, int tag)
{
  return (val)((payload << shift)) | tag;
}

#define val_make(payload, shift, tag) \
  ((val)(((payload) << (shift))) | (tag))

word
val_tag (val v, int shift)
{
  return ((word)v)&((1<<shift)-1);
}

word
val_payload (val v, int shift)
{
  return ((word)v)>>shift;
}

word
val_signed_payload (val v, int shift)
{
  return ((sword)v)>>shift;
}

/* Memory allocation
   
   All new memory is allocated from a contigous region of free memory.
   When that region runs out, the garbage collector is invoked to
   create a new region.
 */

val *mem_next;
val *mem_end;

val *mem_gc (int n);

val *
mem_alloc (int n)
{
  val *ptr = mem_next;
  if (ptr + n > mem_end)
    ptr = mem_gc (n);

  mem_next = ptr + ((n+1)&~1);
  return ptr;
}

/* Values that point into the heap.
 */

bool
val_ptr_p (val v)
{
  return val_tag (v, 2) != 0 && val_tag (v, 3) != 7;
}

val
val_ptr_make (val *ptr, int tag)
{
  return ((word)ptr) + tag;
}

val *
val_ptr (val v, int tag)
{
  return (val *)(((word)v)-tag);
}

val *
val_ptr_any_tag (val v)
{
  return (val *)(((word)v)&~7);
}

/* Headers

   Headers are only used as the first word of vectors, byte vectors,
   and code blocks; they are illegal in any other place.  Headers
   share a 3 bit tag with the special values and characters.  Thus, we
   need to distinguish 5 choices, and we do it with these bits:

     1111 - vectors
   000111 - byte vectors
   010111 - code blocks
   100111 - characters
   110111 - special values
*/

word
val_head (val v, int tag)
{
  return val_ptr(v,tag)[0];
}

val
head_make (word payload, int shift, int tag)
{
  return (val)((payload << shift)) + tag;
}

word
head_tag (word h, int shift)
{
  return ((word)h)&((1<<shift)-1);
}

word
head_payload (word h, int shift)
{
  return h>>shift;
}

/* Booleans and special values
 */

#define bool_f val_make (0, 6, 0x37)
#define bool_t val_make (1, 6, 0x37)
#define nil    val_make (2, 6, 0x37)
#define unspec val_make (3, 6, 0x37)

/* Characters
*/

bool
chr_p (val p)
{
  return val_tag (p, 6) == 0x27;
}

#define chr_make(code) val_make (code, 6, 0x27)

int
chr_code (val v)
{
  return val_payload (v, 6);
}


/* Small integers

   Small integers only use the lower two bits as the tag.
*/

const sword fixnum_min = -536870912;
const sword fixnum_max =  536870911;

bool
fixnum_p (val v)
{
  return val_tag (v, 2) == 0;
}

#define fixnum_make(n) val_make (n, 2, 0)

sword
fixnum_num (val v)
{
  return val_signed_payload (v, 2);
}

/* Pairs
 */

bool
pair_p (val v)
{
  return val_tag (v, 3) == 1;
}

bool
pair_ptr_p (val *v)
{
  /* This is the price we pay for wanting to represent pairs with only
     two words.
  */
  if (head_tag (v[0], 3) == 7)
    return (head_tag (v[0], 6) == 0x27
	    || head_tag (v[0], 6) == 0x37);
  else
    return head_tag (v[0], 3) != 6;
}

val
pair_alloc ()
{
  val *ptr = mem_alloc (2);
  return val_ptr_make (ptr, 1);
}

val *
pair_ptr (val v)
{
  return val_ptr (v, 1);
}

/* Vectors

   Vectors store their length in the header.
*/

bool
vec_p (val v)
{
  return val_tag (v, 3) == 2;
}

bool
vec_ptr_p (val *v)
{
  return head_tag (v[0], 4) == 15;
}

val
vec_alloc (word len)
{
  val *ptr = mem_alloc (len + 1);
  ptr[0] = head_make (len, 4, 15);
  return val_ptr_make (ptr, 2);
}

word
vec_ptr_len (val *v)
{
  return head_payload (v[0], 4);
}

word
vec_len (val v)
{
  return vec_ptr_len (val_ptr (v, 2));
}

val *
vec_ptr (val v)
{
  return val_ptr (v, 2) + 1;
}

/* Byte vectors

   Byte vectors and code blocks have to share a tag since there aren't
   enough tags for everyone.  For efficiency, we make code blocks a
   sub-class of byte vectors: every code block is also a byte vector.

   Byte vectors can be accessed as 8 bit bytes, 16 bit half-words, 32
   bit words, and as 32 bit and 64 bit floating point numbers.  Thus,
   we have a lot of accessors.
 */

bool
bytev_p (val v)
{
  return val_tag (v, 3) == 5;
}

bool
bytev_ptr_p (val *v)
{
  return head_tag (v[0], 6) == 7;
}

val
bytev_alloc (word len)
{
  val *ptr = mem_alloc ((len+3)/4 + 1);
  ptr[0] = head_make (len, 6, 7);
  return val_ptr_make (ptr, 5);
}

word
bytev_ptr_len (val *v)
{
  return head_payload (v[0], 6);
}

word
bytev_len (val v)
{
  return bytev_ptr_len (val_ptr (v, 5));
}

#define bytev_ptr(p,t) ((t *)(val_ptr((p),5)+1))

/* Code blocks

   Code blocks are just like byte vectors, except that they are
   followed by an array of values.

   Use the byte vector accessors for the first part, and the vector
   accessor for the second part.
*/

bool
code_ptr_p (val *v)
{
  return head_tag (v[0], 6) == 0x17;
}

word
code_ptr_lit_begin (val *v)
{
  return (bytev_ptr_len (v) + 3) / 4;
}

word
code_ptr_lit_end (val *v)
{
  return v[code_ptr_lit_begin (v)-1];
}

bool
code_p (val v)
{
  return bytev_p (v) && code_ptr_p (val_ptr (v, 5));
}

word
code_lit_begin (val v)
{
  return code_ptr_lit_begin (val_ptr (v, 5));
}

word
code_lit_end (val v)
{
  return code_ptr_lit_end (val_ptr (v, 5));
}

/* Records

   The representation of records is a bit on the complicated side.  A
   record object has a pointer to another record in its first word.
   That other record is called the 'descriptor' for the first record.

   The descriptor contains various information about the record, but
   the only information needed by the run-time is the size of the
   record, and whether or not it stores raw bytes or values.

   This size is stored in the first field of the descriptor, as a
   small integer.  When that integer is positive, the record stores
   values (and is this 'vector like'), when it is negative, the record
   stores bytes, like a byte vector.  However, in the latter case, the
   size is in words and not bytes.

   To access vectors, use any of the vector or byte vector accessors,
   as appropriate.
*/

bool
rec_p (val v)
{
  return val_tag (v, 3) == 3;
}

bool
rec_ptr_p (val *v)
{
  return head_tag (v[0], 3) == 6;
}

val
rec_alloc (word len)
{
  val *ptr = mem_alloc (len+1);
  return val_ptr_make (ptr, 3);
}

val *
rec_ptr (val v)
{
  return val_ptr (v, 3) + 1;
}

val
rec_header_make (val desc)
{
  return val_ptr_make (val_ptr (desc, 3), 6);
}

void
rec_set_desc (val v, val desc)
{
  rec_ptr(v)[-1] = rec_header_make (desc);
}

val
rec_ptr_desc (val *v)
{
  return val_ptr_make (val_ptr (v[0], 6), 3);
}

val
rec_desc (val v)
{
  return rec_ptr_desc (val_ptr (v,3));
}

/* Garbage collection

   When the region of free memory fills up, we find all objects that
   are still alive in it, and copy them over into a second region.
   Then we continue to allocate from that second region.

   To find all living objects, we simply start with a set of root
   values, and follow the graph of pointers from there.  These root
   values are in fixed storage locations, called the registers.  Some
   of them are allocated for fixed purposes, while others are free to
   be used by the code as it sees fit.
*/

const int reg_max_len = 256;
val reg[256];
int reg_len;

const word mem_size = 217000;
val *mem_first;

void
mem_init ()
{
  mem_first = malloc (mem_size*4);
  if (mem_first == NULL)
    abort ();

  mem_next = mem_first;
  mem_end = mem_next + mem_size;

  reg_len = 0;
}

/* The garbage collection algorithm itself consists of two functions:
   'copy' and 'scan'.  The 'copy' function copies one object to the
   new region without changing its content, while the 'scan' function
   examines each word of an object and calls 'copy' to move all
   objects into the new memory region.

   The collection starts with calling 'copy' for each register.  This
   will give us a number of objects in the new memory region.  Then we
   call 'scan' in a loop for each object in the new region, starting
   at the beginning of the region and working towards its end.  Since
   'scan' calls 'copy', more objects will appear in the new region as
   we scan, and we will eventually reach them with our 'scan' loop.

   Note that 'scan' calls 'copy', but 'copy' never calls 'scan'.  The
   algorithm is not recursive.  This is important since recursing for
   deeply nested data structures might overflow the call stack.
 */

val pk (char *title, val x);

val *mem_new_first;
val *mem_new_end;
val *mem_new_next;

void
mem_install_fwd_ptr (val *old, val *new)
{
  old[0] = val_ptr_make (new, 1);
}

val *
mem_follow_fwd_ptr (val *ptr)
{
  word w = ptr[0];
  if (val_tag (w, 3) == 1 &&
      val_ptr (w, 1) >= mem_new_first && val_ptr (w, 1) < mem_new_end)
    return val_ptr (w, 1);
  else
    return ptr;

}

val
mem_copy (val v)
{
  sword size;
  val *ptr, *new_ptr;

  if (!val_ptr_p (v))
    return v;

  ptr = val_ptr_any_tag (v);

  /* If we find a forwarding pointer, we just follow it.
   */
  new_ptr = mem_follow_fwd_ptr (ptr);
  if (new_ptr != ptr)
    return val_ptr_make (new_ptr, val_tag (v, 3));

  if (pair_ptr_p (ptr))
    size = 2;
  else if (vec_ptr_p (ptr))
    size = vec_ptr_len (ptr) + 1;
  else if (bytev_ptr_p (ptr))
    size = (bytev_ptr_len (ptr) + 3) / 4 + 1;
  else if (code_ptr_p (ptr))
    size += code_ptr_lit_end (ptr) + 1;
  else if (rec_ptr_p (ptr))
    {
      /* The descriptor might have already been copied and thus we
	 might find a forwarding pointer in its place.
      */
      val *desc_ptr = mem_follow_fwd_ptr (val_ptr(rec_ptr_desc (ptr),3));
      size = abs (fixnum_num (desc_ptr[1])) + 1;
    }
  else
    abort ();

  new_ptr = mem_new_next;
  mem_new_next += (size+1)&~1;

  memcpy (new_ptr, ptr, size*sizeof(word));
  mem_install_fwd_ptr (ptr, new_ptr);

  return val_ptr_make (new_ptr, val_tag (v, 3));
}

val *
mem_scan (val *ptr)
{
  sword size;

  val *orig = ptr;

  if (pair_ptr_p (ptr))
      size = 2;
  else if (vec_ptr_p (ptr))
    {
      size = vec_ptr_len (ptr);
      ptr += 1;
    }
  else if (bytev_ptr_p (ptr))
    {
      ptr += (bytev_ptr_len (ptr) + 3) / 4 + 1;
      size = 0;
    }
  else if (code_ptr_p (ptr))
    {
      int b = code_ptr_lit_begin (ptr);
      int e = code_ptr_lit_end (ptr);
      size = e - b;
      ptr += b;
    }
  else if (rec_ptr_p (ptr))
    {
      /* We need to copy the descriptor here manually, since it has a
	 funny tag that the rest of the code doesn't want to see.
      */
      val desc = mem_copy (rec_ptr_desc (ptr));
      ptr[0] = rec_header_make (desc);
      size = fixnum_num (rec_ptr(desc)[0]);
      ptr += 1;
      if (size < 0)
	{
	  ptr += size;
	  size = 0;
	}
    }
  else
    abort ();

  for (int i = 0; i < size; i++)
    ptr[i] = mem_copy (ptr[i]);

  return (val *)((word)((ptr + size)+1) & ~7);
}

void debug_write (val x);
void mem_check ();

val *
mem_gc (int n)
{
#ifdef DEBUG
  mem_check ();
#endif

  mem_new_first = malloc (mem_size * 4);
  if (mem_new_first == NULL)
    abort ();

  mem_new_end = mem_new_first + mem_size;
  mem_new_next = mem_new_first;

  for (int i = 0; i < reg_len; i++)
    reg[i] = mem_copy (reg[i]);

  val *ptr = mem_new_first;
  int count = 0;
  while (ptr < mem_new_next)
    {
      ptr = mem_scan (ptr);
      count++;
    }
    
  free (mem_first);
  mem_first = mem_new_first;
  mem_end = mem_new_end;
  mem_next = mem_new_next;

  mem_new_first = NULL;

  dbg ("GC: copied %d objects, %d words (%02f%%)\n",
       count, mem_next - mem_first, (mem_next - mem_first)*100.0/mem_size);

  if (mem_new_end - mem_new_next < n)
    {
      printf ("FULL\n");
      abort ();
    }

#ifdef DEBUG
  mem_check ();
#endif

  return mem_next;
}

/* Checking the heap
   
  To track down devious low-level bugs, it is often helpful to check
  the heap for consistency.  In DEBUG mode, this is done before and
  after each garbage collection.  Together with the flag that runs the
  garbage collector before each allocation, this narrows down heap
  corruptions to a few operations.
*/

void
mem_check ()
{
  word *shadow_heap = malloc (mem_size * 4);

  /* Scan the heap once to find the starts of all objects.  This is
     used in the next pass to validate pointer values.  This first
     pass also checks that records have sensible descriptors.
  */

  memset (shadow_heap, 0, mem_size *4);

  val *ptr = mem_first;
  while (ptr < mem_next)
    {
      word size;

      if (pair_ptr_p (ptr))
	{
	  // printf ("p");
	  size = 2;
	}
      else if (vec_ptr_p (ptr))
	{
	  // printf ("v");
	  size = vec_ptr_len (ptr) + 1;
	}
      else if (bytev_ptr_p (ptr))
	{
	  // printf ("b");
	  size = (bytev_ptr_len (ptr) + 3) / 4 + 1;
	}
      else if (code_ptr_p (ptr))
	{
	  // printf ("c");
	  size = code_ptr_lit_end (ptr) + 1;
	}
      else if (rec_ptr_p (ptr))
	{
	  val desc = rec_ptr_desc (ptr);
	  if (!rec_p (desc))
	    abort ();
	  size = fixnum_num (rec_ptr (desc)[0]) + 1;
	  // printf ("r");
	}
      else
	abort ();

      word *shadow = shadow_heap + (ptr - mem_first);
      shadow_heap[ptr - mem_first] = size;

      ptr = (val *)((word)((ptr + size)+1) & ~7);

      //printf ("%d ", size);
    }
  //printf ("\n");

  /* In the second pass, we check each value in the heap.  Pointer
     values must point to objects, and we must not find headers and
     record descriptors at all.
  */

  ptr = mem_first;
  while (ptr < mem_next)
    {
      word size = shadow_heap[ptr - mem_first];
      if (size == 0)
	abort ();

      val *begin = ptr;
      val *end = ptr + size;

      if (pair_ptr_p (ptr))
	;
      else if (vec_ptr_p (ptr))
	ptr += 1;
      else if (bytev_ptr_p (ptr))
	ptr += size;
      else if (code_ptr_p (ptr))
	ptr += code_ptr_lit_begin (ptr) + 1;
      else if (rec_ptr_p (ptr))
	ptr += 1;
      else
	abort ();

      while (ptr < end)
	{
	  val v = *ptr++;
	  if (val_ptr_p (v))
	    {
	      val *p = val_ptr_any_tag (v);
	      if (p < mem_first || p > mem_end)
		abort();

	      word s = shadow_heap[p - mem_first];
	      if (s == 0)
		abort;
	      // XXX - check for consistent tags and headers
	    }
	  // XXX - check for headers and record descriptors.
	}

      ptr = (val *)((word)((end)+1) & ~7);
    }

  free (shadow_heap);
}


/* Bootstrap interpreter

   Suo is bootstrapped by letting the compiler compile itself in a
   native environment.  During this stage, the compiler is executed by
   the bootstrap interpreter.  This interpreter is not very fast and
   does not understand the full Suo language; it is just enough to run
   the compiler.

   Like all lispish systems, the bootstrap interpreter has a reader,
   writer, and evaluator.

   None of these components is recursive; no matter what program is
   executed, they only use a fixed amount of the C stack.  The
   necessary data structures for dealing with nested data structured
   and nested control flow are all allocated in the heap.
*/

/* The bootstrap interpreter uses the registers as a little stack.

   The stack is maintained via the GC_BEGIN, GC_PROTECT, GC_END, and G
   macros.  Each function (or block) should be within a GC_BEGIN /
   GC_END pair.  Each variable to protect should be announced with
   GC_PROTECT, and then used via G.

   Here is a small example that creates a list with N elements
   initialized to X:

       void
       make_list (int n, val x)
       {
         val res = nil;

         GC_BEGIN;
	 GC_PROTECT (x);
	 GC_PROTECT (res);
	 
	 for (int i = 0; i < n; i++)
	   G(res) = cons (G(x), G(res));

	 GC_END;
	 return G(res);
       }

   One particular pattern is worth pointing out specifically: Nested
   function calls with multiple arguments should be avoided.  This
   expression

      foo (bar (), G(x))

   is unsafe.  If 'bar' causes the garbage collector to run, the value
   of G(x) has changed and must thus be evaluated after calling 'bar'.
   But there is no guarantee for this; the compiler might just as well
   evaluate G(x) before calling bar.  It is thus necessary to write
   the expression like this:

      val y = bar ();
      foo (y, G(x));

   Global variables need to be protected, too.  This is done by
   allocating the first few registers for them, via GC_DECLARE_GLOBAL
   and GC_PROTECT_GLOBAL.  They can be accessed with the G macro, just
   like protected locals.  GC_PROTECT_GLOBAL must be used outside of
   any GC_BEGIN/GC_END pair.

*/

#define GC_BEGIN         int __gc_start = reg_len
#define GC_PROTECT(var)  val *G_##var = &reg[reg_len++]; G(var) = var
#define G(var)           (*G_##var)
#define GC_END           reg_len = __gc_start
#define GC_RETURN(var)   GC_END; return G(var)        

#define GC_DECLARE_GLOBAL(var) val *G_##var
#define GC_PROTECT_GLOBAL(var) G_##var = &reg[reg_len++]; G(var) = nil

/* Bootstrap primitives

   These primitives are mostly for writing the bootstrap interpreter
   itself.  They do no error checking.
*/

GC_DECLARE_GLOBAL (boot_record_type_type);
GC_DECLARE_GLOBAL (boot_string_type);
GC_DECLARE_GLOBAL (boot_symbol_type);
GC_DECLARE_GLOBAL (boot_symbols);

GC_DECLARE_GLOBAL (boot_dot_token);

val
car (val v)
{
  return pair_ptr(v)[0];
}

val
cdr (val v)
{
  return pair_ptr(v)[1];
}

void
set_car (val v, val x)
{
  pair_ptr(v)[0] = x;
}

void
set_cdr (val v, val x)
{
  pair_ptr(v)[1] = x;
}

val
cons (val a, val d)
{
  GC_BEGIN;
  GC_PROTECT (a);
  GC_PROTECT (d);

  val v = pair_alloc ();
  set_car (v, G(a));
  set_cdr (v, G(d));
  
  GC_END;
  return v;
}

val
vec_ref (val v, int i)
{
  return vec_ptr(v)[i];
}

void
vec_set (val v, int i, val x)
{
  vec_ptr(v)[i] = x;
}

val
vec_make (word len, val init)
{
  GC_BEGIN;
  GC_PROTECT (init);

  val v = vec_alloc (len);
  for (int i = 0; i < len; i++)
    vec_set (v, i, G(init));

  GC_END;
  return v;
}

unsigned char
bytev_ref_u8 (val v, int i)
{
  return bytev_ptr(v, unsigned char)[i];
}

void
bytev_set_u8 (val v, int i, unsigned char x)
{
  bytev_ptr(v, unsigned char)[i] = x;
}

val
rec_ref (val v, int i)
{
  return rec_ptr(v)[i];
}

val
rec_set (val v, int i, val x)
{
  rec_ptr(v)[i] = x;
}

int
rec_len (val v)
{
  return fixnum_num (rec_ref (rec_desc (v), 0));
}

val
rec_make (val type, ...)
{
  int n = fixnum_num (rec_ref (type, 0));

  GC_BEGIN;
  GC_PROTECT (type);
  
  int G_args = reg_len;
  va_list ap;
  va_start (ap, type);
  for (int i = 0; i < n; i++)
    reg[reg_len++] = va_arg (ap, val);

  val v = rec_alloc (n);
  rec_set_desc (v, G(type));
  for (int i = 0; i < n; i++)
    rec_set (v, i, reg[G_args+i]);

  GC_END;
  return v;
}

val
string_make (char *str)
{
  int n = strlen (str);
  val b = bytev_alloc (n);
  memcpy (bytev_ptr (b, char *), str, n);
  return rec_make (G(boot_string_type), b);
}

int
string_eq (val a, char *b)
{
  val bytes = rec_ref (a, 0);
  return (bytev_len (bytes) == strlen (b)
	  && memcmp (bytev_ptr (bytes, char *), b, bytev_len (bytes)) == 0);
}

val
intern (char *str)
{
  val s = string_make (str);
  return rec_make (G(boot_symbol_type), s);
}

val
symbol_name (val sym)
{
  return rec_ref (sym, 0);
}

/* Bootstrap initialisation
 */

void
boot_init ()
{
  GC_PROTECT_GLOBAL (boot_record_type_type);
  GC_PROTECT_GLOBAL (boot_string_type);
  GC_PROTECT_GLOBAL (boot_symbol_type);
  GC_PROTECT_GLOBAL (boot_symbols);
  GC_PROTECT_GLOBAL (boot_dot_token);

  G(boot_record_type_type) = rec_alloc (2);
  rec_set_desc (G(boot_record_type_type), G(boot_record_type_type));
  rec_ptr(G(boot_record_type_type))[0] = fixnum_make (2);
  rec_ptr(G(boot_record_type_type))[1] = nil;

  G(boot_string_type) = rec_make (G(boot_record_type_type),
				  fixnum_make (1),
				  nil);

  G(boot_symbol_type) = rec_make (G(boot_record_type_type),
				  fixnum_make (1),
				  nil);
  
  G(boot_symbols) = vec_make (511, nil);

  G(boot_dot_token) = string_make ("{dot token}");

  val x;

  x = intern ("record-type");
  rec_set (G(boot_record_type_type), 1, x);
  x = intern ("string");
  rec_set (G(boot_string_type), 1, x);
  x = intern ("symbol");
  rec_set (G(boot_symbol_type), 1, x);
}

/* Bootstrap writer

   For a computer, writing is easier than reading, and we deal with it
   first.

   The state is stored as a list of 'frames'.  Each frame contains the
   object that is being written, and the index of the element to be
   printed next.
*/

val
boot_write_push (val stack, val x, int i)
{
  val res = nil;

  GC_BEGIN;
  GC_PROTECT (stack);
  GC_PROTECT (x);
  GC_PROTECT (res);

  val y = cons (G(x), fixnum_make (i));
  G(res) = cons (y, G(stack));

  GC_END;
  return G(res);
}

const char *boot_read_whitespace = " \t\n";
const char *boot_read_delimiters = "()[]{}';";

val
boot_write_start (val stack, val x)
{
  if (fixnum_p (x))
    printf ("%d", fixnum_num (x));
  else if (chr_p (x))
    {
      word c = chr_code (x);
      printf ("#x%x", c);
    }
  else if (x == nil)
    printf ("()");
  else if (x == bool_t)
    printf ("#t");
  else if (x == bool_f)
    printf ("#f");
  else if (x == unspec)
    printf ("#unspec");
  else if (pair_p (x))
    {
      printf ("(");
      return boot_write_push (stack, x, 0);
    }
  else if (vec_p (x))
    {
      printf ("[");
      return boot_write_push (stack, x, 0);
    }
  else if (rec_p (x))
    {
      val type = rec_desc (x);
      if (type == G(boot_string_type))
	{
	  val b = rec_ref (x, 0);
	  int n = bytev_len (b);
	  printf ("\"");
	  for (int i = 0; i < n; i++)
	    {
	      unsigned char c = bytev_ref_u8 (b, i);
	      if (isprint (c))
		printf ("%c", c);
	      else
		printf ("\\x%02x", c);
	    }
	  printf ("\"");
	}
      else if (type == G(boot_symbol_type))
	{
	  val s = rec_ref (x, 0);
	  val b = rec_ref (s, 0);
	  int n = bytev_len (b);
	  for (int i = 0; i < n; i++)
	    {
	      unsigned char c = bytev_ref_u8 (b, i);
	      if (strchr (boot_read_whitespace, c)
		  || strchr (boot_read_delimiters, c)
		  || (c == '.' && n == 1))
		printf ("\\%c", c);
	      else
		printf ("%c", c);
	    }
	}
      else
	{
	  printf ("{...}");
	}
    }
  else if (bytev_p (x))
    {
      int n = bytev_len (x);
      printf ("/");
      for (int i = 0; i < n; i++)
	{
	  unsigned char c = bytev_ref_u8 (x, i);
	    printf ("%02x", c);
	}
      printf ("/");
    }
  else
    printf ("?");

  return stack;
}

void
boot_write (val x)
{
  val stack = nil;

  GC_BEGIN;
  GC_PROTECT (stack);

  G(stack) = boot_write_start (G(stack), x);
  while (G(stack) != nil)
    {
      val f = car (G(stack));
      val x = car (f);
      val i = cdr (f);

      if (pair_p (x))
	{
	  int ii = fixnum_num (i);
	  if (ii == 0)
	    {
	      val y = car (x);
	      set_cdr (f, fixnum_make (1));
	      G(stack) = boot_write_start (G(stack), y);
	    }
	  else if (ii == 1)
	    {
	      val y = cdr (x);
	      if (pair_p (y))
		{
		  printf (" ");
		  set_car (f, y);
		  set_cdr (f, fixnum_make (0));
		}
	      else if (y == nil)
		{
		  printf (")");
		  G(stack) = cdr (G(stack));
		}
	      else
		{
		  set_cdr (f, fixnum_make (2));
		  printf (" . ");
		  G(stack) = boot_write_start (G(stack), y);
		}
	    }
	  else
	    {
	      printf (")");
	      G(stack) = cdr (G(stack));
	    }
	}
      else if (vec_p (x))
	{
	  int ii = fixnum_num (i);
	  if (ii < vec_len (x))
	    {
	      val y = vec_ref (x, ii);
	      set_cdr (f, fixnum_make (ii+1));
	      if (ii > 0)
		printf (" ");
	      G(stack) = boot_write_start (G(stack), y);
	    }
	  else
	    {
	      printf ("]");
	      G(stack) = cdr (G(stack));
	    }
	}
    }
  GC_END;
}

/* Bootstrap reader.

   Like the writer, the reader stores its state in a stack of frames.
   A frame contains a indication of what kind of construct is
   currently being read, and a list of accumulated values for that
   construct.
*/

int
boot_read_skip_whitespace ()
{
  int c;
  while (true)
    {
      c = getchar ();
      if (c == ';')
	{
	  while (true)
	    {
	      c = getchar ();
	      if (c == EOF)
		return EOF;
	      if (c == '\n')
		break;
	    }
	}
      else if (c == EOF || !strchr (boot_read_whitespace, c))
	return c;
    }
}

val
boot_read_to_fixnum (val tok, int n)
{
  sword num = 0, sign = 1;
  char *ptr = bytev_ptr (tok, char), *end = ptr + n;
  
  if (*ptr == '-')
    {
      sign = -1;
      ptr++;
    }
  else if (*ptr == '+')
    {
      sign = 1;
      ptr++;
    }

  if (ptr == end)
    return bool_f;

  while (ptr < end && isdigit (*ptr))
    {
      num = 10*num + (*ptr - '0');
      if (sign*num < fixnum_min || sign*num > fixnum_max)
	{
	  printf ("number of out range\n");
	  return unspec;
	}
      ptr++;
    }

  if (ptr == end)
    return fixnum_make (sign*num);
  else
    return bool_f;
}

val
boot_read_token (int first)
{
  val tok = bytev_alloc (200);
  int n = 0, escaped = 0, any_escaped = 0;
  int c = first;

  GC_BEGIN;
  GC_PROTECT (tok);
  while (true)
    {
      if (c == EOF
	  || (!escaped
	      && (strchr (boot_read_delimiters, c)
		  || strchr (boot_read_whitespace, c))))
	{
	  ungetc (c, stdin);
	  break;
	}

      if (c == '\\')
	{
	  escaped = 1;
	  any_escaped = 1;
	}
      else
	{
	  if (bytev_len (G(tok)) < n+1)
	    {
	      val y = bytev_alloc (n+200);
	      memcpy (bytev_ptr (y, void), bytev_ptr (G(tok), void),
		      bytev_len (G(tok)));
	      G(tok) = y;
	    }
	  
	  bytev_set_u8 (G(tok), n, c);
	  n += 1;
	  escaped = 0;
	}
      c = getchar();
    }

  val res = boot_read_to_fixnum (G(tok), n);
  if (res == bool_f)
    {
      if (!any_escaped
	  && n == 1
	  && bytev_ref_u8 (G(tok), 0) == '.')
	res = G(boot_dot_token);
      else
	{
	  res = bytev_alloc (n);
	  memcpy (bytev_ptr (res, void), bytev_ptr (G(tok), void), n);
	  res = rec_make (G(boot_string_type), res);
	  res = rec_make (G(boot_symbol_type), res);
	}
    }

  GC_END;
  return res;
}

val
boot_read_string ()
{
  val tok = bytev_alloc (200);
  int n = 0, escaped = 0;
  
  GC_BEGIN;
  GC_PROTECT (tok);
  while (true)
    {
      int c = getchar();
      if (c == EOF || (c == '"' && !escaped))
	break;
      
      if (c == '\\')
	escaped = 1;
      else
	{
	  if (bytev_len (G(tok)) < n+1)
	    {
	      val y = bytev_alloc (n+200);
	      memcpy (bytev_ptr (y, void), bytev_ptr (G(tok), void),
		      bytev_len (G(tok)));
	      G(tok) = y;
	    }

	  bytev_set_u8 (G(tok), n, c);
	  n += 1;
	  escaped = 0;
	}
    }

  val res = bytev_alloc (n);
  memcpy (bytev_ptr (res, void), bytev_ptr (G(tok), void), n);
  res = rec_make (G(boot_string_type), res);

  GC_END;
  return res;
}

/* All possible constructs are listed in a static table.  That table
   contains the opening character, the closing character (if any), and
   a function to call when the construct has been read completely.
 */

val
boot_read_finish_outer (val x, int n, char *unused)
{
  if (n != 1)
    return unspec;
  else
    return car (x);
}

val
boot_read_finish_list (val x, int n, char *unused)
{
  return x;
}

val
boot_read_finish_vector (val x, int n, char *unused)
{
  GC_BEGIN;
  GC_PROTECT (x);

  val z = vec_alloc (n);
  x = G(x);
  for (int i = 0; i < n; i++)
    {
      vec_set (z, i, car (x));
      x = cdr (x);
    }

  GC_END;
  return z;
}

val
boot_read_finish_abbrev (val x, int n, char *tag)
{
  GC_BEGIN;
  GC_PROTECT (x);

  val z = intern (tag);
  z = cons (z, G(x));

  GC_END;
  return z;
}

val
boot_read_finish_sharp_list (val x, int n, char *unused)
{
  GC_BEGIN;
  GC_PROTECT(x);

  G(x) = cons (G(x), nil);
  G(x) = cons (nil, G(x));
  val z = intern ("fn");
  G(x) = cons (z, G(x));

  GC_END;
  return G(x);
}

val
boot_read_finish_sharp_vector (val x, int n, char *unused)
{
  GC_BEGIN;
  GC_PROTECT(x);

  G(x) = cons (G(x), nil);
  val z = intern ("fn");
  G(x) = cons (z, G(x));

  GC_END;
  return G(x);
}

struct boot_read_construct {
  int opener, closer;
  val (*finisher) (val elements, int n, char *data);
  char *data;
} boot_read_constructs[] = {
  { ' ', 0,   boot_read_finish_outer },
  { '(', ')', boot_read_finish_list },
  { '[', ']', boot_read_finish_vector },
  { '\'', 0,  boot_read_finish_abbrev, "quote" },
  { 1, ')', boot_read_finish_sharp_list },
  { 2, ']', boot_read_finish_sharp_vector },
  { 0 }
};

val
boot_read_start (val stack, char opener)
{
  for (int i = 0; boot_read_constructs[i].opener; i++)
    if (boot_read_constructs[i].opener == opener)
      {
	GC_BEGIN;
	GC_PROTECT(stack);
	val y = cons (fixnum_make (i), nil);
	G(stack) = cons (y, G(stack));
	GC_END;
	return G(stack);
      }
  
  return unspec;
}

int
boot_read_delimiter (val stack)
{
  return boot_read_constructs[fixnum_num(car(car(stack)))].closer;
}

void
boot_read_add (val stack, val x)
{
  val f = car (stack);

  GC_BEGIN;
  GC_PROTECT (f);
  val y = cons (x, cdr (G(f)));
  set_cdr (G(f), y);
  GC_END;
}

val
boot_read_finish (val stack)
{
  val f = car (stack);
  val y = cdr (f), x = nil;
  int n = 0;

  if (y != nil && cdr (y) != nil && car (cdr (y)) == G(boot_dot_token))
    {
      x = car (y);
      y = cdr (cdr (y));
    }

  while (y != nil)
    {
      val z = cdr (y);
      set_cdr (y, x);
      x = y;
      y = z;
      n++;
    }

  int i = fixnum_num(car(f));
  char *data = boot_read_constructs[i].data;
  return boot_read_constructs[i].finisher(x, n, data);
}

struct {
  char *sym;
  val v;
} boot_read_sharps[] = {
  { "t", bool_t },
  { "f", bool_f },

  { "@sum", fixnum_make (12) },

  NULL
};

val
boot_read_sharp_symbol (val sym)
{
  val name = symbol_name (sym);
  for (int i = 0; boot_read_sharps[i].sym; i++)
    {
      if (string_eq (name, boot_read_sharps[i].sym))
	return boot_read_sharps[i].v;
    }

  printf ("unrecognized # construct: #");
  boot_write (sym);
  printf ("\n");
  return unspec;
}

struct {
  char *sym;
  val v;
} boot_read_chars[] = {
  { "space", chr_make (' ') },
  { "nl",    chr_make ('\n') },

  NULL
};

val
boot_read_char_symbol (val sym)
{
  val name = symbol_name (sym);
  val bytes = rec_ref (name, 0);
  if (bytev_len (bytes) == 1)
    return chr_make (bytev_ptr (bytes, char)[0]);
  else
    {
      for (int i = 0; boot_read_sharps[i].sym; i++)
	{
	  if (string_eq (name, boot_read_chars[i].sym))
	    return boot_read_chars[i].v;
	}
    }

  printf ("unrecognized #\\ construct: #\\");
  boot_write (sym);
  printf ("\n");
  return unspec;
}

val
boot_read ()
{
  val x = unspec, y, stack = nil;

  GC_BEGIN;
  GC_PROTECT (stack);
  GC_PROTECT (x);

  G(stack) = boot_read_start (G(stack), ' ');

  while (G(stack) != nil)
    {
      int c = boot_read_skip_whitespace ();

      if (c == EOF)
	{
	  if (cdr(G(stack)) != nil)
	    printf ("unexpected end of input\n");
	  G(x) = unspec;
	}
      else if (c == '"')
	{
	  G(x) = boot_read_string ();
	}
      else if (c == '#')
	{
	  int c = boot_read_skip_whitespace ();
	  if (c == EOF)
	    {
	      printf ("unexpected end of input\n");
	      return unspec;
	    }
	  else if (c == '\\')
	    {
	      int c = boot_read_skip_whitespace ();
	      G(x) = boot_read_char_symbol (boot_read_token (c));
	    }
	  else if (c == '(')
	    {
	      G(stack) = boot_read_start (G(stack), 1);
	      continue;
	    }
	  else if (c == '[')
	    {
	      G(stack) = boot_read_start (G(stack), 2);
	      continue;
	    }
	  else
	    G(x) = boot_read_sharp_symbol (boot_read_token (c));
	}
      else if (strchr (boot_read_delimiters, c))
	{
	  if (c == boot_read_delimiter (G(stack)))
	    {
	      G(x) = boot_read_finish (G(stack));
	      G(stack) = cdr (G(stack));
	    }
	  else
	    {
	      G(stack) = boot_read_start (G(stack), c);
	      if (G(stack) == unspec)
		{
		  printf ("unexpected delimiter '%c'\n", c);
		  G(x) = unspec;
		}
	      else
		continue;
	    }
	}
      else
	G(x) = boot_read_token (c);
      
      if (G(x) == unspec)
	{
	  GC_END;
	  return unspec;
	}

      while (G(stack) != nil)
	{
	  boot_read_add (G(stack), G(x));
	  if (boot_read_delimiter (G(stack)) == 0)
	    {
	      G(x) = boot_read_finish (G(stack));
	      G(stack) = cdr (G(stack));
	    }
	  else
	    break;
	}
    }

  GC_END;
  return G(x);
}

/* Debugging tools
 */

void
debug_write (val x)
{
  if (val_ptr_p (x) && mem_new_first)
    x = val_ptr_make (mem_follow_fwd_ptr (val_ptr_any_tag (x)),
		      val_tag (x, 3));

  if (fixnum_p (x))
    printf ("%d", fixnum_num (x));
  else if (chr_p (x))
    {
      word c = chr_code (x);
      printf ("#x%x", c);
    }
  else if (x == nil)
    printf ("()");
  else if (x == bool_t)
    printf ("#t");
  else if (x == bool_f)
    printf ("#f");
  else if (x == unspec)
    printf ("#unspec");
  else if (pair_p (x))
    {
      printf ("(");
      while (pair_p (x))
	{
	  debug_write (car (x));
	  x = cdr (x);
	  if (pair_p (x))
	    printf (" ");
	}
      if (x != nil)
	{
	  printf (" . ");
	  debug_write (x);
	}
      printf (")");
    }
  else if (vec_p (x))
    {
      printf ("[");
      int n = vec_len (x);
      for (int i = 0; i < n; i++)
	{
	  if (i > 0)
	    printf (" ");
	  debug_write (vec_ref (x, i));
	}
      printf ("]");
    }
  else if (rec_p (x))
    printf ("{...}");
  else if (bytev_p (x))
    {
      int n = bytev_len (x);
      printf ("\"");
      for (int i = 0; i < n; i++)
	{
	  unsigned char c = bytev_ref_u8 (x, i);
	  if (isprint (c))
	    printf ("%c", c);
	  else
	    printf ("\\x%02x", c);
	}
      printf ("\"");
    }
  else
    printf ("?");
}

void
debug_regs ()
{
  for (int i = 0; i < reg_len; i++)
    {
      printf ("%3d: ", i);
      debug_write (reg[i]);
      printf ("\n");
    }
}

val
pk (char *title, val x)
{
  printf ("%s: ", title);
  debug_write (x);
  printf ("\n");
  return x;
}

/* Main

   Just for testing right now.
 */

void
main (int arg, char **argv)
{
  mem_init ();
  boot_init ();

  val x = nil, y = nil, z = nil;
  GC_BEGIN;
  GC_PROTECT (x);
  GC_PROTECT (y);
  GC_PROTECT (z);

#if 1
  while (true)
    {
      G(x) = boot_read ();
      if (G(x) == unspec)
	break;
      boot_write (G(x));
      printf ("\n");
    }
#endif

  GC_END;
}
