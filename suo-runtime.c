/* Welcome.

   Suo is little programming environment that is meant to be fun to
   use, fun to write, and fun to learn about.
*/

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

/* Data types and representation.
 
   Suo knows about the following kinds of values: pairs, vectors, byte
   vectors, records, code blocks, small integers, characters,
   booleans, the empty list. and the 'unspecified' value.

   All of these values are represented as 32 bit (or maybe 64 bit)
   words.  Some of them are pointers into a big heap of words.  Unused
   words in that heap are automatically found and reused for new
   values.

   We use the types 'word' and 'sword' when working with raw bits, and
   the type 'val' when working with the values those bits represent.

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

#define FIXNUM_P(v)        (((v)&3)==0)
#define FIXNUM_VAL(v)      (((sword)(v))>>2)
#define MAKE_FIXNUM(n)     (((word)(n))<<2)

#define CHAR_P(v)          (((v)&7)==6)
#define CHAR_VAL(v)        (((word)(v))>>3)
#define MAKE_CHAR(n)       (((word)(n))<<3|6)

#define HEAP_P(v)          (((v)&3)==1)
#define FIELD_LOC(o,i)     (((val *)(o-1))+i)
#define FIELD_REF(o,i)     (((val *)(o-1))[i])
#define FIELD_SET(o,i,v)   (((val *)(o-1))[i]=(v))

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
