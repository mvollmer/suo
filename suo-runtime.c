/* suo-runtime -- lowest level runtime 
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

typedef unsigned int word;
typedef   signed int sword;
typedef         word val;

#define HEAP_SIZE (64*1024*1024)

val regs[1+256];

#define UNSWIZZLE(p,b)   (((word)p)+((word)b))

/* Correct all pointers in MEM from offsets into proper pointers by
   adding MEM to them.
*/
void
unswizzle_objects (val *mem, word n)
{
  val *ptr;
  for (ptr = mem; ptr < mem + n;)
    {
      sword size;
      char *type;
      val v = *ptr;
      
      fprintf (stderr, "on %d %08x\n", ptr-mem, v);

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
	      else if ((v & 15) == 7)
		{
		  /* stringbuf header */
		  word t = (v >> 4) & 3, s = v >> 6;
		  switch (t)
		    {
		    case 0:
		      type = "UCS-1";
		      size = (s+3)/4;
		      break;
		    case 1:
		      type = "UCS-2";
		      size = (s+1)/2;
		      break;
		    case 3:
		      type = "UCS-4";
		      size = s;
		      break;
		    }
		}
	      else if ((v & 15) == 11)
		{
		  /* bytevec header */
		  type = "bytevec";
		  size = -(((v >> 4) + 3) / 4);
		}
	      else
		{
		  /* code header */
		  type = "code";
		  size = (v >> 4) & 0xFF;
		  ptr += (v >> 12);
		}
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

      fprintf (stderr, " %s, %d words\n", type, size);

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
	ptr += size;
    }
}

void
sys (val code)
{
  fprintf (stderr, "sys %08x\n", code);
}

void
go (val code)
{
  register val *esi asm ("%esi") = (val *)code;
  regs[0] = (val)sys;
  asm ("mov %0,%%ebp\n\t lea 4(%%esi),%%eax\n\t jmp *%%eax"
       :
       : "r" (regs+1), "r" (esi));
}

void
perror_exit (const char *msg)
{
  perror (msg);
  exit (1);
}

int
main (int argc, char **argv)
{
  if (argc != 2)
    {
      write (2, "usage: suo <image>\n", 20);
      exit (1);
    }

  val *heap = malloc (HEAP_SIZE);
  if (heap == NULL)
    perror_exit ("malloc");

  int fd = open (argv[1], O_RDONLY);
  if (fd < 0)
    perror_exit (argv[1]);

  struct stat buf;
  if (fstat (fd, &buf) < 0)
    perror_exit (argv[1]);

  word start;
  sword n = read (fd, &start, 4);
  if (n != 4)
    perror_exit (argv[1]);

  n = read (fd, heap, buf.st_size-4);
  if (n < 0)
    perror_exit (argv[1]);

  unswizzle_objects (heap, n/4);

  start += (word)heap;
  go (start);
}
