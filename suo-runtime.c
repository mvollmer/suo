/* suo-runtime -- lowest level runtime 
 */

#include <stdio.h>
#include <stdarg.h>
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

val
sys (int n_args, val first)
{
  int i;
  val *args = &first;

  fprintf (stderr, "sys %d", n_args);
  for (i = 0; i < n_args; i++)
    fprintf (stderr, " %08x", args[i]);
  fprintf (stderr, "\n");

  if (n_args > 0)
    {
      switch (args[0])
	{
	case 1:
	  fprintf (stderr, "HALT\n");
	  exit (0);
	case 5:
	  fprintf (stderr, "debug\n");
	  break;
	default:
	  fprintf (stderr, "unknown syscall\n");
	  abort ();
	}
    }
  
  return 0x0000000a;
}

void
go (val *code, val *free)
{
  register val *esi asm ("%esi") = code;
  register val *edi asm ("%edi") = free;
  regs[0] = (val)sys;
  asm ("mov %0,%%ebp\n\t lea 4(%%esi),%%eax\n\t jmp *%%eax"
       :
       : "r" (regs+1), "r" (esi), "r" (edi));
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

  ssize_t n = read (fd, heap, buf.st_size);
  if (n < 0)
    perror_exit (argv[1]);

  unswizzle_objects (heap, n/4);

  go (heap, heap + n/4);
}
