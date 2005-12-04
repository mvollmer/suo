int
foo (void (*x)(int))
{
  asm ("push %eax");
  x(12);
  return 0;
}
