#include <stdio.h>

void reverse_int_array(int * data, unsigned int dataobjs)
{
  int i, t;
  
  for(i=0; i<dataobjs/2; i++)
    {
      t = *(data+i);
      *(data+i) = *(data+dataobjs-1-i);
      *(data+dataobjs-1-i) = t;
    }
}

void reverse_int_ptr_array(int **ptrs, unsigned int ptrobjs)
{
  int *t;
  int i;
  
  for(i=0; i<ptrobjs/2; i++)
    {
      t = *(ptrs+i);
      *(ptrs+i) = *(ptrs+ptrobjs-1-i);
      *(ptrs+ptrobjs-1-i) = t;
    }
}

void
reverse_int_ptr_ptrtest(int **ptrs)
{
  reverse_int_ptr_array(ptrs, 2);
  
  reverse_int_array(*(ptrs+0), 4);
  reverse_int_array(*(ptrs+1), 4);
}
