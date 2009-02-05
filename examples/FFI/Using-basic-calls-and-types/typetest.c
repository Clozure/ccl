#include <stdio.h>

// First set of tuturial functions

void
void_void_test(void)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Exited  %s:\n", __FUNCTION__);
}

signed char
sc_sc_test(signed char data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %d\n", (signed int)data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}

unsigned char
uc_uc_test(unsigned char data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %d\n", (signed int)data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}

// Second set of tutorial functions

int
si_si_test(int data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %d\n", data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}
     
long
sl_sl_test(long data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %ld\n", data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}
     
long long
sll_sll_test(long long data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %lld\n", data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}
     
float
f_f_test(float data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %e\n", data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}
     
double
d_d_test(double data)
{
  printf("Entered %s:\n", __FUNCTION__);
  printf("Data In: %e\n", data);
  printf("Exited  %s:\n", __FUNCTION__);
  return data;
}
