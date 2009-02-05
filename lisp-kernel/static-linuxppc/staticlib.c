typedef struct  {
  char *name;
  void *(*func)();
} external_function;

#define NULL ((void *)0)
#include "external-functions.h"

int
string_compare(char *a, char *b)
{
  char ch;

  while (ch = *a++) {
    if (*b++ != ch) {
      return 1;
    }
  }
  return !!*b;
}

      
void *
dlsym(void *handle, char *name)
{
  external_function *p;
  char *fname;

  for (p = external_functions; fname = p->name; p++) {
    if (!string_compare(name, fname)) {
      return (void *)(p->func);
    }
  }
  return NULL;
}

void *
dlopen(char *path, int mode)
{
  return NULL;
}

void *
dlerror()
{
  return (void *)"No shared library support\n";
}

void *
dlclose()
{
  return NULL;
}
