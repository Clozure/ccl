#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int
(*cclmain)();

int
main(int argc, char *argv[], char *envp, void *auxv)
{
  char buf[PATH_MAX], *path, *lastslash;
  int n, prefixlen;
  void *libhandle;

  if ((n = readlink("/proc/self/exe", buf, PATH_MAX)) > 0) {
    path = malloc(n+4+3);
    buf[n] = 0;
    lastslash = strrchr(buf,'/');
    if (lastslash) {
      lastslash++;
      prefixlen = lastslash-buf;
      strncpy(path,buf,prefixlen);
      path[prefixlen] = 0;
      strcat(path,"lib");
      strcat(path,lastslash);
      strcat(path,".so");
    } else {
      memmove(path,"lib",3);
      memmove(path+3,buf,n);
      memmove(path+3+n,".so",3);
      path[n+3+3] = 0;
    }
    libhandle = dlopen(path,RTLD_GLOBAL|RTLD_NOW);
    if (libhandle != NULL) {
      cclmain = dlsym(libhandle, "cclmain");
      if (cclmain != NULL) {
        return cclmain(argc,argv,envp, auxv);
      } else {
        fprintf(stderr, "Couldn't resolve library entrpoint.\n");
      }
    } else {
      fprintf(stderr, "Couldn't open shared library %s : %s\n",
              path, dlerror());
    }
    return 1;
  }
}




  
