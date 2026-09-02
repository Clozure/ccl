/* Probe which fixed VAs a (possibly hardened) process may MAP_FIXED RW.
 *
 *   cc -o /tmp/probe tools/darwin-fixed-mmap-probe.c && /tmp/probe
 *   codesign --force -s - --entitlements ents.plist /tmp/probe && /tmp/probe
 *
 * macOS 26.6.2 / M4: com.apple.security.hardened-process.dyld-ro makes
 * MAP_FIXED RW at 0x200000000 (CCL STATIC_BASE_ADDRESS) fail EPERM —
 * the image loader then dies at AREA_STATIC.  See
 * doc/porting/darwin.md "Enhanced Security / MIE readiness".
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>

int main(void) {
  unsigned long addrs[] = {
    0x100000000UL,      /* 4 GB */
    0x200000000UL,      /* 8 GB — CCL STATIC_BASE_ADDRESS */
    0x400000000UL,      /* 16 GB */
    0x1000000000UL,     /* 64 GB */
    0x10000000000UL,    /* 1 TB */
    0x300000000000UL,   /* 48 TB — CCL IMAGE_BASE_ADDRESS */
  };
  for (unsigned i = 0; i < sizeof(addrs)/sizeof(*addrs); i++) {
    void *want = (void *)addrs[i];
    void *got = mmap(want, 0x4000, PROT_READ|PROT_WRITE,
                     MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
    if (got == MAP_FAILED) {
      printf("0x%012lx  FIXED-RW  FAIL  %s (errno %d)\n",
             addrs[i], strerror(errno), errno);
    } else {
      printf("0x%012lx  FIXED-RW  OK\n", addrs[i]);
      munmap(got, 0x4000);
    }
  }
  /* Where does the OS put an unhinted RW mapping? */
  void *anywhere = mmap(NULL, 0x4000, PROT_READ|PROT_WRITE,
                        MAP_PRIVATE|MAP_ANON, -1, 0);
  printf("NULL-hint anon RW -> %p\n", anywhere);
  return 0;
}
