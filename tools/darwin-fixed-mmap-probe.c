/* Probe which fixed VAs a (possibly hardened) process may MAP_FIXED RW,
 * and what already occupies them (VM region protection + user tag only —
 * no paths, no personal data).
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
#include <mach/mach.h>
#include <mach/mach_vm.h>

static const char *
tag_name(unsigned tag)
{
  switch (tag) {
  case 0:  return "";
  case VM_MEMORY_MALLOC: return " malloc";
  case VM_MEMORY_STACK: return " stack";
  case VM_MEMORY_DYLD: return " dyld";
  case VM_MEMORY_DYLD_MALLOC: return " dyld-malloc";
  case VM_MEMORY_OS_ALLOC_ONCE: return " os-alloc-once";
  default: return " ?";
  }
}

static void
region_info(unsigned long where)
{
  mach_vm_address_t addr = where;
  mach_vm_size_t size = 0;
  natural_t depth = 64;
  vm_region_submap_info_data_64_t info;
  mach_msg_type_number_t count = VM_REGION_SUBMAP_INFO_COUNT_64;
  kern_return_t kr = mach_vm_region_recurse(mach_task_self(), &addr, &size,
                                            &depth,
                                            (vm_region_recurse_info_t)&info,
                                            &count);
  if (kr != KERN_SUCCESS) {
    printf("0x%012lx  region: none at or above (kr=%d)\n", where, kr);
    return;
  }
  printf("0x%012lx  region: [0x%llx..0x%llx) prot=%d max=%d tag=%u%s%s\n",
         where, (unsigned long long)addr, (unsigned long long)(addr + size),
         info.protection, info.max_protection, info.user_tag,
         tag_name(info.user_tag),
         (addr <= where && where < addr + size) ? " (covers)" : " (above)");
}

int main(void) {
  unsigned long addrs[] = {
    0x100000000UL,      /* 4 GB */
    0x200000000UL,      /* 8 GB — CCL STATIC_BASE_ADDRESS */
    0x400000000UL,      /* 16 GB */
    0x1000000000UL,     /* 64 GB */
    0x10000000000UL,    /* 1 TB */
    0x300000000000UL,   /* 48 TB — CCL IMAGE_BASE_ADDRESS */
  };
  unsigned i;

  for (i = 0; i < sizeof(addrs)/sizeof(*addrs); i++) {
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
  printf("--\n");
  for (i = 0; i < sizeof(addrs)/sizeof(*addrs); i++) {
    region_info(addrs[i]);
  }
  /* Where does the OS put an unhinted RW mapping? */
  {
    void *anywhere = mmap(NULL, 0x4000, PROT_READ|PROT_WRITE,
                          MAP_PRIVATE|MAP_ANON, -1, 0);
    printf("NULL-hint anon RW -> %p\n", anywhere);
  }
  return 0;
}
