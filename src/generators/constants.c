#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "../pkvm-proxy.h"
#include "../hypercalls.h"

#define for_each_hcall(N, V) \
  char N[1024]; \
  for (struct __kv *_kv = smccc_func_numbers; _kv->k; ++_kv) { \
    int V = _kv->v; \
    memcpy(N, _kv->k, strlen(_kv->k) + 1);

#define endfor_each_hcall }


int main(void) {

  printf("open Pkvm_types\n");

  /* Various compile-time constants. */
  printf("let sizeof_void_p = %lu\n", sizeof(void *));
  printf("let sizeof_int = %lu\n", sizeof(int));
  printf("let sizeof___u64 = %lu\n", sizeof(__u64));

  /* We should ask the kernel about struct kvm_hyp_memcache metrics instead. */
  printf("let sizeof_hprox_memcache = %lu\n", sizeof(struct hprox_memcache));
  printf("let memcache_head_off = %lu\n", __builtin_offsetof(struct hprox_memcache, head));
  printf("let memcache_nr_off = %lu\n", __builtin_offsetof(struct hprox_memcache, nr_pages));

  /* Hypercalls. */
  printf("let smccc_func_number : type a. a host_smccc_func -> _ = function\n");
  for_each_hcall(name, num)
    name[0] = toupper(name[0]);
    printf("| %s _ -> Some %d\n", name, num);
  endfor_each_hcall
  printf("| _ -> None\n");
  printf("[@@warning \"-28\"]\n");

  return 0;
}
