#include <sys/ioctl.h>
#include <sys/mman.h>
#include <strings.h>

#define __USE_GNU   // glibc
#define _GNU_SOURCE // musl
#include <sched.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#include "pkvm-proxy.h"

CAMLprim value caml_sizes(void) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_tuple(4);
  Store_field(res, 0, Val_long(sizeof(void *)));
  Store_field(res, 1, Val_long(sizeof(int)));
  Store_field(res, 2, Val_long(sizeof(__u64)));
  Store_field(res, 3, Val_long(sizeof(struct hprox_memcache)));
  CAMLreturn(res);
}

static inline int __ioc_wrap(int res) {
  if (res < 0) uerror("ioctl", Nothing);
  return Val_long(res);
}

CAMLprim value caml_pkvm_ioctl (value fd, value req) {
  CAMLparam2(fd, req);
  CAMLreturn(__ioc_wrap(ioctl(Long_val(fd), Long_val(req))));
}

CAMLprim value caml_pkvm_ioctl_long (value fd, value req, value arg) {
  CAMLparam3(fd, req, arg);
  CAMLreturn(__ioc_wrap(ioctl(Long_val(fd), Long_val(req), Long_val(arg))));
}

CAMLprim value caml_pkvm_ioctl_ptr (value fd, value req, value arg) {
  CAMLparam3(fd, req, arg);
  CAMLreturn(__ioc_wrap(ioctl(Long_val(fd), Long_val(req), Caml_ba_data_val(arg))));
}

/* Assumes _IOC_{NONE,WRITE,READ} haven't been redefined by arch, since we use
 * constructor indices.
 */
CAMLprim value caml__IOC(value dir, value type, value nr, value size) {
  CAMLparam4(dir, type, nr, size);
  CAMLreturn(Val_long(_IOC(Long_val(dir), Long_val(type), Long_val(nr), Long_val(size))));
}

extern value caml_unix_mapped_alloc(int, int, void *, intnat *);

/* XXX Works around hyp-proxy not implementing stat.
 * It's super unsafe because the fd size is provided as an argument and cannot
 * be checked.
 */
CAMLprim value caml_super_unsafe_ba_mmap(value fd, value size) {
  CAMLparam2(fd, size);
  intnat sz = Long_val(size);
  void *addr = mmap(NULL, sz, PROT_READ|PROT_WRITE, MAP_SHARED, Long_val(fd), 0);
  if (addr == (void *) MAP_FAILED) uerror("bad_map_file", Nothing);
  CAMLreturn(caml_unix_mapped_alloc(CAML_BA_UINT8|CAML_BA_C_LAYOUT, 1, addr, &sz));
}

CAMLprim value caml_ba_munmap(value arr) {
  CAMLparam1(arr);
  if (munmap(Caml_ba_data_val(arr), caml_ba_byte_size(Caml_ba_array_val(arr))) != 0)
    uerror("munmap", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ba_bzero(value arr) {
  CAMLparam1(arr);
  bzero(Caml_ba_data_val(arr), caml_ba_byte_size(Caml_ba_array_val(arr)));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_sched_setaffinity(value thread, value cpus) {
  CAMLparam2(thread, cpus);
  cpu_set_t cpu_set;
  CPU_ZERO(&cpu_set);
  for (unsigned int i = 0; i < Wosize_val(cpus); ++i)
    CPU_SET(Long_val(Field(cpus, i)), &cpu_set);
  if (sched_setaffinity(Long_val(thread), sizeof(cpu_set_t), &cpu_set) < 0)
    uerror("sched_setaffinity", Nothing);
  CAMLreturn(Val_unit);
}
