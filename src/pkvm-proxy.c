#include <sys/ioctl.h>
#include <sys/mman.h>
#include <strings.h>
#include <errno.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#include "pkvm-proxy.h"

static void caml_pkvm_proxy_error(int errcode) {
  CAMLparam0();
  CAMLlocal1(err);
  const value *exn = caml_named_value("Pkvm.Proxy");
  if (exn == NULL) caml_invalid_argument("Pkvm exceptions not initialized.");
  err = caml_unix_error_of_code (errcode);
  value res = caml_alloc_small(2, 0);
  Field(res, 0) = *exn;
  Field(res, 1) = err;
  caml_raise(res);
  CAMLnoreturn;
}

static inline int __ioc_wrap(int res) {
  if (res < 0) caml_pkvm_proxy_error(errno);
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
