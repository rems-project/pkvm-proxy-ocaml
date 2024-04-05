#include <sys/ioctl.h>
#include <fcntl.h>
#include <linux/kcov.h>

#include <caml/callback.h>
#include <caml/unixsupport.h>

#define KCOV_INIT_HYP_TRACE   _IOR('c', 2, unsigned long)
#define KCOV_ENABLE_HYP       (0x1ull << 48)
#define KCOV_ENABLE_HYP_ONLY  (0x1ull << 49)

CAMLprim value caml_kcov_init(value fd, value cover_size) {
  CAMLparam2(fd, cover_size);
  if (ioctl(Long_val(fd), KCOV_INIT_HYP_TRACE, Long_val(cover_size)))
    uerror("ioctl(kcov/init)", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_kcov_enable(value fd) {
  CAMLparam1(fd);
  if (ioctl(Long_val(fd), KCOV_ENABLE, KCOV_TRACE_PC | KCOV_ENABLE_HYP | KCOV_ENABLE_HYP_ONLY))
    uerror("ioctl(kcov/enable)", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_kcov_disable(value fd) {
  CAMLparam1(fd);
  if (ioctl(Long_val(fd), KCOV_DISABLE, 0))
    uerror("ioctl(kcov/disable)", Nothing);
  CAMLreturn(Val_unit);
}
