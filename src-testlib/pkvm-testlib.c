#define __USE_GNU   // glibc
#define _GNU_SOURCE // musl
#include <sched.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>


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

CAMLprim value caml_sched_getaffinity(value thread) {
  CAMLparam1(thread);
  CAMLlocal1(res);
  cpu_set_t cpu_set;
  if (sched_getaffinity(Long_val(thread), sizeof(cpu_set_t), &cpu_set) < 0)
    uerror("sched_getaffinity", Nothing);
  res = caml_alloc_tuple(CPU_COUNT(&cpu_set));
  int i = 0;
  for (int c = 0; c < CPU_SETSIZE; ++c)
    if (CPU_ISSET(c, &cpu_set))
      Store_field(res, i++, Val_long(c));
  CAMLreturn(res);
}
