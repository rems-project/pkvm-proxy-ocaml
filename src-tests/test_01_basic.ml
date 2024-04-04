open Pkvm_proxy
open Pkvm_testlib
open Pkvm_proxy_utils

let fault_at_0xdead =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x93c08007L && f.hpfar_el2 = 0xd0L && f.far_el2 = 0xeadL))

let trap_on_hypercall =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x5a000000L && f.hpfar_el2 = 0L && f.far_el2 = 0L))

let t_share_hyp = test "share_hyp" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  host_share_hyp reg

let t_share_unshare_hyp = test "host_share|unshare_hyp" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  host_share_hyp reg;
  host_unshare_hyp reg;
  kernel_region_free reg

let t_init_teardown_vm = test "init|teardown_vm" @@ fun _ ->
  let vm = init_vm () in
  teardown_vm vm

let t_init_deinit_vcpu = test "init_vcpu + deinit" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  teardown_vm vm;
  free_vcpu vcpu

let t_init_deinit_vcpus = test "init_vcpu + deinit, multiple" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  let vcpu1 = init_vcpu vm 0
  and vcpu2 = init_vcpu vm 1 in
  teardown_vm vm;
  free_vcpu vcpu1;
  free_vcpu vcpu2

let t_init_vcpus_bad = test "init_vcpu out of order" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  pkvm_expect_error (init_vcpu vm) 1;
  teardown_vm vm

let t_vcpu_load_put = test "vcpu_load|put" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu

let t_map_unmap = test "host_map_guest + host_reclaim_page" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 ~init:"whatever" in
  assert (Bigstring.sub_string ~n:8 (region_memory cbuf) = "whatever");
  host_map_guest vcpu.mem cbuf 0x0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  let zeros = String.init 8 (fun _ -> '\x00') in
  assert (Bigstring.sub_string ~n:8 (region_memory cbuf) = zeros);
  kernel_region_free cbuf

let t_map_no_memcache = test "host_map_guest with no memcache" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  pkvm_expect_error (host_map_guest ~memcache_topup:false vcpu.mem cbuf) 0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  kernel_region_free cbuf

let t_vcpu_run = test "vcpu_run" @@ fun _ ->
  let code = {%asm|
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 ~init:code in
  host_map_guest vcpu.mem cbuf 0x0L;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  kernel_region_free cbuf

let t_guest_hvc_version = test "guest_hvc: version" @@ fun _ ->
  let code = {%asm|
    movz w0, 0x8000, lsl 16
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 ~init:code in
  host_map_guest vcpu.mem cbuf 0x0L;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  kernel_region_free cbuf

let t_guest_hvc_mem_share =
  test "guest_hvc: mem_share" @@ fun _ ->
  let code = {%asm|
    movz w0, 0xc600, lsl 16
    movk w0, 0x0003
    mov w1, 0x2000
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 ~init:code in
  host_map_guest vcpu.mem cbuf 0x0L;
  let mbuf = kernel_region_alloc 0x1000 in
  host_map_guest vcpu.mem mbuf 0x2000L;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  kernel_region_free cbuf;
  host_reclaim_region mbuf;
  kernel_region_free mbuf

let t_guest_hvc_mem_unshare =
  test "guest_hvc: mem_share + mem_unshare" @@ fun _ ->
  let code = {%asm|
    movz w0, 0xc600, lsl 16
    movk w0, 0x0003
    mov w1, 0x2000
    hvc 0
    movz w0, 0xc600, lsl 16
    movk w0, 0x0004
    mov w1, 0x2000
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 ~init:code in
  host_map_guest vcpu.mem cbuf 0x0L;
  let mbuf = kernel_region_alloc 0x1000 in
  host_map_guest vcpu.mem mbuf 0x2000L;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  kernel_region_free cbuf;
  host_reclaim_region mbuf;
  kernel_region_free mbuf

let _ = main [
  t_share_hyp
; t_share_unshare_hyp
; t_init_teardown_vm
; t_init_deinit_vcpu
; t_init_deinit_vcpus
; t_init_vcpus_bad
; t_vcpu_load_put
; t_map_unmap
; t_map_no_memcache
; t_vcpu_run
; t_guest_hvc_version
; t_guest_hvc_mem_share
; t_guest_hvc_mem_unshare
]
