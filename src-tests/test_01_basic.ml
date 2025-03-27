open Pkvm_proxy
open Pkvm_testlib
open Pkvm_proxy_utils

let fault_at_0xdead =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x93c08007L && f.hpfar_el2 = 0xd0L && f.far_el2 = 0xeadL))

let trap_on_hypercall =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x5a000000L && f.hpfar_el2 = 0L && f.far_el2 = 0L))

let push_memcache ?(offset = 0) (head, nr) region =
  if offset < 0 then invalid_arg "push_memcache: negative offset";
  let _, phys = Region.addr region in
  Bigstring.set_int64 (Region.memory region) offset head;
  (Int64.(phys + of_int offset), nr + 1)

let t_share_unshare_hyp = test "host_share|unshare_hyp" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  host_share_hyp mem;
  host_unshare_hyp mem;
  Region.free mem

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

let t_vcpu_load_put = test "vcpu_load|put" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu

let t_vcpu_sync_state = test "vcpu_sync_state" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_sync_state ();
  vcpu_load vcpu;
  vcpu_sync_state ();
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu

let t_map_unmap = test "host_map_guest + host_reclaim_page" @@ fun _ ->
  let mem = Region.alloc 0x1000 ~init:"whatever" in
  assert (Bigstring.sub_string ~n:8 (Region.memory mem) = "whatever");
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu mem 0x0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  let zeros = String.init 8 (fun _ -> '\x00') in
  assert (Bigstring.sub_string ~n:8 (Region.memory mem) = zeros);
  Region.free mem

let t_map_no_memcache = test "host_map_guest with no memcache" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  pkvm_expect_error (host_map_guest ~topup_memcache:false vcpu mem) 0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  Region.free mem

let t_map_some_memcache = test "host_map_guest with some memcache" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  topup_vcpu_memcache vcpu 1;
  pkvm_expect_error (host_map_guest ~topup_memcache:false vcpu mem) 0L;
  topup_vcpu_memcache vcpu 10;
  host_map_guest ~topup_memcache:false vcpu mem 0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  Region.free mem

let t_map_memcache_manual = test "host_map_guest with manual memcache" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  let mc_pages = List.init 10 (fun _ -> Region.alloc 0x1000 ~release:true) in
  vcpu_load vcpu;
  let mc = List.fold_left push_memcache vcpu.mem.@[vcpu_memcache] mc_pages in
  vcpu.mem.@[vcpu_memcache] <- mc;
  host_map_guest ~topup_memcache:false vcpu mem 0L;
  vcpu_put ();
  teardown_vm ~free_memcache:false vm;
  free_vcpu ~free_memcache:false vcpu;
  host_reclaim_region mem;
  Region.free mem;
  List.iter Region.close mc_pages

let t_adjust_pc = test "adjust_pc" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_adjust_pc vcpu;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu

let t_timer_set_cntvoff = test "timer_set_cntvoff" @@ fun _ ->
  timer_set_cntvoff 0L

let t_vcpu_run = test "vcpu_run" @@ fun _ ->
  let exe = Region.alloc 0x1000 ~init: {%asm|
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu exe 0x0L;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region exe;
  Region.free exe

let t_vcpu_run_n = test "vcpu_run_n" @@ fun _ ->
  let exe = Region.alloc 0x1000 ~init: {%asm|
    movz w0, 0x8000, lsl 16
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vcpus = 4 in
  let vm = init_vm ~vcpus () in
  let vcpus = List.init vcpus (init_vcpu vm) in
  let vcpu0 = List.hd vcpus in
  vcpu_load vcpu0;
  host_map_guest vcpu0 exe 0x0L;
  vcpu_put ();
  vcpus |> List.iter (fun vcpu ->
    vcpu_load vcpu;
    vcpu_run_expect vcpu ~cond:fault_at_0xdead;
    vcpu_put ());
  teardown_vm vm;
  List.iter free_vcpu vcpus;
  host_reclaim_region exe;
  Region.free exe

let t_vcpu_run_fpu = test "vcpu_run fpu" @@ fun _ ->
  let exe = Region.alloc 0x1000 ~init: {%asm|
    mrs x1, cpacr_el1
    mov x0, #(3 << 20)
    orr x0, x1, x0
    msr cpacr_el1, x0
    fmov d10, 1
    fadd d10, d10, d10
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu exe 0x0L;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region exe;
  Region.free exe

let t_guest_hvc_version = test "guest_hvc: version" @@ fun _ ->
  let exe = Region.alloc 0x1000 ~init: {%asm|
    movz w0, 0x8000, lsl 16
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu exe 0x0L;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region exe;
  Region.free exe

let t_guest_hvc_nonsense = test "guest_hvc: nonsense" @@ fun _ ->
  let exe = Region.alloc 0x1000 ~init: {%asm|
    movz w0, 0xa0a0, lsl 16
    movk w0, 0xfbfb
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu exe 0x0L;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region exe;
  Region.free exe

let t_guest_hvc_mem_share =
  test "guest_hvc: mem_share" @@ fun _ ->
  let mem = Region.alloc 0x1000
  and exe = Region.alloc 0x1000 ~init: {%asm|
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
  host_map_guest vcpu exe 0x0L;
  host_map_guest vcpu mem 0x2000L;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region exe;
  host_reclaim_region mem;
  Region.free exe;
  Region.free mem

let t_guest_hvc_mem_unshare =
  test "guest_hvc: mem_share + mem_unshare" @@ fun _ ->
  let mem = Region.alloc 0x1000
  and exe = Region.alloc 0x1000 ~init: {%asm|
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
  host_map_guest vcpu exe 0x0L;
  host_map_guest vcpu mem 0x2000L;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region exe;
  host_reclaim_region mem;
  Region.free exe;
  Region.free mem

(* Tests region resource leaks. About 2500 spins leave a 512M machine without
   memory. *)
let t_burn_in spins = test "spin and do not leak" @@ fun _ ->
  for i = 1 to spins do
    Fmt.pr "%a %d@." Fmt.(styled (`Fg (`Hi `Red)) string) "->> cycle:" i;
    let vm = init_vm ~vcpus:10 () in
    let vcpus = List.init 10 (init_vcpu vm) in
    teardown_vm vm;
    List.iter free_vcpu vcpus
  done

let _ = main [
  t_share_unshare_hyp;
  t_init_teardown_vm;
  t_init_deinit_vcpu;
  t_init_deinit_vcpus;
  t_vcpu_load_put;
  t_vcpu_sync_state;
  t_map_unmap;
  t_map_no_memcache;
  t_map_some_memcache;
  t_map_memcache_manual;
  t_adjust_pc;
  t_timer_set_cntvoff;
  t_vcpu_run;
  t_vcpu_run_n;
  t_vcpu_run_fpu;
  t_guest_hvc_version;
  t_guest_hvc_nonsense;
  t_guest_hvc_mem_share;
  t_guest_hvc_mem_unshare;
  t_burn_in 0;
]
