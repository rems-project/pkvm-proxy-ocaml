open Pkvm_proxy
open Pkvm_proxy_utils

module Log = (val Logs.(Src.create "pkvm_tests" |> src_log))

let setup_log () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Debug)

(* Vcpu_run can spontaneously interrupt at any point.
 * When this happens, the return (from the underlying ioctl) is 0.
 *)
let rec vcpu_run_expect ?(exit = 2) ?esr ?far ?hpfar ?disr vcpu =
  let check a = function Some x -> x = a | _ -> true in
  match Pkvm_proxy.vcpu_run vcpu with
  | 0 -> vcpu_run_expect ~exit ?esr ?far ?hpfar ?disr vcpu
  | res ->
    let f = vcpu.mem.@[vcpu_fault] in
    let ok =
      res = exit && check f.esr_el2 esr && check f.far_el2 far &&
      check f.hpfar_el2 hpfar && check f.disr_el1 disr in
    if not ok then (
      Log.err (fun k -> k "vcpu run: exit %d, fault: %a" res pp_fault_info f);
      invalid_arg "vcpu_run_expect"
    )

let main xs =
  setup_log ();
  sched_setaffinity ~thread:0 [|0|];
  xs |> List.iter (fun (name, test) ->
    Log.app (fun k -> k "-> start: %a" Fmt.(styled `Bold string) name);
    test ();
    Log.app (fun k -> k "<- done: %a" Fmt.(styled `Bold string) name));
  Log.app (fun k -> k "all done")


let t_region_share () =
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  kernel_region_share_hyp reg

let t_region_share_unshare () =
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  kernel_region_share_hyp reg;
  kernel_region_unshare_hyp reg;
  kernel_region_free reg

let t_init_deinit_vm () =
  let vm = init_vm () in
  teardown_vm vm

let t_init_deinit_vcpu () =
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  teardown_vm vm;
  teardown_vcpu vcpu

let t_vcpu_load_put () =
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu

let t_vcpu_run () =
  let code = Pkvm_asm.(asm [mov (x 30) 0xdeadfffL; ldr (x 0) (x 30)]) in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08006L ~hpfar:0xdead0L ~far:0xfffL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf

let t_guest_hvc_version () =
  let code = Pkvm_asm.(asm [
    hvc 0 VERSION_FUNC_ID;
    mov (x 30) 0xdeadfffL;
    ldr (x 0) (x 30)]
  ) in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08006L ~hpfar:0xdead0L ~far:0xfffL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf

let t_guest_hvc_mem_share () =
  let code = Pkvm_asm.(asm [
    hvc 0 (VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID (0x2000L, 0L, 0L));
    mov (x 30) 0xdeadfffL;
    ldr (x 0) (x 30)]
  ) in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  let mbuf = kernel_region_alloc 0x1000 in
  kernel_region_release mbuf;
  map_region_guest vcpu.mem mbuf 0x2000L;
  vcpu_run_expect vcpu ~esr:0x5a000000L ~hpfar:0x0L ~far:0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08006L ~hpfar:0xdead0L ~far:0xfffL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf;
  kernel_region_reclaim mbuf;
  kernel_region_free mbuf

let t_guest_hvc_mem_unshare () =
  let code = Pkvm_asm.(asm [
    hvc 0 (VENDOR_HYP_KVM_MEM_SHARE_FUNC_ID (0x2000L, 0L, 0L));
    hvc 0 (VENDOR_HYP_KVM_MEM_UNSHARE_FUNC_ID (0x2000L, 0L, 0L));
    mov (x 30) 0xdeadfffL;
    ldr (x 0) (x 30)]
  ) in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  let mbuf = kernel_region_alloc 0x1000 in
  kernel_region_release mbuf;
  map_region_guest vcpu.mem mbuf 0x2000L;
  vcpu_run_expect vcpu ~esr:0x5a000000L ~hpfar:0x0L ~far:0x0L;
  vcpu_run_expect vcpu ~esr:0x5a000000L ~hpfar:0x0L ~far:0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08006L ~hpfar:0xdead0L ~far:0xfffL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf;
  kernel_region_reclaim mbuf;
  kernel_region_free mbuf

let _ = main [
  "kernel share", t_region_share
; "kernel_share_unshare", t_region_share_unshare
; "init/deinit_vm", t_init_deinit_vm
; "init/deinit_vcpu", t_init_deinit_vcpu
; "load/put_vcpu", t_vcpu_load_put
; "run_vcpu", t_vcpu_run
; "guest_hvc_version", t_guest_hvc_version
; "guest_hvc_mem_share", t_guest_hvc_mem_share
; "guest_hvc_mem_unshare", t_guest_hvc_mem_unshare
]
