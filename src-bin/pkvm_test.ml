open Pkvm_proxy
open Pkvm_proxy_utils

module Log = (val Logs.(Src.create "pkvm_tests" |> src_log))

let setup_log () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Warning)

let config = Fmt.str "/payload/%s.json" Sys.argv.(0)

let pp_name = Fmt.(styled `Bold string)
let main xs =
  setup_log ();
  sched_setaffinity ~thread:0 [|0|];
  let select = match Pkvm_test_config.load config with
  | Ok (ll, select) ->
      Logs.set_level ~all:true ll;
      select
  | Error (`Msg msg) ->
      Log.warn (fun k -> k "`%s': %s" config msg);
      fun _ -> true
  in
  xs |> List.iter (fun (name, test) ->
    match select name with
    | true ->
        Log.app (fun k -> k "-> start: %a" pp_name name);
        test ();
        Log.app (fun k -> k "<- done: %a" pp_name name);
    | false -> Log.app (fun k -> k "** skip: %a" pp_name name)
  );
  Log.app (fun k -> k "all done")


(* Vcpu_run can spontaneously interrupt at any point.
 * When this happens, the return (from the underlying ioctl) is 0.
 *)
let rec vcpu_run_expect ?(exit = 2) ?esr ?far ?hpfar ?disr vcpu =
  let check a = function Some x -> x = a | _ -> true in
  let pp_x lbl ppf = function Some a -> Fmt.pf ppf "%s 0x%Lx" lbl a | _ -> () in
  match Pkvm_proxy.vcpu_run vcpu with
  | 0 -> vcpu_run_expect ~exit ?esr ?far ?hpfar ?disr vcpu
  | res ->
    let f = vcpu.mem.@[vcpu_fault] in
    let ok =
      res = exit && check f.esr_el2 esr && check f.far_el2 far &&
      check f.hpfar_el2 hpfar && check f.disr_el1 disr in
    if not ok then (
      Log.err (fun k ->
        k "@[vcpu_run:@ exit %d (expect %d)@ fault %a@ expect %a, %a, %a, %a"
        res exit pp_fault_info f
        (pp_x "esr") esr (pp_x "far") far (pp_x "hpfar") hpfar (pp_x "disr") disr);
      invalid_arg "vcpu_run_expect")

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
  let code = {%asm|
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08007L ~hpfar:0xd0L ~far:0xeadL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf

let t_guest_hvc_version () =
  let code = {%asm|
    movz w0, 0x8000, lsl 16
    hvc 0
    movz x30, 0xdead
    ldr x0, [x30]
  |} in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08007L ~hpfar:0xd0L ~far:0xeadL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf

let t_guest_hvc_mem_share () =
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
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  let mbuf = kernel_region_alloc 0x1000 in
  kernel_region_release mbuf;
  map_region_guest vcpu.mem mbuf 0x2000L;
  vcpu_run_expect vcpu ~esr:0x5a000000L ~hpfar:0x0L ~far:0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08007L ~hpfar:0xd0L ~far:0xeadL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf;
  kernel_region_reclaim mbuf;
  kernel_region_free mbuf

let t_guest_hvc_mem_unshare () =
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
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string code (region_memory cbuf);
  map_region_guest vcpu.mem cbuf 0x0L;
  let mbuf = kernel_region_alloc 0x1000 in
  kernel_region_release mbuf;
  map_region_guest vcpu.mem mbuf 0x2000L;
  vcpu_run_expect vcpu ~esr:0x5a000000L ~hpfar:0x0L ~far:0x0L;
  vcpu_run_expect vcpu ~esr:0x5a000000L ~hpfar:0x0L ~far:0x0L;
  vcpu_run_expect vcpu ~esr:0x93c08007L ~hpfar:0xd0L ~far:0xeadL;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf;
  kernel_region_reclaim mbuf;
  kernel_region_free mbuf

let _ = main [
  "kernel share", t_region_share
; "kernel share+unshare", t_region_share_unshare
; "vm init+deinit", t_init_deinit_vm
; "vcpu init+deinit", t_init_deinit_vcpu
; "vcpu load+put", t_vcpu_load_put
; "vcpu run", t_vcpu_run
; "guest hvc version", t_guest_hvc_version
; "guest hvc mem_share", t_guest_hvc_mem_share
; "guest hvc mem_unshare", t_guest_hvc_mem_unshare
]
