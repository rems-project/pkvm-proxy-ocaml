open Pkvm_proxy
open Pkvm_proxy_utils

module Log = (val Logs.(Src.create "pkvm_tests" |> src_log))

let setup_log () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Warning)

let config = Fmt.str "/payload/%s.json" Sys.argv.(0)

let pp_tag = Fmt.(styled `Bold @@ styled `Green @@ styled `Italic string)

let main xs =
  setup_log ();
  sched_setaffinity ~thread:0 [|0|];
  let select = match Pkvm_test_config.load config with
  | Ok (ll, f) ->
      Logs.set_level ~all:true ll;
      f
  | Error (`Msg msg) ->
      Log.warn (fun k -> k "`%s': %s" config msg);
      fun _ -> true
  in
  xs |> List.iteri (fun i (name, test) ->
    match select name with
    | true ->
        Log.app (fun k -> k "@.╭────────────────────@.│ start (#%d): %a@." i pp_tag name);
        test ();
        Log.app (fun k -> k "@.│ ok: %a@.╰────────────────────@." pp_tag name);

    | false ->
        Log.app (fun k -> k "@.== skip: %a@." pp_tag name)
  );
  Log.app (fun k -> k "all done")

let fault_at_0xdead =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x93c08007L && f.hpfar_el2 = 0xd0L && f.far_el2 = 0xeadL))

let trap_on_hypercall =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x5a000000L && f.hpfar_el2 = 0L && f.far_el2 = 0L))

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

let t_init_deinit_vcpus () =
  let vm = init_vm ~vcpus:2 () in
  let vcpu1 = init_vcpu vm 0
  and vcpu2 = init_vcpu vm 1 in
  teardown_vm vm;
  teardown_vcpu vcpu1;
  teardown_vcpu vcpu2

let t_vcpu_load_put () =
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu

let t_map_unmap () =
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  Bigstring.blit_from_string "whatever" (region_memory cbuf);
  assert (Bigstring.sub_string ~n:8 (region_memory cbuf) = "whatever");
  map_region_guest vcpu.mem cbuf 0x0L;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  assert (Bigstring.sub_string ~n:8 (region_memory cbuf)
          = String.init 8 (fun _ -> '\x00'));
  kernel_region_free cbuf

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
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
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
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
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
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
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
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:trap_on_hypercall;
  vcpu_run_expect vcpu ~cond:fault_at_0xdead;
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
; "vcpu init+deinit poly", t_init_deinit_vcpus
; "vcpu load+put", t_vcpu_load_put
; "guest map+unmap", t_map_unmap
; "vcpu run", t_vcpu_run
; "guest hvc version", t_guest_hvc_version
; "guest hvc mem_share", t_guest_hvc_mem_share
; "guest hvc mem_unshare", t_guest_hvc_mem_unshare
]
