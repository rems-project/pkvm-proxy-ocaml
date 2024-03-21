open Pkvm_proxy
open Pkvm_proxy_utils
open Pkvm_testlib

let fault_at_0xdead =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x93c08007L && f.hpfar_el2 = 0xd0L && f.far_el2 = 0xeadL))

let trap_on_hypercall =
  Cond.(exit_is 2 &&& fault (fun f ->
    f.esr_el2 = 0x5a000000L && f.hpfar_el2 = 0L && f.far_el2 = 0L))

let t_region_share = test "kernel share" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  kernel_region_share_hyp reg

let t_region_share_unshare = test "kernel share+unshare" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  kernel_region_share_hyp reg;
  kernel_region_unshare_hyp reg;
  kernel_region_free reg

let t_init_deinit_vm = test "vm init+deinit" @@ fun _ ->
  let vm = init_vm () in
  teardown_vm vm

let t_init_deinit_vcpu = test "vcpu init+deinit" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  teardown_vm vm;
  teardown_vcpu vcpu

let t_init_deinit_vcpus = test "vcpu init+deinit poly" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  let vcpu1 = init_vcpu vm 0
  and vcpu2 = init_vcpu vm 1 in
  teardown_vm vm;
  teardown_vcpu vcpu1;
  teardown_vcpu vcpu2

let t_init_vcpus_bad = test "vcpu init fail" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  match init_vcpu vm 1 with
  | _ -> failwith "Expected exception"
  | exception _ -> teardown_vm vm

let t_vcpu_load_put = test "vcpu load+put" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu

let t_map_unmap = test "guest map+unmap" @@ fun _ ->
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

let t_vcpu_run = test "vcpu run" @@ fun _ ->
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

let t_guest_hvc_version = test "guest hvc version" @@ fun _ ->
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

let t_guest_hvc_mem_share = test "guest hvc mem_share" @@ fun _ ->
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

let t_guest_hvc_mem_unshare = test "guest hvc mem_unshare" @@ fun _ ->
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

let t_vcpu_load_put_par_1 = test "vcpu load+put parallel 1" @@ fun _ ->
  let cpus = 4 in
  let vm = init_vm ~vcpus:cpus () in
  let vcpus = Array.init cpus (init_vcpu vm) in
  let _ = spawnv ~cpus @@ fun i ->
    for _ = 1 to 10 do
      vcpu_load vcpus.(i);
      vcpu_put ()
    done
  in
  teardown_vm vm;
  Array.iter teardown_vcpu vcpus

(* Dance around initialising VCPUs on separate threads, which must be done in
   order. This is only for testing — much saner to init them ahead of time on the
   main thread. *)
let t_vcpu_load_put_par_2 = test "vcpu load+put parallel 2" @@ fun _ ->
  let cpus = 4 in
  let vm = init_vm ~vcpus:cpus () in
  let sem = Array.init cpus @@ fun _ -> Semaphore.Binary.make false in
  let vcpus = Array.init cpus @@ fun _ -> None in
  let threads = List.init cpus @@ fun cpu ->
    spawn ~cpu @@ fun () ->
      Semaphore.Binary.acquire sem.(cpu);
      let vcpu = init_vcpu vm cpu in
      Semaphore.Binary.release sem.((cpu + 1) mod cpus);
      vcpus.(cpu) <- Some vcpu;
      for _ = 1 to 10 do
        vcpu_load vcpu;
        vcpu_put ()
      done in
  Semaphore.Binary.release sem.(0);
  List.iter join threads;
  teardown_vm vm;
  Array.iter (function (Some vcpu) -> teardown_vcpu vcpu | _ -> assert false) vcpus

let _ = main [
  t_region_share
; t_region_share_unshare
; t_init_deinit_vm
; t_init_deinit_vcpu
; t_init_deinit_vcpus
; t_init_vcpus_bad
; t_vcpu_load_put
; t_map_unmap
; t_vcpu_run
; t_guest_hvc_version
; t_guest_hvc_mem_share
; t_guest_hvc_mem_unshare
; t_vcpu_load_put_par_1
; t_vcpu_load_put_par_2
]
