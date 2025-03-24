open Pkvm_proxy
open Pkvm_testlib

let t_low_hcall = test "lo hcall" @@ fun _ ->
  let vm = init_vm () in
  (* (Any) init-phase hypercall, illegal at this point. *)
  pkvm_expect_proxy_error (hvc_raw 1) [||];
  teardown_vm vm

let t_hi_hcall = test "hi hcall" @@ fun _ ->
  let vm = init_vm () in
  (* Hypercall number too large to exist. *)
  pkvm_expect_proxy_error (hvc_raw 93) [||];
  teardown_vm vm

let t_init_vm_nonsense_vcpus = test "init vm with nonsense cpus" @@ fun _ ->
  pkvm_expect_error (init_vm ~vcpus:0) ();
  pkvm_expect_error (init_vm ~vcpus:(-10)) ();
  init_vm ~vcpus:512 () |> teardown_vm;
  (* XXX pKVM has no limit on the number of vCPUs!*)
  (* init_vm ~vcpus:99999 () |> teardown_vm; *)
  ()

let t_init_nonexistent_vcpu = test "init nonexistent vcpu" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  pkvm_expect_error (init_vcpu ~index_check:false vm) 3;
  teardown_vm vm

let t_init_vcpu_backwards = test "init_vcpu out-of-order" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  pkvm_expect_error (init_vcpu vm) 1;
  teardown_vm vm

let t_vcpu_load_reload = test "load_vcpu reload" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_load vcpu;
  vcpu_load vcpu;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu

let t_vcpu_load_overwrite = test "load_vcpu load overwrite" @@ fun _ ->
  let vm = init_vm ~vcpus:2 () in
  let vcpu0 = init_vcpu vm 0 in
  let vcpu1 = init_vcpu vm 1 in
  vcpu_load vcpu0;
  vcpu_load vcpu1;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu0;
  free_vcpu vcpu1

let t_vcpu_load_no_handle = test "load_vcpu bad vm handle" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load { vcpu with vm = { vm with handle = 99999 } };
  teardown_vm vm;
  free_vcpu vcpu

let t_vcpu_load_bad_index = test "load_vcpu bad index" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load { vcpu with idx = 999 };
  teardown_vm vm;
  free_vcpu vcpu

let t_vcpu_put_twice = test "vcpu_put twice" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu

let t_share_hyp_twice = test "host_share_hyp twice" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  host_share_hyp mem;
  pkvm_expect_error host_share_hyp mem;
  host_unshare_hyp mem;
  Region.free mem

let t_share_guest_twice = test "host_map_guest twice" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu mem 0x0L;
  pkvm_expect_error (host_map_guest vcpu mem) 0x4000L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  Region.free mem

let t_share_guest_no_vcpu = test "host_map_guest no vcpu" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  vcpu_put ();
  pkvm_expect_error (host_map_guest vcpu mem) 0x0L;
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  Region.free mem

let t_share_hyp_then_guest =
  test "host_share_hyp then host_map_guest" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_share_hyp mem;
  pkvm_expect_error (host_map_guest vcpu mem) 0x0L;
  host_unshare_hyp mem;
  host_map_guest vcpu mem 0x0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  Region.free mem

let t_share_guest_then_hyp =
  test "host_map_guest then host_share_hyp" @@ fun _ ->
  let mem = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu mem 0x0L;
  pkvm_expect_error host_share_hyp mem;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem;
  Region.free mem

let t_guest_share_same_addr =
  test "host_map_guest addr clash" @@ fun _ ->
  let mem1 = Region.alloc 0x1000 in
  let mem2 = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu mem1 0x0L;
  pkvm_expect_error (host_map_guest vcpu mem2) 0x0L;
  host_map_guest vcpu mem2 0x1000L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region mem1;
  host_reclaim_region mem2;
  Region.free mem1;
  Region.free mem2

(* TODO
   share failures:
    - share hyp memory hole
    - share hyp io address
    - share guest memory hole
    - share guest io address
*)

let _ = main [
  t_low_hcall
; t_hi_hcall
; t_init_vm_nonsense_vcpus
; t_init_nonexistent_vcpu
; t_init_vcpu_backwards
; t_vcpu_load_reload
; t_vcpu_load_overwrite
; t_vcpu_load_no_handle
; t_vcpu_load_bad_index
; t_vcpu_put_twice
; t_share_hyp_twice
; t_share_guest_twice
; t_share_guest_no_vcpu
; t_share_hyp_then_guest
; t_share_guest_then_hyp
; t_guest_share_same_addr
]
