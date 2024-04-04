open Pkvm_proxy
open Pkvm_testlib

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
  t_share_hyp_twice
; t_share_guest_twice
; t_share_hyp_then_guest
; t_share_guest_then_hyp
; t_guest_share_same_addr
]
