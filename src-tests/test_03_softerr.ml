open Pkvm_proxy
open Pkvm_testlib

let t_share_hyp_twice = test "host_share_hyp twice" @@ fun _ ->
  let reg = Region.alloc 0x1000 in
  host_share_hyp reg;
  pkvm_expect_error host_share_hyp reg;
  host_unshare_hyp reg;
  Region.free reg

let t_share_guest_twice = test "host_map_guest twice" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = Region.alloc 0x1000 in
  host_map_guest vcpu cbuf 0x0L;
  pkvm_expect_error (host_map_guest vcpu cbuf) 0x4000L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region cbuf;
  Region.free cbuf

let t_share_hyp_then_guest =
  test "host_share_hyp then host_map_guest" @@ fun _ ->
  let reg = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_share_hyp reg;
  pkvm_expect_error (host_map_guest vcpu reg) 0x0L;
  host_unshare_hyp reg;
  host_map_guest vcpu reg 0x0L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region reg;
  Region.free reg

let t_share_guest_then_hyp =
  test "host_map_guest then host_share_hyp" @@ fun _ ->
  let reg = Region.alloc 0x1000 in
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  host_map_guest vcpu reg 0x0L;
  pkvm_expect_error host_share_hyp reg;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region reg;
  Region.free reg

let t_guest_share_same_addr =
  test "host_map_guest addr clash" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let buf1 = Region.alloc 0x1000 in
  let buf2 = Region.alloc 0x1000 in
  host_map_guest vcpu buf1 0x0L;
  pkvm_expect_error (host_map_guest vcpu buf2) 0x0L;
  host_map_guest vcpu buf2 0x1000L;
  vcpu_put ();
  teardown_vm vm;
  free_vcpu vcpu;
  host_reclaim_region buf1;
  Region.free buf1;
  host_reclaim_region buf2;
  Region.free buf2

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
