open Pkvm_proxy
open Pkvm_testlib

let t_share_hyp_twice = test "host_share_hyp twice" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  kernel_region_share_hyp reg;
  pkvm_expect_error kernel_region_share_hyp reg;
  kernel_region_unshare_hyp reg;
  kernel_region_free reg

let t_share_guest_twice = test "host_map_guest twice" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let cbuf = kernel_region_alloc 0x1000 in
  kernel_region_release cbuf;
  map_region_guest vcpu.mem cbuf 0x0L;
  pkvm_expect_error (map_region_guest vcpu.mem cbuf) 0x4000L;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim cbuf;
  kernel_region_free cbuf

let t_share_hyp_then_guest =
  test "host_share_hyp then host_map_guest" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  kernel_region_share_hyp reg;
  pkvm_expect_error (map_region_guest vcpu.mem reg) 0x0L;
  kernel_region_unshare_hyp reg;
  map_region_guest vcpu.mem reg 0x0L;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim reg;
  kernel_region_free reg

let t_share_guest_then_hyp =
  test "host_map_guest then host_share_hyp" @@ fun _ ->
  let reg = kernel_region_alloc 0x1000 in
  kernel_region_release reg;
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  map_region_guest vcpu.mem reg 0x0L;
  pkvm_expect_error kernel_region_share_hyp reg;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim reg;
  kernel_region_free reg

let t_guest_share_same_addr =
  test "host_map_guest addr clash" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  let buf1 = kernel_region_alloc 0x1000 in
  let buf2 = kernel_region_alloc 0x1000 in
  kernel_region_release buf1;
  kernel_region_release buf2;
  map_region_guest vcpu.mem buf1 0x0L;
  pkvm_expect_error (map_region_guest vcpu.mem buf2) 0x0L;
  map_region_guest vcpu.mem buf2 0x1000L;
  vcpu_put ();
  teardown_vm vm;
  teardown_vcpu vcpu;
  kernel_region_reclaim buf1;
  kernel_region_free buf1;
  kernel_region_reclaim buf2;
  kernel_region_free buf2

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
