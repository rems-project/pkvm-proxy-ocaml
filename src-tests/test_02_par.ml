open Pkvm_proxy
open Pkvm_testlib

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
  Array.iter free_vcpu vcpus

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
  Array.iter (function (Some vcpu) -> free_vcpu vcpu | _ -> assert false) vcpus

let t_vcpu_load_conflict =
  test "vcpu load on multiple threads" @@ fun _ ->
  let vm = init_vm () in
  let vcpu = init_vcpu vm 0 in
  vcpu_load vcpu;
  spawn ~cpu:1 (fun _ -> vcpu_load vcpu; pkvm_expect_error vcpu_run vcpu) |> join;
  vcpu_put();
  teardown_vm vm;
  free_vcpu vcpu

let _ = main ~want_cpus:4 [
  t_vcpu_load_put_par_1
; t_vcpu_load_put_par_2
; t_vcpu_load_conflict
]
