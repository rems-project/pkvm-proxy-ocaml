open Pkvm_proxy

module Log = (val Logs.(Src.create "Random" |> src_log))

type rgn = Rgn : 'a region -> rgn
let cast (Rgn r) : _ region = Obj.magic r

let rec remove i = function
| [] -> invalid_arg "remove: index too big"
| x::xs when i = 0 -> x, xs
| x::xs -> let r, xs = remove (i - 1) xs in r, (x::xs)

let pick xs = List.nth xs (List.length xs |> Random.int)
let pluck xs = remove (List.length xs |> Random.int) xs
let region xs = pick xs |> cast

let max_vms = 10
and max_vcpus = 100

let pp_len ppf = Fmt.(using List.length int |> styled `Bold) ppf

let rec step i vms vcpus rgns =
  Log.warn (fun k ->
    k "+ step %d vms: %a, vcpus: %a, regions: %a@."
      i pp_len vms pp_len vcpus pp_len rgns);
  let has_vm = List.is_empty vms |> not
  and has_vcpu = List.is_empty vcpus |> not
  in
  let vms, vcpus, rgns =
    try
      match Random.int 14 with
      | 0  -> host_share_hyp (region rgns); vms, vcpus, rgns
      | 1  -> host_unshare_hyp (region rgns); vms, vcpus, rgns
      | 2  -> host_reclaim_region (region rgns); vms, vcpus, rgns
      | 3  ->
          (if has_vcpu then
            host_map_guest ~topup_memcache:(Random.bool())
              (pick vcpus) (region rgns) (Random.int64 0xfffffL));
          vms, vcpus, rgns
      | 4  ->
          (if has_vcpu then vcpu_adjust_pc (pick vcpus));
          vms, vcpus, rgns
      | 5  ->
          (if has_vcpu then ignore (vcpu_run (pick vcpus)));
          vms, vcpus, rgns
      | 6  ->
          (* timer_set_cntvoff *)
          vms, vcpus, rgns
      | 7  ->
          ( if List.length vms < max_vms then
              init_vm ~vcpus:(Random.int 5) ~protected:true () :: vms
            else vms ), vcpus, rgns
      | 8 ->
          let vcpus =
            if has_vm && List.length vcpus < max_vcpus then
              init_vcpu (pick vms) (Random.int 5) :: vcpus
            else vcpus in
          vms, vcpus, rgns
      | 9 ->
          ( if has_vm then
              let vm, vms = pluck vms in
              teardown_vm vm;
              vms
            else vms ), vcpus, rgns
      | 10 ->
          (if has_vcpu then vcpu_load (pick vcpus));
          vms, vcpus, rgns
      | 11 -> vcpu_put(); vms, vcpus, rgns
      | 12 -> vcpu_sync_state(); vms, vcpus, rgns
      | 13 ->
          vms,
          ( if has_vcpu then
              let vcpu, vcpus = pluck vcpus in
              free_vcpu vcpu;
              vcpus
            else vcpus ),
          rgns
      | _ -> assert false
    with HVC _ -> vms, vcpus, rgns
  in
  step (i + 1) vms vcpus rgns

let random1 () = step 0 [] [] (List.init 50 (fun _ -> Rgn (Region.alloc 0x1000)))

let _ =
  Random.init 314159;
  Logs.(set_level ~all:true (Some Debug));
  Logs.set_reporter (Logs_fmt.reporter ());
  random1 ()
