let schema =
  let open Json.Q in
  obj (fun ll ts -> ll, ts)
  |> mem_opt "loglevel"  string
  |> mem_opt "run" (mems bool)

let buf_add_fd buf fd =
  let bs = Bytes.create 4096 in
  let rec go () = match Unix.read fd bs 0 4096 with
  | 0 -> ()
  | n -> Buffer.add_subbytes buf bs 0 n; go () in
  go ()

let (let*) = Result.bind

let to_unix_result f x = match f x with
| exception Unix.Unix_error (err, _, _) -> Error (`Msg (Unix.error_message err))
| v -> Ok v

module Smap = Map.Make(String)

let load file =
  let buf = Buffer.create 17 in
  let* fd = to_unix_result Unix.(openfile file [O_RDONLY]) 0 in
  buf_add_fd buf fd; Unix.close fd;
  let* json = Json.of_string (Buffer.contents buf) in
  let* level, run = Json.Q.query schema json in
  let* level = Option.fold level ~none:(Ok None) ~some:Logs.level_of_string in
  let run = match run with
  | Some xs -> Smap.of_seq (List.to_seq xs)
  | _ -> Smap.empty in
  let runf test = Smap.find_opt test run |> Option.value ~default:true in
  Ok (level, runf)
