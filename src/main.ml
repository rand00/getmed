open Rc2.T

module T = struct 
  type cli_arg = [
    | `Append_title of string
    | `Config_path of string
    | `Debug of bool
  ]

  type t = cli_arg
end
open T


let update_rc args ~settings () = 
  List.fold_left (fun settings_acc -> function 
      | `Append_title s ->
        let devices =
          List.map (fun d -> { d with folders_append = s })
            settings_acc.devices
        in
        { settings_acc with devices } 
      | `Debug b ->
        { settings_acc with debug = b }
    ) settings args
  |> fun settings -> (Ok ()), settings

let update_append_title title ~settings =
  let devices = List.map (fun d ->
      { d with folders_append = title }
    ) settings.devices
  in { settings with devices }  

let arg_handler config_path =
  let open Batteries in
  let open StateResult.Infix in
  let settings = Rc2.std in
  let path = match config_path with
    | Some path -> Ok path
    | None -> Rc2.find () in
  let settings_r =
    (path, settings) >>= Rc2.read_from_file 
  in
  fun append_title debug -> 
    settings_r >>= fun ~settings () -> 
    let settings = match append_title with
      | Some t -> update_append_title t ~settings 
      | None -> settings in
    let settings = { settings with debug }
    in
    Getmed.getmed' ~settings

open Cmdliner

let config_path = 
  let doc = "" in
  Arg.(value & opt (some file) None &
       info ["rc"] ~docv:"RC" ~doc)
  
let append_title =
  let doc = "" in
  Arg.(value & opt (some string) None &
       info ["a"; "append"] ~docv:"APPEND_TITLE" ~doc)

let debug = 
  let doc = "" in
  Arg.(value & flag & info ["debug"] ~docv:"DEBUG" ~doc)
  
let cmd =
  let doc =
    "Getmed is a CLI program for semi-automatically \
     transferring media from a connected camera device. \
     Make a '.getmedrc' file in your home-folder to \
     specify your cameras settings." in
  let man = [ ] in
  Term.(const arg_handler $ config_path $ append_title $ debug),
  Term.(info "getmed" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man)

let () = Term.(exit @@ eval cmd)

