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

let update_rc_append_title title ~settings =
  let devices = List.map (fun d ->
      { d with folders_append = title }
    ) settings.devices
  in { settings with devices }  

let update_rc_debug debug settings = { settings with debug }

let arg_handler config_path append_title debug =
  let open Batteries in
  let open StateResult.Infix in
  let settings = Rc2.std in
  let path = match config_path with
    | Some path -> Ok path
    | None -> Rc2.find () in
  (path, settings)
  >>= Rc2.read_from_file 
  >>= fun ~settings () ->
  append_title
  |> Option.map (update_rc_append_title ~settings) 
  |> Option.default settings
  |> update_rc_debug debug
  |> Getmed.getmed

open Cmdliner

let config_path = 
  let doc = "" in
  Arg.(value & opt (some file) None & info ["rc"] ~docv:"RC" ~doc)
  
let append_title =
  let doc = "" in
  Arg.(value & opt (some string) None & info ["a"; "append"]
         ~docv:"APPEND_TITLE" ~doc)

let debug = 
  let doc = "" in
  Arg.(value & flag & info ["debug"] ~docv:"DEBUG" ~doc)
  
let cmd =
  let doc = "convenient and safe media-transfer and sorting." in
  let man = [ ] in
  Term.(const arg_handler $ config_path $ append_title $ debug),
  Term.(info "getmed" ~version:"%%VERSION%%" ~doc ~man)

let () = Term.(exit @@ eval cmd)

(*
"is a CLI program for semi-automatically \
     transferring media from a connected camera device. \
     Make a '.getmedrc' file in your home-folder to \
     specify your cameras settings."
*)
