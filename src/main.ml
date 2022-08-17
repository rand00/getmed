open Rc2.T

let update_rc_append_title title ~settings =
  let devices = List.map (fun d ->
      { d with folders_append = title }
    ) settings.devices
  in { settings with devices }  

let update_rc_debug debug (settings:config) = { settings with debug }

let update_rc_safe_run safe_run (settings:config) =
  if not safe_run then settings else {
    settings with
    devices = List.map (fun d ->
        { d with 
          cleanup = `None;
          unmount = false;
        }
      ) settings.devices 
  }

let print_success settings = 
  let msg = match List.exists (fun d -> d.active) settings.devices with
    | false -> "No active devices."
    | true  -> "Ran succesfully for active devices."
  in
  Msg.term ~colors:settings.colors `Major "main" [ msg ]

let print_error exn_name s =
  Msg.term `Error "main" ~colors:Rc2.Default.colors
    [ exn_name;":\n\n"; s ];
  exit 1
    
let print_error_exn exn =
  Msg.term `Error "main" ~colors:Rc2.Default.colors
    [ Printexc.to_string exn ];
  exit 1

let handle_errors_last (r, (settings : Rc2.config)) = match r with
  | BatResult.Ok (Some dirs_transferred_to) ->
    print_success settings;
    dirs_transferred_to |> CCList.iter @@ fun dir -> 
    Msg.term ~colors:settings.colors `Notif "getmed" [
      "Partial summary: Created dir: '"; dir; "'."
    ]
  | BatResult.Ok None -> print_success settings
  | BatResult.Error (Exceptions.RcParseError s) ->
    print_error "Exceptions.RcParseError" s
  | BatResult.Error e -> print_error_exn e 
  
let arg_handler print_template show_rc_options = 
  if print_template then (
    Rc2.get_template () |> print_endline;
    exit 0
  );
  if show_rc_options then (
    Rc2.get_rc_options () |> print_endline;
    exit 0
  );
  fun config_path append_title safe_run debug -> 
    let open Batteries in
    let open StateResult.Infix in
    let settings = Rc2.std in
    let path = match config_path with
      | Some path -> Ok path
      | None -> Rc2.find () in
    begin
      (path, settings)
      >>= Rc2_handlers.read_from_file 
      >>= fun ~settings () ->
      append_title
      |> Option.map (update_rc_append_title ~settings) 
      |> Option.default settings
      |> update_rc_debug debug
      |> update_rc_safe_run safe_run
      |> Getmed.getmed
    end
    |> handle_errors_last

open Cmdliner

let config_path = 
  let doc = "Sets the configuration-file to use. Mostly \
             intended for testing rc-files as you can \
             disable your devices within the rc-file." in
  let docv = "RC_PATH" in
  Arg.(value & opt (some file) None & info ["rc"] ~docv ~doc)
  
let append_title =
  let doc = "Appends a given title to the media-directories names." in
  let docv = "APPEND_TITLE" in
  Arg.(value & opt (some string) None & info ["a"; "append"] ~docv ~doc)

let debug = 
  let doc = "Turns on debug-printing." in
  Arg.(value & flag & info ["debug"] ~docv:"DEBUG" ~doc)

let safe_run = 
  let doc = "Disable cleanup and unmount for all devices." in
  Arg.(value & flag & info ["safe-run"] ~docv:"SAFE_RUN" ~doc)

let print_template_rc =
  let doc = "Prints a template for defining your own \
             rc-file. Direct output at '~/.$(tname)rc'." in
  let docv = "PRINT_TEMPLATE_RC" in
  Arg.(value & flag & info ["print-template-rc"] ~docv ~doc)

let show_rc_options =
  let doc = "Prints the available options for the fields \
             in the rc-format." in
  let docv = "SHOW_RC_OPTIONS" in
  Arg.(value & flag & info ["show-rc-options"] ~docv ~doc)

let cmd =
  let version = "2.0.0" in
  let doc = "convenient and safe media-transfer and sorting." in
  let man = [
    `S Manpage.s_description;
    `P "Fullautomatically \
        transfer media from a connected camera device, following \
        your specification in '~/.getmedrc'.";
    `P "$(tname) will mount, transfer, name, cleanup \
        and unmount, depending on your rc- and cli-specification \
        of how and what to do. See options $(i, \
        --print-template-rc) and $(i, --show-rc-options), for \
        constructing your '~/.getmedrc'.";
    `P "If you use a system where mounting of devices is not \
        done automatically, $(tname) can do this for you. If \
        needed, $(tname) will call $(b,sudo) for you.";
    `S Manpage.s_see_also;
    `P "$(b,mount), $(b,unmount), $(b,fstab), $(b,sudo)"
  ] in
  let term = Term.(
    const arg_handler
    $ print_template_rc
    $ show_rc_options
    $ config_path
    $ append_title
    $ safe_run
    $ debug
  )
  in
  Cmd.(v (info "getmed" ~version ~doc ~man) term)

let () =
  Cmd.eval cmd
  |> exit

