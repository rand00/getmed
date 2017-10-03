(*
  Getmed - a media-file downloader and sorter.
  Copyright (C) 2014 Claes Worm

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Batteries 
open BatExt

module T = struct 
  type cli_arg = [
    | `Append_title of string
    | `Config_path of string
    | `Debug of bool
  ]

  type t = cli_arg
end
open T

let rec split_config args = 
  let rec aux path_opt conf_acc = function
    | [] -> path_opt, conf_acc
    | `Config_path p :: tl -> aux (Some p) conf_acc tl
    | (`Append_title _ as hd)::tl
    | (`Debug _        as hd)::tl -> aux path_opt (hd::conf_acc) tl
  in
  aux None [] args

let handle_all () = 
  let make_indent ?(extra_indent=0) ?(arg_parse_std=5) arg = 
    String.make 
      (arg_parse_std + (String.length arg) + extra_indent) ' '
  in
  let template_rc_arg = "--template-rc"
  and print_rc_options_arg = "--print-rc-options"
  and append_short_arg = "-a"
  and append_long_arg = "--append"
  and config_path_long_arg = "--rc"
  and debug_arg = "--debug"
  in
  let template_rc_indent = make_indent template_rc_arg
  and print_rc_options_indent = make_indent print_rc_options_arg
  and append_short_indent = make_indent ~extra_indent:7 append_short_arg
  and append_long_indent = make_indent ~extra_indent:7 append_long_arg
  and config_path_long_indent = make_indent ~extra_indent:7 config_path_long_arg
  and debug_indent = make_indent ~extra_indent:7 debug_arg
  in
  let append_title_doc = 
    "<title> : Appends a given title to the media-directories names"
  and config_path_doc =
    "<file-path> : Sets the configuration-file to use."
  in
  let args_acc = ref [] in

  Arg.parse 
    [
      ( config_path_long_arg, 
        Arg.String (fun s -> args_acc := ((`Config_path s) :: !args_acc)),
        Msg.termwrap 
          ~initial_indent:config_path_long_indent
          ~subsequent_indent:config_path_long_indent
          ~initial_nonwrap:(String.length config_path_long_indent)
          [ config_path_doc ] );

      ( append_short_arg, 
        Arg.String (fun s -> args_acc := ((`Append_title s) :: !args_acc)),
        Msg.termwrap 
          ~initial_indent:append_short_indent
          ~subsequent_indent:append_short_indent
          ~initial_nonwrap:(String.length append_short_indent)
          [ append_title_doc ] );
      ( append_long_arg, 
        Arg.String (fun s -> args_acc := ((`Append_title s) :: !args_acc)),
        Msg.termwrap 
          ~initial_indent:append_long_indent
          ~subsequent_indent:append_long_indent
          ~initial_nonwrap:(String.length append_long_indent)
          [ append_title_doc ] );

      ( template_rc_arg, 
        Arg.Unit (fun () -> 
            Rc2.get_template () |> print_endline; 
            exit 0),
        Msg.termwrap 
          ~initial_indent:template_rc_indent
          ~subsequent_indent:template_rc_indent
          ~initial_nonwrap:(String.length template_rc_indent)
          [ ": Prints a template rc-format to std-out; ";
            "use this for generating your first rc by piping ";
            "output to '.getmedrc' in your ";
            "home folder or current working directory."; 
          ] ); 
      ( print_rc_options_arg,
          Arg.Unit (fun () -> 
            Rc2.get_rc_options () |> print_endline; 
              exit 0),
        Msg.termwrap 
          ~initial_indent:print_rc_options_indent
          ~subsequent_indent:print_rc_options_indent
          ~initial_nonwrap:(String.length print_rc_options_indent)
          [ ": Prints the possible options to set on various "
          ; "fields in the rc-format."; 
          ] ); 

      ( debug_arg, 
        Arg.Unit (fun () -> args_acc := ((`Debug true) :: !args_acc)),
        Msg.termwrap 
          ~initial_indent:debug_indent
          ~subsequent_indent:debug_indent
          ~initial_nonwrap:(String.length debug_indent)
          [ ": Turns on debug-printing." ] );
    ]

    (fun _ -> ()) (* anonymoys args*)

    ((Msg.termwrap 
        ~initial_indent:""
        ~subsequent_indent:""
        ~initial_nonwrap:1
        [ "\nGetmed is a CLI program for semi-automatically ";
          "transferring media from a connected camera device. ";
          "Make a '.getmedrc' file in your home-folder or ";
          "current working directory to specify your cameras ";
          "settings."; ]) 
     ^ "\n\n*** Arguments ***");

  !args_acc (* the accumulated arguments returned *)

open Rc2

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


(**For updating settings from cmd-line args*)
