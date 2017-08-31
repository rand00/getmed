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
    | `Debug of bool
  ]

  type t = cli_arg
end
open T
    
let handle_all () = 
  let make_indent ?(extra_indent=0) ?(arg_parse_std=5) arg = 
    String.make 
      (arg_parse_std + (String.length arg) + extra_indent) ' '
  in
  let template_rc_arg = "--template-rc"
  and append_short_arg = "-a"
  and append_long_arg = "--append"
  and debug_arg = "--debug"
  in
  let std_rc_indent = make_indent template_rc_arg
  and append_short_indent = make_indent ~extra_indent:7 append_short_arg
  and append_long_indent = make_indent ~extra_indent:7 append_long_arg
  and debug_indent = make_indent ~extra_indent:7 debug_arg
  in
  let append_title_doc = 
    "<title> : Appends a given title to the media-directories names" 
  in
  let args_acc = ref [] in
  
  Arg.parse 
    [ ( append_short_arg, 
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
          Rc2.get_template () 
          |> print_endline; 
          exit 0),
        Msg.termwrap 
          ~initial_indent:std_rc_indent
          ~subsequent_indent:std_rc_indent
          ~initial_nonwrap:(String.length std_rc_indent)
          [ ": Prints a template rc-format to std-out; ";
            "use this for generating your first rc by piping ";
            "output to a '.getmedrc'-file in your ";
            "home folder or current working directory."; 
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
        [ "\nGetmed is a cmd-line program for automatically ";
          "transferring media from a connected camera device. ";
          "Make a '.getmedrc' file in your home-folder or ";
          "current working directory to specify your personal ";
          "settings."; ]) 
     ^ "\n\n*** Arguments ***");
  
  !args_acc (* the accumulated arguments returned *)

open Rc2

(*goto why did I use fold_left_result.. *)
let update_rc args ~settings () = 
  List.fold_left_result (fun settings_acc -> function 
      | `Append_title s ->
        let devices =
          List.map (fun d -> { d with folders_append = s })
            settings_acc.devices
        in
        Ok { settings_acc with devices } 
      | `Debug b ->
        Ok { settings_acc with debug = b }
    ) settings args
  |> function
  | Ok settings -> (Ok ()), settings
  | (Bad _) as bad -> bad, settings


(**For updating settings from cmd-line args*)
