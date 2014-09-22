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
open Core_rand00

open Settings
open Exceptions

open File.Infix

let handle_result = function 
  | ((Ok _), _) -> 
    ( Msg.term `Notif "main" [ "Getmed ran succesfully." ];
      exit 0 )
  | ((Bad (BeforeMounting _ )), _ ) -> exit 1
  | ((Bad exn_after_mounting), settings) ->
    ((match settings.unmount with 
    | true  -> ( Dev.unmount ~settings |> ignore )
    | false -> () );
     raise exn_after_mounting )


(**Run `getmed with super-user rights (for blkid and mounting)*)

let getmed ~settings ~cmdline_args = 

  let open Settings.SetResMonad in
  ( eval_put settings 

    >>& Rc.find >>+ Rc.update
    >>+ Args.update ~cmdline_args 
    >>? Settings.print_if_debug

    >>& Dev.find_eos >>+ Dev.fix_and_mount 
    >>@>> Exceptions.wrap 
      ~wrap:(fun e -> BeforeMounting e)
      
    >>+ Media.search
    >>? Settings.print_if_debug
    >>+ fun media ~settings -> 
      put settings

      >>! Media.dirs_fix
      >>! Media.transfer media
      >>! Media.remove media 
      >>! Dev.unmount )

  |> handle_result


let init () = 

  let std_settings_thunk () = {
    types_to_transfer = `All;
    mount_path = "/mnt/getmed_device0";
    search_subdir = "DCIM"; 
    img_to_root = (Sys.getenv "HOME") /: "Videos" /: "getmed"; 
    vid_to_root = (Sys.getenv "HOME") /: "Pictures" /: "getmed";
    title = Folder.Name.today();
    append_title = ""; 
    remove_media = false;
    unmount = true;
    debug = false;
  } 
  and cmdline_args = Args.handle_all () (*possibly exits*)
  in 

  getmed 
    ~settings:std_settings_thunk
    ~cmdline_args


let _ = init ()
