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
open Core_rand
open Result.Monad
open Exceptions

let is_eos = function
  | <:re< _* "EOS_DEVEL" >> -> true
  | <:re< _* "EOS_DIGITAL" >> -> true
  | _ -> false

let find_eos () = 
  (Sys.shell_outlines "blkid"
   >>= (fun blkid -> 
     try 
       Ok (Enum.find is_eos blkid 
              |> (function 
                  | <:re< ("/dev/" _* as dev) ":" >> -> dev 
                  | _ -> ""))
     with Not_found ->
       ( Msg.term `Error "find_eos"
           [ "There is no relevant device present, ";  
             "or you have not run getmed with the proper ";
             "rights." ];
         Bad DeviceNotPresent )))


let get_dir_if_mounted dev dir =
  Sys.shell_outlines "mount" 
  >>= (fun mount_lines -> 
    try 
      Enum.find (Pcre.pmatch ~pat:("^" ^ dev)) mount_lines
      |> function 
          | <:re< [^" "]* " on " ( [^" "]{1+} as dir_existing ) >> -> 
            ( Msg.term `Notif "mount"
                [ "Device '"; dev; "' already mounted at '";
                  dir_existing; "' - going to use existing ";
                  "mountpoint." ];
              Ok (`Dont_mount dir_existing) )
          | _ -> Ok (`Mount dir) (*will not happen*)

    with Not_found -> 
      ( Msg.term `Notif "mount"
          [ "Device not already mounted; ";
            "going to mount at '"; dir; "'." ];
        Ok (`Mount dir) ))


let mountpoint_fix_or_find dev dir = 
  get_dir_if_mounted dev dir
  >>= function
    | (`Dont_mount dir_existing) as dir_and_action -> 
      Ok dir_and_action
    | (`Mount dir) as dir_and_action -> 
      if Sys.file_exists dir && Sys.is_directory dir then 
        match (Array.length (Sys.readdir dir)) with
        | 0 -> Ok dir_and_action
        | _ -> 
          ( Msg.term `Error "fix mountpoint"
              [ "The given mount-folder is not empty. ";
                "Choosing not to mount and choosing to exit."];
            Bad MountFolderNotEmpty )
      else (*if dir does not exist*) 
        ( Msg.term `Notif "mountp_fix"
            [ "Mount-point \""; dir; "\" "; 
              "does not exist - creating it now." ];
          Unix.mkdir dir 0o755;
          Ok dir_and_action )


let mount dev = function
  | `Dont_mount dir -> Ok dir
  | `Mount dir ->   
    (Sys.command 
       (String.concat " " 
          [ "mount"; dev; (dir |> Folder.escape_spaces) ]) 
    |> function 
        | 0 -> 
          ( Msg.term `Notif "mount" [ "Mount succesful." ]; 
            Ok dir )
        | errcode -> 
          ( Msg.term `Error "mount" 
              [ "Error occured during mounting. "; 
                "Error-code was '"; String.of_int errcode;
                "' - see 'man mount' for more info." ];
            Bad MountError ))


let fix_and_mount dev ~settings = Settings.(
  (mountpoint_fix_or_find dev settings.mount_path >>= mount dev)
  |> function
      | Ok mount_path -> ((Ok ()), { settings with mount_path })
      | (Bad _) as bad -> (bad, settings) )


let unmount ~settings = Settings.(
  let s = settings in
  match s.unmount with
  | false -> 
    ( Msg.term `Notif "unmount" [ "Not going to unmount." ];
      Ok () )
  | true  -> 
    (Sys.command ("umount " ^ (s.mount_path |> Folder.escape_spaces))
    |> function 
        | 0 -> 
          ( Msg.term `Notif "unmount" [ "Unmount succesful." ]; 
            Ok () )
        | errcode -> 
          ( Msg.term `Error "unmount" 
              [ "Error occured during unmounting. "; 
                "Error-code was '"; String.of_int errcode;
                "'." ];
            Bad UnMountFailure )))
      
