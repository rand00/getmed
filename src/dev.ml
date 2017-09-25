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
open Result.Monad
open Exceptions

let is_eos = function
  | <:re< _* "EOS_DEVEL" >> -> true
  | <:re< _* "EOS_DIGITAL" >> -> true
  | _ -> false

let pmatch ~pattern s = Pcre.pmatch ~pat:pattern s

(*goto factor out is-device checking all user-patterns*)
let is_device device_match blkid_line =
  List.exists (fun pattern ->
      pmatch
        ~pattern:(
          match pattern with
          | `Uuid p -> " UUID=\"" ^ p ^ "\""
          | `Label p -> " LABEL=\"" ^ p ^ "\""
        )
        blkid_line
    ) device_match

(*goto make able to find any dev based on settings*)
(*goto trap exceptions in monad (and in all other functions)*)
let find ~settings () = 
  let open Rc2 in
  let blkid = Unix.command_getlines "blkid" in
  try 
    (Ok (Enum.find (is_device settings.device_match) blkid 
         |> (function 
             | <:re< ("/dev/" _* as dev) ":" >> -> dev 
             | _ -> "")))
  , settings
  with Not_found ->
    ( Msg.term `Error "find device"
        [ "The device '"; settings.name; "' is not connected, ";
          "or you have not run getmed with enough rights." ];
      (Bad DeviceNotPresent), settings
    )


let get_dir_if_mounted dev dir =
  Ok (Unix.command_getlines "mount")
  >>= (fun mount_lines -> 
      try 
        Enum.find (Pcre.pmatch ~pat:("^" ^ dev)) mount_lines
        |> function 
        | <:re< [^" "]* " on " ( [^" "]{1+} as dir_existing ) >> -> 
          ( Msg.term `Notif "mount"
              [ "Device '"; dev; "' already mounted at '";
                dir_existing; "'. Will use existing ";
                "mountpoint." ];
            Ok (`Dont_mount dir_existing) )
        | _ -> Ok (`Mount dir) (*will not happen*)

      with Not_found -> 
        Ok (`Mount dir)
    )

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
            [ "The supplied mount-point '";
              dir;
              "'is not empty. "];
          Bad MountFolderNotEmpty )
    else if Sys.file_exists dir && not (Sys.is_directory dir) then
      ( Msg.term `Error "fix mountpoint"
          [ "The given mount-point '";
            dir;
            "' is not a directory."];
        Bad MountFolderIsNotADirectory )
    else (*if dir does not exist*) 
      ( Msg.term `Notif "fix mountpoint"
          [ "Mount-point '";
            dir;
            "' does not exist - creating it now." ];
        try
          Unix.mkdir dir 0o755;
          Ok dir_and_action
        with exn ->
          Bad exn
      )


let mount dev = function
  | `Dont_mount dir -> Ok dir
  | `Mount dir ->   
    (Sys.command 
       (String.concat " " 
          [ "mount"; dev; (dir |> Folder.escape) ]) 
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


let mount_smartly ~settings dev =
  let open Rc2 in
  begin
    mountpoint_fix_or_find dev settings.mount_path
    >>= mount dev
  end
  |> function
  | Ok mount_path -> ((Ok ()), { settings with mount_path })
  | (Bad _) as bad -> (bad, settings)

open Rc2.T

let unmount ~settings () = 
  let s = settings in
  match s.unmount with
  | false -> 
    ( Msg.term `Notif "unmount" [
          "Not going to unmount device '";
          settings.name; "'.";
        ];
      Ok () )
  | true  -> 
    (Sys.command ("umount " ^ (s.mount_path |> Folder.escape))
     |> function 
     | 0 -> 
       ( Msg.term `Notif "unmount" [
             "Unmount succesful for device '";
             settings.name; "'.";
           ]; 
         Ok () )
     | errcode -> 
       ( Msg.term `Error "unmount" [
             "Error occured during unmounting device '";
             settings.name; "'.";
             "Error-code was '";
             String.of_int errcode; "'."
           ];
         Bad UnMountFailure ))

