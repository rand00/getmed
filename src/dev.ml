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
open Rc2.T

type device = {
  name : string;
  fstype : string;
  label : string;
  uuid : string;
  mountpoint : string;
} [@@ deriving show]


module Parse = struct

  open Rresult
  open Astring
  open String

  type device_parse_error = [
    | `Equals
    | `Ident
    | `Value
    | `White
  ] [@@ deriving show]

  let print_device = function
    | Ok s -> print_endline @@ show_device s
    | Error e -> 
      print_endline ("Error: " ^ show_device_parse_error e)

  let print_and_run place f s =
    print_endline (place ^ String.Sub.to_string s);
    f s

  let drop_white s = Ok (Sub.drop ~sat:Char.Ascii.is_white s)
      
  let parse_equals s = match Sub.head s with
    | Some '=' -> Ok (Sub.tail s)
    | Some _ | None -> Error `Equals 

  let parse_value s = match String.Sub.head s with
    | Some '"' -> 
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' ->
          let acc = List.rev (data :: acc) in
          Ok (String.Sub.(to_string @@ concat acc), (String.Sub.tail rem))
        | Some '\\' ->
          let rem = String.Sub.tail rem in
          begin match String.Sub.head rem with
            | Some ('"' | '\\' as c) ->
              let acc = String.(sub (of_char c)) :: data :: acc in
              loop acc (String.Sub.tail rem)
            | Some _ | None -> Error `Value
          end
        | None | Some _ -> Error `Value
      in
      loop [] (String.Sub.tail s)
    | Some _ ->
      let is_data c = not (Char.Ascii.is_white c) in
      let data, rem = String.Sub.span ~sat:is_data s in
      Ok (Sub.to_string data, rem)
    | None -> Ok ("", s)

  let parse_kv ident s =
    match
      drop_white s
      >>| Sub.is_prefix ~affix:(Sub.v ident) 
    with
    | Ok true ->
      Sub.drop ~sat:(fun c ->
          Char.Ascii.(is_letter c || is_white c)
        ) s
      (*< goto this should be one step instead of two *)
      |> drop_white
      >>= parse_equals (*print_and_run "equals got:"*) 
      >>= drop_white
      >>= parse_value >>| fun (value, s) -> 
      value, s
    | Ok false -> Error `Ident
    | Error _ as e -> e

  let parse_device lsblk =
    let s = Sub.v lsblk in
    parse_kv "NAME" s >>= fun (name_value, s) ->
    parse_kv "FSTYPE" s >>= fun (fstype_value, s) -> 
    parse_kv "LABEL" s >>= fun (label_value, s) ->
    parse_kv "UUID" s >>= fun (uuid_value, s) ->
    parse_kv "MOUNTPOINT" s >>| fun (mountpoint_value, s) -> 
    { name = name_value;
      fstype = fstype_value;
      label = label_value;
      uuid = uuid_value;
      mountpoint = mountpoint_value;
    }

  let device = parse_device

end

let pmatch ~pattern s = Pcre.pmatch ~pat:pattern s

(*goto? factor out is-device checking all user-patterns*)
let is_device device_match dev =
  List.exists (function 
      | `Uuid pattern -> pmatch ~pattern dev.uuid
      | `Label pattern -> pmatch ~pattern dev.label
    ) device_match

let find_aux parse_dev is_dev = 
  Unix.command_getlines
    "lsblk -Pp -o name,fstype,label,uuid,mountpoint" 
  |> BatEnum.map parse_dev 
  |> BatList.of_enum 
  |> CCList.find_map is_dev 

let find ~settings () =
  let s = settings in
  let open Rresult in
  let open Rc2 in
  find_aux Parse.device (function
      | Ok dev when is_device s.device.device_match dev -> Some dev
      | Ok _ -> None
      | Error _ ->
        Msg.term ~colors:s.colors `Notif "parse device"
          [ "Failed to parse some device." ];
        None 
    )
  |> function
  | Some dev -> BatResult.Ok dev, s
  | None ->
    ( Msg.term ~colors:s.colors `Notif "find device"
        [ "The device '"; s.device.name; "' is not connected, ";
          "or you have not run getmed with enough rights." ];
      (BatResult.Bad DeviceNotPresent), s
    )

let get_dir_if_mounted dev dir ~colors =
  match dev.mountpoint with
  | dir_existing when
      Sys.file_exists dir_existing
      && Sys.is_directory dir_existing ->
    begin
      Msg.term ~colors `Notif "mount"
        [ "Device '"; dev.name; "' already mounted at '";
          dir_existing; "'. Will use existing ";
          "mountpoint." ];
      Ok (`Dont_mount dir_existing)
    end
  | _ -> Ok (`Mount dir) 

let mountpoint_fix_or_find (dev:device) dir ~colors =
  let msg = Msg.term ~colors in
  get_dir_if_mounted dev dir ~colors 
  >>= function
  | (`Dont_mount dir_existing) as dir_and_action -> 
    Ok dir_and_action
  | (`Mount dir) as dir_and_action -> 
    if Sys.file_exists dir && Sys.is_directory dir then 
      match (Array.length (Sys.readdir dir)) with
      | 0 -> Ok dir_and_action
      | _ -> 
        ( msg `Error "fix mountpoint"
            [ "The supplied mount-point '";
              dir;
              "'is not empty. "];
          Bad MountFolderNotEmpty )
    else if Sys.file_exists dir && not (Sys.is_directory dir) then
      ( msg `Error "fix mountpoint"
          [ "The given mount-point '";
            dir;
            "' is not a directory."];
        Bad MountFolderIsNotADirectory )
    else (*if dir does not exist*) 
      ( msg `Notif "fix mountpoint"
          [ "Mount-point '";
            dir;
            "' does not exist - creating it now." ];
        try
          Unix.mkdir dir 0o755;
          Ok dir_and_action
        with exn ->
          Bad exn
      )

let mount dev ~colors action =
  let msg = Msg.term ~colors in
  match action with 
  | `Dont_mount dir -> Ok dir
  | `Mount dir ->   
    (Sys.command 
       (String.concat " " 
          [ "mount"; dev.name; (dir |> Folder.escape) ]) 
     |> function 
     | 0 -> 
       ( msg `Notif "mount" [
             "Mount of device '"; dev.name;
             "' succesful."
           ]; 
         Ok dir )
     | errcode -> 
       ( msg `Error "mount" 
           [ "Error occured during mounting. "; 
             "Error-code was '"; String.of_int errcode;
             "' - see 'man mount' for more info." ];
         Bad MountError ))

let mount_smartly ~settings (dev:device) =
  let s = settings in
  mountpoint_fix_or_find dev s.device.mount_path ~colors:s.colors
  >>= mount dev ~colors:s.colors
  |> function
  | Ok mount_path ->
    ((Ok ()), ({ s with device = { s.device with mount_path }}))
  | (Bad _) as bad ->
    (bad, s)

let unmount ~settings () = 
  let s, colors = settings, settings.colors in
  let msg = Msg.term ~colors in
  match s.device.unmount with
  | false -> 
    ( msg `Notif "unmount" [
          "Not going to unmount device '";
          s.device.name; "'.";
        ];
      Ok () )
  | true  -> 
    (Sys.command ("umount " ^ (s.device.mount_path |> Folder.escape))
     |> function 
     | 0 -> 
       ( msg `Notif "unmount" [
             "Unmount succesful for device '";
             s.device.name; "'.";
           ]; 
         Ok () )
     | errcode -> 
       ( msg `Error "unmount" [
             "Error occured during unmounting device '";
             s.device.name; "'.";
             "Error-code was '";
             String.of_int errcode; "'."
           ];
         Bad UnMountFailure ))

