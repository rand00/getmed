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

open Exceptions
open Result.Monad
open Settings
open File.Infix

(*goto: beginning writing of new rc-format functions*)

let find () = 
  try Ok (
    List.find Sys.file_exists  
      [ (* (Sys.getcwd ()) /: ".getmedrc"; *)
        (Sys.getenv "HOME") /: ".getmedrc" ] )
  with Not_found -> Bad RcNotFound

(*goto: order in modules*)
let get_value = function
  | <:re< ( [" \t"]* "=" [" \t"]* )
      "\""? ( [^"\""]* as value ) >> -> Some value
  | _ -> None

let get_bool = function 
  | <:re< ( [" \t"]* "=" [" \t"]* ) "\""? ( "true" ) >> -> Some true
  | <:re< ( [" \t"]* "=" [" \t"]* ) "\""? ( "false" ) >> -> Some false
  | _ -> None

let parse_and_set settings = function
  | <:re< "device_path" (_* as rest ) >> -> 
    ( match get_value rest with 
    | Some value -> Some { settings with search_subdir = value } | _ -> None )
  | <:re< "mount_path" (_* as rest ) >> -> 
    ( match get_value rest with 
    | Some value -> Some { settings with mount_path = value } | _ -> None )
  | <:re< "image_to_path" (_* as rest ) >> -> 
    ( match get_value rest with 
    | Some value -> Some { settings with img_to_root = value } | _ -> None )
  | <:re< "video_to_path" (_* as rest ) >> ->
    ( match get_value rest with 
    | Some value -> Some { settings with vid_to_root = value } | _ -> None )
  | <:re< "append_title" (_* as rest ) >> ->
    ( match get_value rest with 
    | Some value -> Some { settings with append_title = value } | _ -> None )
  | <:re< "remove_media" (_* as rest ) >> ->
    ( match get_bool rest with 
    | Some bool -> Some { settings with remove_media = bool } | _ -> None )
  | <:re< "unmount" (_* as rest ) >> ->
    ( match get_bool rest with 
    | Some bool -> Some { settings with unmount = bool } | _ -> None )
  | <:re< "debug" (_* as rest ) >> ->
    ( match get_bool rest with 
    | Some bool -> Some { settings with debug = bool } | _ -> None )
  | _ -> None

let update rc_file ~settings = 
  (Result.catch File.lines_of rc_file
   >>= fun lines -> 
   Result.catch 
     (fold (fun settings elem -> 
          ( match parse_and_set settings elem with
            | Some settings' -> settings'
            | None -> settings )
        ) settings) 
     lines)
  |> function
  | Ok settings -> (Ok ()), settings
  | (Bad _) as bad -> bad, settings

let get_template () = String.concat "\n" 
  [ "mount_path = /mnt/your_folder";
    "device_path = device_media_subfolder";
    "image_to_path = /some/directory/of/choice";
    "video_to_path = /some/directory/of/choice";
    "append_title = some_appended_title_possibly_empty";
    "remove_media = false";
    "unmount = true"; ]
