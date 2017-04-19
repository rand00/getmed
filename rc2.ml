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

(*
open Batteries 
open Batext

open Exceptions
open Result.Monad
open Settings
open File.Infix
*)

open Result

type device_config = {
  name : string;
  active : bool;

  device_match : [ `Uuid of string | `Label of string ];

  mount_path : string;
  search_subdirs : [ `Recurse of string | `Only of string ] list;
  
  image_exts : string list;
  image_meta_exts : string list;
  video_exts : string list;
  video_meta_exts : string list;

  (*have separate for vid/img?*)
  files_append : [ `String of string | `Nothing ];
  files_prepend : [ `Date | `String of string | `Nothing ];

  image_destinations : string list;
  video_destinations : string list;

  (* images_groupby : "DATE_CREATION" *)

  cleanup : [ `Remove_originals | `Format | `None ];
  unmount : bool;

} [@@ deriving yojson]

type config = device_config list
[@@ deriving yojson]

(*
let find () = 
  try Ok (
    List.find Sys.file_exists  
      [ (Sys.getcwd ()) /: ".getmedrc";
        (Sys.getenv "HOME") /: ".getmedrc" ] )
  with Not_found -> Bad RcNotFound

type file_path = string
  
(*goto continue writing interface*)
let update : file_path -> settings:Settings.t -> (unit, 'a) result
  = fun file ~settings ->
    failwith "todo"

let get_template : unit -> string
  = fun () ->
    failwith "todo"
*)

let _ = match Sys.argv with
  | [| _; "print" |] -> 
    [
      {
        name = "canon50d";
        active = true;
        device_match = `Label "EOS_*";
        mount_path = "/mnt/camera";
        search_subdirs = [ `Recurse "/DCIM" ];

        image_exts = [ "jpg"; "cr2"; ];
        image_meta_exts = ["xmp"];
        video_exts = [ "mov"; "avi" ];
        video_meta_exts = [];

        files_append = `String "lala";
        files_prepend = `Nothing;

        image_destinations = [ "/home/foo/images" ];
        video_destinations = [ "/home/foo/videos" ];
        
        cleanup = `Remove_originals;
        unmount = true;
      }
    ]
    |> config_to_yojson
    |> Yojson.Safe.to_string
    |> print_endline

  | [| _; "read" |] ->
    let buf = Bytes.create 10_000 in
    let len_inp = Unix.read Unix.stdin buf 0 10_000 in
    Bytes.sub_string buf 0 len_inp
    |> Yojson.Safe.from_string
    |> config_of_yojson
    |> function
    | Ok _ -> print_endline "Sucesfully parsed input"
    | Error e -> print_endline ("Error: "^e)
    
    
    
