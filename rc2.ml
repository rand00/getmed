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
open Result.Monad
open Settings
*)
open Exceptions

module ResultStd = Result

open Batteries 
open BatExt

open File.Infix
module Result = ResultStd

type device_config = {
  name : string;
  active : bool;

  device_match : [ `Uuid of string | `Label of string ] list;

  mount_path : string;
  search_subdirs : [ `Recurse of string | `Only of string ] list;
  
  image_exts : string list;
  image_meta_exts : string list;

  video_exts : string list;
  video_meta_exts : string list;

  (*have separate for vid/img?*)
  folders_append : [ `String of string | `Nothing ];
  folders_prepend : [ `Date | `String of string | `Nothing ];

  (*goto remember support environment vars in dir names
    < what about env-vars for other fields?
  *)
  image_destinations : string list;
  video_destinations : string list;

  (* images_groupby : "DATE_CREATION" *)

  cleanup : [ `Remove_originals | `Format | `None ];
  unmount : bool;

} [@@ deriving yojson]

type config = device_config list
[@@ deriving yojson]

let example = {
  name = "camera-name";
  active = true;
  device_match = [ `Label "DEVICE_LABEL_WITH_WILDCARD" ];
  mount_path = "/mount/path";
  search_subdirs = [ `Recurse "/folders/within/device/to/search" ];

  image_exts = [ "jpg"; "cr2"; "arw"; ];
  image_meta_exts = ["xmp"];
  video_exts = [ "mov"; "avi"; "mp4" ];
  video_meta_exts = [ "xml" ];

  folders_append = `String "folder_appended_title";
  folders_prepend = `String "folder_prepend_string";

  image_destinations = [ "/image/root/destinations" ];
  video_destinations = [ "/video/root/destinations" ];

  cleanup = `Remove_originals;
  unmount = true;
}

let std = {
  name = "std-camera";
  active = true;
  device_match = [ `Label "EOS_*" ];
  mount_path = "/tmp/getmed-tmp";
  search_subdirs = [ `Recurse "/" ];

  image_exts = [ "jpg"; "cr2"; "arw"; ]; (*goto add more*)
  image_meta_exts = ["xmp"];
  video_exts = [ "mov"; "avi"; "mp4" ];
  video_meta_exts = [ "xml" ];

  folders_append = `Nothing;
  folders_prepend = `Date;

  image_destinations = [ "$HOME/Pictures/getmed/" ];
  video_destinations = [ "$HOME/Videos/getmed/" ];

  cleanup = `None;
  unmount = true;
}


(*goto make examples for each sumtype, that should be printed after the wrapper-example
  as: ... wrapper example ... where 'cleanup = Remove_originals OR ...' where ... 
*)


let find () = 
  try BatResult.Ok (
    List.find Sys.file_exists  
      [ (Sys.getcwd ()) /: ".getmedrc";
        (Sys.getenv "HOME") /: ".getmedrc" ] )
  with Not_found -> BatResult.Bad RcNotFound

type file_path = string
  
(*goto continue writing interface*)
let update : file_path -> settings:config -> (unit, _) result * config
  = fun file ~settings ->
    Yojson.Safe.from_file file
    |> config_of_yojson
    |> function
    | Result.Ok settings' ->
      Msg.term `Notif "update-rc" [
        "Sucesfully parsed config-file."
      ];
      (BatResult.Ok (), settings')
    | Result.Error e ->
      (BatResult.Bad (RcParseError e), settings)

    (* failwith "todo" *)

let get_template : unit -> string
  = fun () ->
    failwith "todo"

let test = false

let _ =
  if test then 
    match Sys.argv with
    | [| _; "print" |] -> 
      [
        {
          name = "canon50d";
          active = true;
          device_match = [ `Label "EOS_*" ];
          mount_path = "/mnt/camera";
          search_subdirs = [ `Recurse "/DCIM" ];

          image_exts = [ "jpg"; "cr2"; ];
          image_meta_exts = ["xmp"];
          video_exts = [ "mov"; "avi" ];
          video_meta_exts = [];

          folders_append = `String "lala";
          folders_prepend = `Nothing;

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
      | Result.Ok _ -> print_endline "Sucesfully parsed input"
      | Result.Error e -> print_endline ("Error: "^e)



