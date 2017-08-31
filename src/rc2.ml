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

module ResultStd = Result
open Batteries 
open BatExt
open File.Infix
module Result = ResultStd

open Exceptions

module T = struct 

  type device_config = {
    name : string;
    active : bool;

    device_match : [ `Uuid of string | `Label of string ] list;

    mount_path : string;
    search_subdirs : [ `Recurse of string | `Only of string ] list;

    types_to_transfer : [ `Img | `Vid | `All | `None ];
    
    image_exts : string list;
    image_meta_exts : string list;

    video_exts : string list;
    video_meta_exts : string list;

    (*gomaybe have separate for vid/img?*)
    (*gomaybe have option that appends camera-name
      e.g. 20172312_title_sony-rx100 for having from all cameras in
      same folder
    *)
    folders_append : string;
    folders_append_cam_name : bool;
    (*gomaybe later would be useful to have time as well?*)
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

  (*goto report github issue that deriving signals 'config' as missing
    where it's 'boolean'
  *)
  type config = {
    debug : bool;
    devices : device_config list;
  } [@@ deriving yojson]

end
include T

let folder_prefix settings =
  match settings.folders_prepend with
    | `Date -> Folder.Name.today ()
    | `String s -> s
    | `Nothing -> ""

let template_rc = {
  name = "camera-name";
  active = true;
  device_match = [ `Label "DEVICE-LABEL-REGEX" ];
  mount_path = "/place/to/mount/device";
  search_subdirs = [ `Recurse "/subtree/within/device/to/search" ];
  types_to_transfer = `All;
  
  image_exts = [ "jpg"; "cr2"; "arw"; ];
  image_meta_exts = ["xmp"];
  video_exts = [ "mov"; "avi"; "mp4" ];
  video_meta_exts = [ "xml" ];

  folders_append = "folder_appended_title";
  folders_append_cam_name = false;
  folders_prepend = `String "folder_prepend_string";

  image_destinations = [ "/image/root/destinations" ];
  video_destinations = [ "/video/root/destinations" ];

  cleanup = `Remove_originals;
  unmount = true;
}

let std : T.config = {
  debug = false;
  devices = [];
}

let foo = {
  name = "std-camera";
  active = true;
  device_match = [ `Label "EOS_*" ];
  mount_path = "/tmp/getmed-tmp";
  search_subdirs = [ `Recurse "/" ];
  types_to_transfer = `All;
  
  image_exts = [ "jpg"; "cr2"; "arw"; ]; (*goto add more*)
  image_meta_exts = ["xmp"];
  video_exts = [ "mov"; "avi"; "mp4" ];
  video_meta_exts = [ "xml"; "log" (*magick lantern*) ];

  folders_append = "";
  folders_append_cam_name = false;  
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
let read_from_file ~settings file =
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
(*
let update_one_dev arg dev =
  match arg with
  | `Append_title s -> { dev with folders_append = s }
  | _ -> dev 

let update_config arg config =
  match arg with
  | `Debug 

let update_one_arg rc arg = List.map (update_one_dev arg) rc

let update_cli_aux rc args = List.fold_left update_one_arg rc args

let update_cli ~args ~settings () =
  BatResult.Ok (),
  { settings with
    devices = update_cli_aux settings.devices args }
*)
(*goo update_cli with lift*) 

let to_string config =
  config 
  |> config_to_yojson
  |> Yojson.Safe.pretty_to_string

let device_config_to_string config =
  config
  |> device_config_to_yojson
  |> Yojson.Safe.pretty_to_string

(*goto implement*)
let get_template : unit -> string
  = fun () ->
    { std with devices = [template_rc] }
    |> to_string

(*not used for now*)
let print_if_debug ~settings pass_on = 
  let () = match settings.debug with 
    | false -> ()
    | true -> 
      let sep = (String.make 35 '>') in
      let print_frame s = 
        print_endline 
          (String.concat "\n" [ sep; s; sep ]) in
      print_frame (to_string settings) 
  in ()

let print_dev_config ~debug (pass_on, settings) = 
  let () = match debug with 
    | false -> ()
    | true -> 
      let sep = (String.make 35 '>') in
      let print_frame s = 
        print_endline 
          (String.concat "\n" [ sep; s; sep ]) in
      print_frame (device_config_to_string settings)
  in ()

(*
let _ =
  let test = false in
  if test then 
    match Sys.argv with
    | [| _; "print" |] -> 
      { devices = [
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
      ]}
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
*)

