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

type t = {
  types_to_transfer : Media_types.media;
  mount_path : string;
  search_subdir : string;
  img_to_root : string;
  vid_to_root : string;
  title : string;
  append_title : string;
  remove_media : bool;
  unmount : bool;
  debug : bool;
}

type elem = 
| Append_title of string
| Debug of bool
(*type to be extended if more cmd-line args get relevant*)

let empty = {
  types_to_transfer = `None;
  mount_path = "";
  search_subdir = "";
  img_to_root = "";
  vid_to_root = "";
  title = "";
  append_title = "";
  remove_media = false;
  unmount = false;
  debug = false;
}


let eval_put settings_thunk = StateResult.(
  try return (Ok ()) ~settings:(settings_thunk ())
  with Not_found -> 
    Msg.term `Error "settings" 
      [ "The 'HOME' environment variable was not set..." ];
    return (Bad Not_found) ~settings:empty 
)
      
let catch f v ~settings = try f v ~settings with exn -> (Bad exn), settings
let catch_map  f v ~settings = try (f v), settings with exn -> (Bad exn), settings

let update_types_to_transfer media_types ~settings = Media_types.(

  let types_to_transfer = ( match media_types with
  | { img = true; vid = true } -> `All
  | { img = true } -> `Img
  | { vid = true } -> `Vid 
  | _ -> `None )

  in { settings with types_to_transfer }
)

let to_string {
  types_to_transfer;
  mount_path;
  search_subdir;
  img_to_root;
  vid_to_root;
  title;
  append_title;
  remove_media;
  unmount;
  debug;
} = 
  (String.concat "\n"
     [ "types_to_transfer: " ^ 
         ( match types_to_transfer with 
         | `Img -> "`Img"
         | `Vid -> "`Vid"
         | `All -> "`All" 
         | `None -> "`None" );
       "mount_path: " ^ mount_path;
       "search_subdir: " ^ search_subdir;
       "img_to_root: " ^ img_to_root;
       "vid_to_root: " ^ vid_to_root;
       "title: " ^ title;
       "append_title: " ^ append_title;
       "remove_media: " ^ (Bool.to_string remove_media);
       "unmount: " ^ (Bool.to_string unmount);
       "debug: " ^ (Bool.to_string debug) ]) 

let print_if_debug pass_on ~settings = 
  let () = match settings.debug with 
    | false -> ()
    | true -> 
      let sep = (String.make 35 '>') in
      let print_frame s = 
        print_endline 
          (String.concat "\n" [ sep; s; sep ]) in
      print_frame (to_string settings) 
  in Ok (pass_on)