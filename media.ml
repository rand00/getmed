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
open Media_types
open Exceptions

open File.Infix
open Result.Monad


let concat_title ~settings typ = 
  let s = settings in
  let c root = String.concat "" 
    (List.flatten
       [ [ root; "/"; s.title; ];
         ( match s.append_title with
         | "" -> [ "" ] 
         | _  -> [ "_"; s.append_title ])])
  in match typ with 
  | `Img -> c s.img_to_root
  | `Vid -> c s.vid_to_root
  | _ -> failwith "pass an `Img or `Vid type"


let dirs_fix ~settings = 
  match settings.types_to_transfer with 
  | `Img -> Folder.create_if_nonexistent (concat_title `Img ~settings)
  | `Vid -> Folder.create_if_nonexistent (concat_title `Vid ~settings)
  | `All -> 
    ( Folder.create_if_nonexistent (concat_title `Img ~settings)
      >>= fun _ -> 
      Folder.create_if_nonexistent (concat_title `Vid ~settings) )
  | `None -> 
    ( Msg.term `Notif "fix media directories"
        [ "No media-files were present on the device." ];
      Bad MediaNotPresent )


type extensions = {
  imgs : string list;
  vids : string list;
}

let exts_std = {
  imgs = [ "jpg"; "jpeg"; "cr2" ];
  vids = [ "mov"; "log" ] (*'log' for magick lantern metadata*)
}


let rec traverse_tree dir ~exts =
  let is_img path = List.mem (File.ext path) exts.imgs
  and is_vid path = List.mem (File.ext path) exts.vids 
  in
  let save path typ = Unix.(
    Some (Enum.singleton {path; typ; size = (Unix.stat path).st_size}))
  in
  let rec aux path =
    (Sys.files_of path) //@ 
    (fun file -> 
       let full_path =  ( path /: file ) in
       if File.is_dir full_path then 
         Some (aux full_path ~exts)
       else if path |> is_img then save full_path `Img
       else if path |> is_vid then save full_path `Vid 
       else None )
    |> Enum.flatten
  in aux dir

let which_media_types media = 
  let rec aux acc = function 
    | { typ = `Img } :: rest_media -> 
      ( match acc with 
      | `Img | `None -> aux `Img rest_media
      | `Vid | `All -> `All ) 
    | { typ = `Vid } :: rest_media -> 
      ( match acc with 
      | `Vid | `None -> aux `Vid rest_media
      | `Img | `All -> `All )
    | { typ = `None } :: rest_media -> aux acc rest_media
    | _ -> acc
  in aux `None media

let search_exts exts ~settings = 
  let s = settings in
  let subdir = ( s.mount_path /: s.search_subdir )
  in ( match File.is_dir subdir with 

  | true -> Settings.SetResMonad.(
    (return ~settings 
       (Result.catch (traverse_tree ~exts) subdir))

    >>+ (fun media_enum ~settings -> 
      let media_list = List.of_enum media_enum in
      let settings = { settings with 
        types_to_transfer = which_media_types media_list }
      in 
      match media_list with 
      | [] -> 
        ( Msg.term `Notif "media search"
            [ "No media files present in the specified";
              "device subfolder - aborting." ];
          ((Bad MediaNotPresent), settings) )
      | file_list -> ((Ok file_list), settings) )) 

  | false -> 
    ( Msg.term `Error "media search"
        [ "The device directory '"; subdir; "', does not";
          "exist - aborting." ];
      ((Bad DeviceFolderNonExistent), settings) ))


let search () ~settings = 
  match settings.types_to_transfer with
  | `Img -> search_exts {exts_std with vids = []} ~settings
  | `Vid -> search_exts {exts_std with imgs = []} ~settings
  | `All -> search_exts exts_std ~settings
  | `None -> (Bad MediaNotPresent), settings


let copy_with ~settings file = 
  (Sys.command 
     (String.concat " " 
        [ "cp"; file.path; (concat_title file.typ ~settings) ] ))
  |> function
      | 0 -> Ok ()
      | err -> begin
        Msg.term `Error ("copy file:"^file.path) 
          [ "An error '"; String.of_int err; 
            "' from program 'cp' occured while ";
            "trying to copy the file." ];
        Bad MediaCopyFailure
      end

let transfer media ~settings = 
  let full_size = 
    List.fold_left (fun acc file -> acc + file.size) 0 media 
  in 
  let result_copy = 
    List.fold_left_result (fun trans_size file -> 
      Msg.progress full_size trans_size file;
      copy_with ~settings file 
      >>= fun () -> 
      Ok (trans_size + file.size)
    ) 0 media 
  in 
  let _ = print_endline "" 
  in
  result_copy >>= fun _ -> Ok media

let remove ~settings media = 
  match settings.remove_media with
  | false -> Ok ()
  | true  -> 
    (match media with 
    | [] -> Ok ()
    | _  -> 
      (Sys.command 
         (String.concat " " 
            ("rm" :: 
                (List.map (fun file -> file.path) 
                   media))))
    |> function
        | 0 -> 
          ( Msg.term `Notif "remove media"
              [ "Media was succesfully removed ";
                "from device." ];
            Ok () )
        | errcode -> 
          ( Msg.term `Error "remove media"
              [ "Something went wrong while removing ";
                "media from the device - errorcode from ";
                "command 'rm' was '"; String.of_int errcode;
                "'." ];
            Bad RemoveFailure ))

