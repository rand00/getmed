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

open File.Infix
open BatResult.Monad

open Rc2.T
open Media_types
open Exceptions


(*goto think > user might want to append camera-name and have all 
  folders from diff. cameras in same dir, e.g. 'root/20170101_title_hi_sonyRx100'
*)
(*goto fix media types - take list of destinations from new settings*)

let concat_titles ~settings typ = 
  let s = settings in
  let c root = String.concat "" 
    (List.flatten
       [ [ root; "/"; Rc2.folder_prefix s; ];
         ( match s.folders_append with
         | "" -> [ "" ] 
         | _  -> [ "_"; s.folders_append ])])
  in match typ with 
  | `Img -> List.map c s.image_destinations
  | `Vid -> List.map c s.video_destinations
  | _ -> failwith "pass an `Img or `Vid type"


let dirs_fix ~settings () =
  let create_dir _ str = Folder.create_if_nonexistent str in
  let create_dirs dirs = List.fold_left_result create_dir true dirs
  in
  match settings.types_to_transfer with 
  | `Img -> create_dirs @@ concat_titles `Img ~settings
  | `Vid -> create_dirs @@ concat_titles `Vid ~settings
  | `All -> 
    ( create_dirs @@ concat_titles `Img ~settings >>= fun _ -> 
      create_dirs @@ concat_titles `Vid ~settings )
  | `None -> 
    ( Msg.term `Notif "fix media directories"
        [ "No media-files were present on the device." ];
      Bad MediaNotPresent )


type extensions = {
  image_exts : string list;
  image_meta_exts : string list;
  video_exts : string list;
  video_meta_exts : string list;
}

let make_extensions
    ?(image_exts=[])
    ?(image_meta_exts=[])
    ?(video_exts=[])
    ?(video_meta_exts=[])
    () =
  {
    image_exts;
    image_meta_exts;
    video_exts;
    video_meta_exts;
  }

let filter_extensions_by_rc settings =
  let s = settings in 
  match settings.types_to_transfer with
  | `None -> make_extensions ()
  | `Img ->
    make_extensions
      ~image_exts:s.image_exts
      ~image_meta_exts:s.image_meta_exts
      ()
  | `Vid ->
    make_extensions
      ~video_exts:s.video_exts
      ~video_meta_exts:s.video_meta_exts
      ()
  | `All ->
    make_extensions
      ~image_exts:s.image_exts
      ~image_meta_exts:s.image_meta_exts
      ~video_exts:s.video_exts
      ~video_meta_exts:s.video_meta_exts
      ()

let exts_std = {
  image_exts = [ "jpg"; "jpeg"; "cr2"; "arw" ];
  image_meta_exts = [ "xmp" ];
  video_exts = [ "mov"; "avi" ];
  video_meta_exts = [ "log"; "xml" ];
}


(*gomaybe generalize into RaUtil *)
(*goto test!*)
let rec traverse_tree
    ~extensions
    ~recurse
    dir
  =
  let e = extensions in
  let is_ext extensions path = List.mem (File.ext path) extensions in
  let save path typ = 
    Some {path; typ; size = (Unix.stat path).Unix.st_size}
  in
  let files = Sys.files_of dir
  in
  let filter_files mediatype extensions = files //@ fun elem -> 
      let path = ( dir /: elem ) in
      if is_ext extensions path then save path mediatype else None 
  in
  let filter_meta meta_type media_files extensions =
    if not @@ Enum.is_empty media_files then
      filter_files meta_type extensions
    else Enum.empty ()
  in
  let image_files = filter_files `Img e.image_exts
  and video_files = filter_files `Vid e.video_exts in
  let image_meta_files = filter_meta `Img_meta image_files e.image_meta_exts
  and video_meta_files = filter_meta `Vid_meta video_files e.video_meta_exts
  in
  let nested_files = (files //@ fun elem -> 
      let path =  ( dir /: elem ) in
      if File.is_dir path && recurse then 
        Some (traverse_tree path ~extensions ~recurse)
      else None
    ) |> Enum.flatten
  in List.enum [
    image_files;
    image_meta_files;
    video_files;
    video_meta_files;
    nested_files;
  ]
  |> Enum.flatten
                  

let which_media_types extract_type media = 
  let rec aux acc = function 
    | file :: rest_media -> (
      match extract_type file with
      | `Img -> 
        ( match acc with 
          | `Img | `None -> aux `Img rest_media
          | `Vid | `All -> `All ) 
      | `Vid -> 
        ( match acc with 
          | `Vid | `None -> aux `Vid rest_media
          | `Img | `All -> `All )
      | `All -> `All
      | _ -> aux acc rest_media
    )
    | _ -> acc
  in aux `None media

let search_aux search_subdir ~(settings:Rc2.device_config) = 
  let s = settings in
  let subdir, recurse = match search_subdir with
    | `Recurse s -> s, true
    | `Only s -> s, false in
  let dir = ( s.mount_path /: subdir ) in (
    match File.is_dir dir with 
    | true ->
      StateResult.Infix.(
        ((Result.catch (
             traverse_tree
               ~recurse
               ~extensions:(filter_extensions_by_rc settings)
           ) dir), settings)
        >>= (fun ~settings media_enum -> 
            let media_list = List.of_enum media_enum in
            let types_to_transfer =
              which_media_types
                (fun file -> file.typ)
                media_list
            in 
            match media_list with 
            | [] -> 
              ( Msg.term `Notif "media search"
                  [ "No media files present in the specified";
                    "device subfolder - aborting." ];
                ((Bad MediaNotPresent), settings) )
            | file_list -> ((Ok (file_list, types_to_transfer)), settings)))

    (*goto should we really fail here? - 
      maybe yes, but atleast inform user*)
    | false -> 
      ( Msg.term `Error "media search"
          [ "The device directory '"; dir; "', does not";
            "exist - aborting." ];
        ((Bad DeviceFolderNonExistent), settings) ))


module S = StateResult.Settings
open StateResult.Infix

(*goto use/make a version of monad that doesn't give settings as arg?*)
let search ~(settings:Rc2.device_config) () =
  let join_files_and_types acc subdir =
    acc
    >>= fun ~settings (acc_files, acc_types) -> 
    (search_aux subdir ~settings)
    >>= fun ~settings (files, types) -> 
    Ok (files @ acc_files, types :: acc_types), settings
  in
  let init = (Ok ([], []), settings) in
  List.fold_left join_files_and_types
    init
    settings.search_subdirs
  >>= fun ~settings (files, types_to_transfer_list) ->
  let types_to_transfer = 
    types_to_transfer_list
    |> which_media_types (fun t -> t)
  in (Ok files, { settings with types_to_transfer })
  

let copy_file ~settings file =
  let error str =
    Msg.term `Error ("copy file:"^file.path) 
        [ "An error '"; str;
          "' from program 'cp' occured while ";
          "trying to copy the file." ]
  in
  List.fold_left_result (fun return_code filename ->
      match return_code with
      | 0 -> 
        Result.catch (fun fn ->
            Sys.command @@
            String.concat " " [ "cp"; file.path; fn ]
          )
          filename
      | error_code -> Bad MediaCopyFailure
    )
    0
    (concat_titles file.typ ~settings)
  |> function
  | Ok 0 -> Ok ()
  | Ok n ->
    error @@ Int.to_string n;
    Bad MediaCopyFailure
  | Bad exn -> 
    error @@ Printexc.to_string exn;
    Bad MediaCopyFailure

let map_result f v = BatResult.Monad.(
    bind v @@ return%f
  )

let (>|=) v f = map_result f v 

let transfer ~settings media () =
  let open BatResult.Infix in
  let full_size = 
    List.fold_left (fun acc file -> acc + file.size) 0 media 
  in 
  let result_copy = 
    List.fold_left_result (fun trans_size file -> 
      Msg.progress full_size trans_size file;
      copy_file ~settings file >|= fun () -> 
      trans_size + file.size
    ) 0 media 
  in 
  let _ = print_endline "" 
  in
  result_copy >>= fun _ -> Ok media

(*warning: now doesn't check for remove media setting*)
let remove files ~recursive = 
  match files with 
  | [] -> Ok ()
  | _  -> 
    Sys.command @@
    String.concat " " (
      List.flatten @@
      ["rm"] ::
      (if recursive then ["-rf"] else []) ::
      [ files ]
    )
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
        Bad RemoveFailure )

let cleanup media ~settings () =
  match settings.cleanup with
  | `None -> Ok ()
  | `Remove_originals ->
    let media_files = List.map (fun {path} -> path) media
    in remove media_files ~recursive:false
  | `Format -> (*goto bette to really format for flash health? *)
    let files_at_mount =
      Sys.readdir settings.mount_path
      |> Array.to_list
    in remove files_at_mount ~recursive:true
  


