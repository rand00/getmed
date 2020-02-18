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

let folder_prefix settings =
  match settings.device.folders_prepend with
  | `Date -> Folder.Name.today ()
  | `String s -> s
  | `Nothing -> ""

let concat_titles ~settings typ = 
  let s = settings in
  let c root =
    let title = match s.device.folders_append with
      | "" -> "" 
      | _  -> "_" ^ s.device.folders_append
    and cam = match s.device.folders_append_cam_name with
      | false -> "" 
      | true  -> "." ^ s.device.name
    in
    let folder_name = folder_prefix s ^ title ^ cam in
    Fpath.(v root / folder_name |> to_string) 
  in
  match typ with 
  | `Img -> List.map c s.device.image_destinations
  | `Img_meta -> List.map c s.device.image_destinations              
  | `Vid -> List.map c s.device.video_destinations
  | `Vid_meta -> List.map c s.device.video_destinations
  | _ -> failwith "pass an `Img or `Vid type"


let dirs_fix ~settings () =
  let s, colors = settings, settings.colors in 
  let create_dir _ folder =
    let created = Folder.create_if_nonexistent ~colors folder in
    let _ = try Unix.set_user_as_owner folder with _ ->
      Msg.term ~colors `Notif "setup media directories"
        [ "Couldn't change owner of folder '";
          folder;
          "' to user." ]
    in created
  in
  let create_dirs dirs = List.fold_left_result create_dir true dirs
  in
  match s.device.types_to_transfer with 
  | `Img -> create_dirs @@ concat_titles `Img ~settings
  | `Vid -> create_dirs @@ concat_titles `Vid ~settings
  | `All -> 
    ( create_dirs @@ concat_titles `Img ~settings >>= fun _ -> 
      create_dirs @@ concat_titles `Vid ~settings )
  | `None -> (
      Msg.term ~colors `Notif "setup media directories"
        [ "No media-files were present on the device." ];
      Bad MediaNotPresent
    )


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

let remove_predot s =
  if String.starts_with s "." then
    String.sub s 1 (String.length s - 1)
  else
    s

let fixup_extensions es =
  let fix ss = List.map (String.lowercase%remove_predot) ss in
  {
    image_exts = fix es.image_exts;
    image_meta_exts = fix es.image_meta_exts;
    video_exts = fix es.video_exts;
    video_meta_exts = fix es.video_meta_exts;
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
  let files = Sys.files_of dir |> List.of_enum
  in
  let filter_files mediatype extensions =
    files
    |> List.filter_map (
      fun elem -> 
        if is_ext extensions elem then
          save (dir /: elem) mediatype
        else None
    )
  in
  let filter_meta meta_type media_files extensions =
    match media_files with
    | [] -> []
    | _  -> filter_files meta_type extensions
  in
  let image_files = filter_files `Img e.image_exts
  and video_files = filter_files `Vid e.video_exts in
  let image_meta_files =
    filter_meta `Img_meta image_files e.image_meta_exts
  and video_meta_files =
    filter_meta `Vid_meta video_files e.video_meta_exts
  in
  let nested_files = 
    files
    |> List.filter_map (fun elem -> 
      let path =  ( dir /: elem ) in
      if File.is_dir path && recurse then 
        Some (traverse_tree path ~extensions ~recurse)
      else None )
    |> List.flatten
  in List.flatten [
    image_files;
    image_meta_files;
    video_files;
    video_meta_files;
    nested_files;
  ]
                  

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

let search_aux search_subdir ~settings = 
  let (s, colors) = settings, settings.colors in
  let msg = Msg.term ~colors in
  let subdir, recurse = match search_subdir with
    | `Recurse s -> s, true
    | `Only s -> s, false in
  let dir = ( s.device.mount_path /: subdir ) in (
    match File.is_dir dir with 
    | true ->
      StateResult.Infix.(
        ((Result.catch (
             traverse_tree
               ~recurse
               ~extensions:(
                 filter_extensions_by_rc s.device
                 |> fixup_extensions
               )
           ) dir), settings)
        >>= (fun ~settings media_list -> 
            let types_to_transfer =
              which_media_types
                (fun (file:media_file) -> file.typ)
                media_list
            in 
            match media_list with 
            | [] -> 
              ( msg `Notif "media search"
                  [ "No media files present in the specified ";
                    "device subfolder, '"; subdir;"'." ];
                (Ok ([], `None), settings) )
            | file_list -> ((Ok (file_list, types_to_transfer)), settings)))

    (*goto should we really fail here? - maybe yes*)
    | false ->
      msg `Notif "media search"
        [ "The device directory '"; dir; "', does not exist." ];
      (Ok ([], `None), settings)
  )
      (* ( msg `Error "media search"
       *     [ "The device directory '"; dir; "', does not ";
       *       "exist." ];
       *   ((Bad DeviceFolderNonExistent), settings) )) *)


module S = StateResult.Settings
open StateResult.Infix

let search ~settings () =
  let s = settings in 
  let join_files_and_types acc subdir =
    acc
    >>= fun ~settings (acc_files, acc_types) -> 
    search_aux subdir ~settings
    >>= fun ~settings (files, types) -> 
    Ok (files @ acc_files, types :: acc_types), settings
  in
  let init = (Ok ([], []), settings) in
  List.fold_left join_files_and_types
    init
    s.device.search_subdirs
  >>= fun ~settings (files, types_to_transfer_list) ->
  let types_to_transfer = 
    types_to_transfer_list
    |> which_media_types (fun t -> t)
  in (Ok files, ({
      s with device = { s.device with types_to_transfer }
    }))


let copy_file ~settings ~progress file file_dest =
  let s, colors = settings, settings.colors in
  let msg = Msg.term ~colors in
  let open BatResult.Monad in
  let error str =
    print_endline "";
    msg `Error ("copy file:"^file.path) 
      [ "An error '"; str; "' occured on copying file." ]
  in
  let file_dest_full = file_dest /: Filename.basename file.path in
  begin match Sys.file_exists file_dest_full with
    | false -> Ok ()
    | true ->
      print_endline "";
      msg `Error "copy file" [
        "Error while trying to copy file '";
        file.path;
        "' - the file already exists at '";
        file_dest_full;
        "'.";
      ];
      Bad MediaCopyFailure
  end
  >>= Result.catch (fun () ->
    let progress = progress ~file in
    Unix.cp ~progress file.path file_dest
  )
  |> function
  | Ok _ as ok -> ok
  | Bad MediaCopyFailure as b -> b
  | Bad exn ->
    error @@ Printexc.to_string exn;
    Bad MediaCopyFailure


let map_result f v = BatResult.Monad.(
    bind v @@ return%f
  )

let (>|=) v f = map_result f v 

(*goto rewrite to use new transfers type instead of media internally;
  . make new type 
  . use internally
*)
let transfer ~settings media () =
  let s, colors = settings, settings.colors in 
  let open BatResult.Infix in
  (*goto fold through transfer-type *)
  let transfers_of_file f = 
    concat_titles f.typ ~settings
    |> List.map (fun dest -> f, dest)
  in
  let transfers = List.map transfers_of_file media in
  let full_transfer_size = 
    List.fold_left (fun acc l ->
        List.fold_left (fun acc (file, _) -> 
            acc + file.size
          ) acc l
      ) 0 transfers in
  let start_time = Unix.gettimeofday () in
  let result_copy =
    List.fold_left_result (fun prev_transf file_transfers ->
        List.fold_left_result (fun prev_transf (file, file_dest) -> 
            Msg.term_file_copy colors file;
            let progress =
              Msg.progress
                ~colors 
                ~start_time
                ~full_transfer_size
                ~prev_transf
            in
            copy_file ~settings ~progress file file_dest >|= fun () -> 
            prev_transf + file.size
          ) prev_transf file_transfers
      ) 0 transfers
  in 
  let _ = print_endline "" 
  in
  result_copy >>= fun _ -> Ok media

(*warning: now doesn't check for remove media setting*)
let remove files ~recursive ~colors =
  let msg = Msg.term ~colors in
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
      ( msg `Notif "remove media"
          [ "Media was succesfully removed ";
            "from device." ];
        Ok () )
    | errcode -> 
      ( msg `Error "remove media"
          [ "Errorcode from command 'rm' was '";
            String.of_int errcode;
            "'." ];
        Bad RemoveFailure )

let cleanup media ~settings () =
  let s, colors = settings, settings.colors in 
  match s.device.cleanup with
  | `None ->
    Msg.term ~colors `Notif "cleanup" [
      "Not going to cleanup device '"; s.device.name; "'."
    ];
    Ok ()
  | `Remove_originals ->
    let media_files = List.map (fun {path} -> Folder.escape path) media
    in remove media_files ~recursive:false ~colors
  | `Format -> (*goto better to really format for flash health? *)
    let files_at_mount =
      Sys.readdir s.device.mount_path
      |> Array.to_list
      |> List.map (fun fn -> s.device.mount_path /: fn)
    in
    if not @@ List.for_all Sys.file_exists files_at_mount then 
      ( Msg.term ~colors `Error "cleanup"
          [ "Some of the files we want to remove doesn't exist." ];
        Bad RemoveFailure )
    else 
      remove files_at_mount ~recursive:true ~colors
  


