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
           | _  -> [ "_"; s.folders_append ]);
         ( match s.folders_append_cam_name with
           | false -> [ "" ] 
           | true  -> [ "."; s.name ])
       ]
    )
  in match typ with 
  | `Img -> List.map c s.image_destinations
  | `Img_meta -> List.map c s.image_destinations              
  | `Vid -> List.map c s.video_destinations
  | `Vid_meta -> List.map c s.video_destinations
  | _ -> failwith "pass an `Img or `Vid type"


let dirs_fix ~settings () =
  let create_dir _ folder =
    let created = Folder.create_if_nonexistent folder in
    let _ = try Unix.set_user_as_owner folder with _ ->
      Msg.term `Notif "setup media directories"
        [ "Couldn't change owner of folder '";
          folder;
          "' to user." ]
    in created
  in
  let create_dirs dirs = List.fold_left_result create_dir true dirs
  in
  match settings.types_to_transfer with 
  | `Img -> create_dirs @@ concat_titles `Img ~settings
  | `Vid -> create_dirs @@ concat_titles `Vid ~settings
  | `All -> 
    ( create_dirs @@ concat_titles `Img ~settings >>= fun _ -> 
      create_dirs @@ concat_titles `Vid ~settings )
  | `None -> 
    ( Msg.term `Notif "setup media directories"
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
               ~extensions:(
                 filter_extensions_by_rc settings
                 |> fixup_extensions
               )
           ) dir), settings)
        >>= (fun ~settings media_list -> 
            let types_to_transfer =
              which_media_types
                (fun file -> file.typ)
                media_list
            in 
            match media_list with 
            | [] -> 
              ( Msg.term `Notif "media search"
                  [ "No media files present in the specified ";
                    "device subfolder, '"; subdir;"'." ];
                (Ok ([], `None), settings) )
            | file_list -> ((Ok (file_list, types_to_transfer)), settings)))

    (*goto should we really fail here? - maybe yes*)
    | false -> 
      ( Msg.term `Error "media search"
          [ "The device directory '"; dir; "', does not ";
            "exist." ];
        ((Bad DeviceFolderNonExistent), settings) ))


module S = StateResult.Settings
open StateResult.Infix

let search ~(settings:Rc2.device_config) () =
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
    settings.search_subdirs
  >>= fun ~settings (files, types_to_transfer_list) ->
  let types_to_transfer = 
    types_to_transfer_list
    |> which_media_types (fun t -> t)
  in (Ok files, { settings with types_to_transfer })


let copy_file ~settings ~progress file =
  let open BatResult.Monad in
  let error str =
    print_endline "";
    Msg.term `Error ("copy file:"^file.path) 
        [ "An error '"; str;
          "' occured on copying file." ]
  in
  List.fold_left_result
    (fun () destination_path ->
       let destination_file =
         (destination_path /: Filename.basename file.path) in
       begin match Sys.file_exists destination_file with
         | false -> Ok destination_path
         | true ->
           print_endline "";
           Msg.term `Error "copy file" [
             "Error while trying to copy file '";
             file.path;
             "' - the file already exists at '";
             destination_file;
             "'.";
           ];
           Bad MediaCopyFailure
       end
       >>= Result.catch (fun dest ->
           let progress = progress ~file in
           Unix.cp ~progress file.path dest
         )
       |> function
       | Ok _ as ok -> ok
       | Bad MediaCopyFailure as b -> b
       | Bad exn ->
         error @@ Printexc.to_string exn;
         Bad MediaCopyFailure
    )
    ()
    (concat_titles file.typ ~settings)

let map_result f v = BatResult.Monad.(
    bind v @@ return%f
  )

let (>|=) v f = map_result f v 

let transfer ~settings media () =
  let open BatResult.Infix in
  let full_transfer_size = 
    List.fold_left (fun acc file -> acc + file.size) 0 media in
  let start_time = Unix.gettimeofday () in
  let result_copy = 
    List.fold_left_result (fun prev_transf file ->
        begin
          let c i = LTerm_style.index i in
          (*>goto supply as theme - set in config*)
          let c1 (*anglebrackets*) = c 1 in
          let c2 (*special text*) = c 2 in
          let c3 (*numbers*) = c 3 in
          let filename_markup = LTerm_text.([
              S "Copying '";
              B_fg c2; S file.path; E_fg;
              S "'"
            ]) in
          let open Lwt in
          Lazy.force LTerm.stdout >>= fun stdout ->
          LTerm.clear_line_next stdout >>= fun () ->
          LTerm.fprintls stdout @@ LTerm_text.eval filename_markup
        end
        |> Lwt_main.run;

        let progress =
          Msg.progress
            ~start_time
            ~full_transfer_size
            ~prev_transf
        in
        copy_file ~settings ~progress file >|= fun () -> 
        prev_transf + file.size
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
          [ "Errorcode from command 'rm' was '";
            String.of_int errcode;
            "'." ];
        Bad RemoveFailure )

let cleanup media ~settings () =
  match settings.cleanup with
  | `None -> Ok ()
  | `Remove_originals ->
    let media_files = List.map (fun {path} -> Folder.escape path) media
    in remove media_files ~recursive:false
  | `Format -> (*goto better to really format for flash health? *)
    let files_at_mount =
      Sys.readdir settings.mount_path
      |> Array.to_list
      |> List.map (fun fn -> settings.mount_path /: fn)
    in
    if not @@ List.for_all Sys.file_exists files_at_mount then 
      ( Msg.term `Error "cleanup"
          [ "Some of the files we want to remove doesn't exist." ];
        Bad RemoveFailure )
    else 
      remove files_at_mount ~recursive:true
  


