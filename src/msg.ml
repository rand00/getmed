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

let termwrap 
    ?(initial_nonwrap=0) 
    (*options for textwrap >>*)
    ?(initial_indent="  ") 
    ?(subsequent_indent="  ") 
    ?(drop_whitespace=true)
    ?(replace_whitespace=true)
    ss = 
  String.concat "" ss
  |> (fun str -> 
      let wrap_len = Sys.term_ncolumns () in
      let (init_line, rest_str) = 
        (match initial_nonwrap with
         | 0 -> [], str
         | n -> 
           let str_len = String.length str in
           let init_maxlen = (min (wrap_len - n) str_len) in
           let init_len = 
             (if str_len > init_maxlen then
                String.rfind_from str (init_maxlen -1) " " 
              else 
                str_len )
           in ( [ String.sub str 0 init_len ], String.lchop ~n:init_len str ))
      in
      let open Wrapper in
      String.concat "\n"
        (List.flatten
           [ init_line;
             rest_str |> wrap 
               (make 
                  ~initial_indent 
                  ~subsequent_indent
                  ~drop_whitespace
                  ~replace_whitespace
                  wrap_len)
           ]))


let term typ title ss = 
  String.concat "" 
    [ "getmed:";
      (match typ with 
       | `Error -> "ERROR:"
       | `Notif -> "");
      title ^ ":\n";
      (termwrap ss) ]
  |> print_endline

let human_readable_bytes bytes =
  let r p = float @@ Int.pow 10 p in
  let kb = r 3
  and mb = r 6
  and gb = r 9
  and tb = r 12
  and bytes = float bytes
  in
  let amount, category =
    if bytes > tb then bytes /. tb, "TB"
    else if bytes > gb then bytes /. gb, "GB"
    else if bytes > mb then bytes /. mb, "MB"
    else if bytes > kb then bytes /. kb, "KB"
    else bytes, "B"
  in Printf.sprintf "%.0f%s" amount category 

let progress ~full_size ~trans_size ~prev_len file = 
  let open Media_types in
  let f = Float.of_int in
  let progress_len =
    (f trans_size /. f full_size) *. 15. 
    |> Int.of_float in
  let progress_bar = String.make progress_len '|' in
  let final_string =
    Printf.sprintf "> [%-15s] [%4s / %4s] Transferring '%s'  \r" 
      progress_bar
      (human_readable_bytes (trans_size+file.size))
      (human_readable_bytes full_size)
      file.path in
  begin
    print_string @@ String.make prev_len ' ' ^ "\r";
    print_string final_string;
    flush stdout;
  end;
  String.length final_string

