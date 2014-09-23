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
                  wrap_len) ]))
    

let term typ title ss = 
  String.concat "" 
    [ "Getmed:";
      (match typ with 
      | `Error -> "ERROR:"
      | `Notif -> "");
      title ^ ":\n";
      (termwrap ss) ]
  |> print_endline

let progress full_size trans_size file = 
  let open Media_types in begin
    Printf.printf "[%10d / %10d] Transferring '%35s'  [%15s]\r" 
      (trans_size+file.size) full_size file.path 
      (String.make (((Float.of_int trans_size) 
                     /. (Float.of_int full_size)) *. 15. 
                       |> Int.of_float) '|' );
    flush stdout;
  end

