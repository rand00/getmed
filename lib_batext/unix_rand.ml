(*
  Core_rand - a library wrapper fro Batteries Included that adds some 
  extra functionality and incorporates other lib's with Batteries.

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

include Unix

let command_getlines cmd = 
  Unix.open_process_in cmd |> IO.lines_of

let command_getstr cmd = 
  Unix.open_process_in cmd |> IO.read_all

let term_ncolumns () = 
  match command_getstr "stty size" with
  | <:re< ["0"-"9"]+ " " (["0"-"9"]+ as cols) >> -> Int.of_string cols
  | _ -> 60

let get_command_output cmd =
  let cmd_out = Unix.open_process_in cmd in
  let outputs, exn = List.unfold_exc (fun () -> input_line cmd_out) in
  let status = Unix.close_process_in cmd_out in
  if exn <> End_of_file then raise exn;
  (status, outputs) 
(*From *UnixJunkie* - to come in later batteries release*)

let cp ~progress fi fo =
  let fo =
    if Sys.is_directory fo then
      let filename = Filename.basename fi in
      Filename.concat fo filename
    else fo
  in
  let i = File.open_in fi
  and o, pos_cb = IO.pos_out @@ File.open_out fo in
  let size_4k = Int.pow 2 12 in
  let size_1m = Int.pow 2 20 in
  let buffer = size_1m / 10 in
  begin
    try
      while true do
        IO.nread i buffer |> IO.nwrite o;
        progress @@ pos_cb ()
      done
    with IO.No_more_input -> ()
  end;
  IO.close_in i;
  IO.close_out o

