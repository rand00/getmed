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
open Result.Monad
open Exceptions

(*goto move into File module *)
let escape_spaces = Pcre.replace ~pat:" " ~templ:"\\ "

(*goto move into File module *)
let escape = Printf.sprintf "'%s'"

let create_if_nonexistent folder = 
  match Sys.file_exists folder with
  | true  -> 
    ( match Sys.is_directory folder with 
    | true  -> 
      ( Msg.term `Notif "create media directory"
          [ "The specified folder '"; folder; "' already exists - ";
            "going to use this and prompt you if any files are to be ";
            "overwritten." ];
        Ok false )
    | false -> 
      ( Msg.term `Error "create media directory"
          [ "The mediadirectory specified, "; folder;
            " is not a directory." ];
        Bad CreateFolder ))
  | false -> ( match Sys.command ("mkdir -p "^(folder |> escape)) with
    | 0 -> begin
      Msg.term `Notif "create media directory"
        [ "The folder '"; folder; "' has now been created ";
          "for media transferring." ];
      Ok true
    end
    | _ -> begin
      Msg.term `Error "create media directory"
        [ "The folder '"; folder; "' could not get created. ";
          "Aborting." ];
      Bad CreateFolder 
    end )


module Name = struct 

  (*goto could have used printf here?..*)
  let pad_zero pad_full digit = String.(
    let digit_str = of_int digit in
    let lpad = pad_full - (length digit_str) in
    (^) (make (if lpad < 0 then 0 else lpad) '0') digit_str )

  let today () = Unix.(
    let t = localtime (time ()) in 
    String.concat "" 
      [ pad_zero 4 (t.tm_year+1900);
        pad_zero 2 (t.tm_mon +1); 
        pad_zero 2 t.tm_mday ] ) 

end

