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

include Arg 

module Simple = struct 

  type anytype = 
    | Int of int
    | Float of float
    | String of string

  type to_type = To_int | To_float | To_string 

  let string_to typ str = 
    let eprint = Printf.eprintf "You were supposed to pass an %s as argument, but you passed '%s'"
    in
    match typ with
    | To_int   -> (try Some (Int (String.to_int str)) with _ -> 
        eprint "integer" str; None )
    | To_float -> (try Some (Float (String.to_float str)) with _ -> 
        eprint "float" str; None)
    | To_string -> Some (String str)

  let parse ~doc = 
    let args = args () 
    in match peek args with
    | Some "-help" | Some "--help" -> 
      print_endline doc; exit 0
    | _ -> 
      List.map (fun typ ->
          (match get args with 
           | Some s -> string_to typ s
           | _ -> None ))

end


