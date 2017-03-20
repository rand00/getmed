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

include File

module Infix = struct 
  let ( /: ) dir file = (String.concat "" [dir; "/" ; file ])
end

let is_dir file = Sys.file_exists file && Sys.is_directory file
(** High level version of !Sys.is_directory - raises no exceptions,
    as the file is bothed checked for existense and dir-type *)

let ext file = String.(
  let _,ext = (try rsplit ~by:"." file with _ -> ("",""))
  in lowercase ext)
(** Get the extension of a file - if no dot is found in filename, then
    it returns the empty string.
    
    *Notice* - this function currently returns whatever is after the first
    dot found - so if the file has no extension, then abnormal results could
    occur. *)

