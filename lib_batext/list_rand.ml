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

include List

let rec fold_left_result f acc = function
  | [] -> Ok acc
  | hd::tl -> (match f acc hd with 
    | Ok v -> fold_left_result f v tl
    | Bad e as bad -> bad)
