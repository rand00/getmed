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

exception CreateFolder
exception RcNotFound

(*device*)
exception DeviceFolderNonExistent 
exception DeviceNotPresent

(*mounting*)
exception MountFolderNotEmpty
exception MountError
exception UnMountFailure

(*transferring*)
exception MediaNotPresent
exception MediaCopyFailure
exception RemoveFailure

(*wrapper exceptions*)
exception BeforeMounting of exn

let wrap_renew wrapper exn = 
  match exn with 
  | BeforeMounting exn' -> wrapper exn'
  | _ -> wrapper exn

(** Wraps a given exn-type in a wrapper-exn to be able to more
    precisely specify where in the computation something went 
    wrong. !wrap_renew matches on existing wrapper-exn's and 
    replaces earlier wrappers.*)

