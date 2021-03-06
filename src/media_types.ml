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

type media_type = [ `Img | `Img_meta | `Vid | `Vid_meta | `All | `None ]

type media_file = {
  path : string;
  typ : media_type;
  size : int
}

(*goto map media_files to this type using destinations *)
type media_transfer = {
  mt_path : string;
  mt_dest : string;
  mt_typ : media_type;
  mt_size : int;
}

