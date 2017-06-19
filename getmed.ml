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

open Settings
open Exceptions

open File.Infix

let handle_result = function 
  | ((Ok _), _) -> 
    ( Msg.term `Notif "main" [ "Getmed ran succesfully." ];
      exit 0 )
  | ((Bad (BeforeMounting exn )), _ ) -> raise exn
  | ((Bad exn_after_mounting), settings) ->
    ((match settings.unmount with 
    | true  -> ( Dev.unmount ~settings |> ignore )
    | false -> () );
     raise exn_after_mounting )


(**Run `getmed with super-user rights (for blkid and mounting)*)

let getmed ~settings ~cmdline_args = 
  let open StateResult.Infix in
  let module S = StateResult.Settings in
  begin 
    StateResult.return () ~settings
    >>= S.lift Rc.find 
    >>= Rc2.update
    >>= Args.update ~cmdline_args
    >>? Settings.print_if_debug

    (*>goto make general, depending on new settings*)
    >>= S.lift Dev.find
    >>= Dev.mount_smartly 
    >>@ Exceptions.wrap_renew (fun e -> BeforeMounting e)

    >>= Media.search (*goto depend on new settings*)
    >>? Settings.print_if_debug
    >>= fun media ~settings -> 
    StateResult.return () ~settings (*> goto all these could be >>? (but using same symbol)*)
    >>= S.read_ignore Media.dirs_fix
    >>= S.read_ignore (Media.transfer media)
    >>= S.read_ignore (Media.remove media)
    >>= S.read_ignore Dev.unmount
  end |> handle_result


let _ = 
  getmed 
    ~settings:(Rc2.std)
    ~cmdline_args:(Args.handle_all ())


