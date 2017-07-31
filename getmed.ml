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
open File.Infix
open Exceptions

open Rc2.T
open StateResult.Infix
module S = StateResult.Settings

let handle_errors_and_unmount = function 
  | ((Ok _), _) -> 
    ( Msg.term `Notif "main" [ "Getmed ran succesfully." ];
      Ok () )
  | ((Bad (BeforeMounting exn )), _ ) -> raise exn
  | ((Bad exn_after_mounting), settings) ->
    ((match settings.unmount with 
    | true  -> ( Dev.unmount ~settings |> ignore )
    | false -> () );
     raise exn_after_mounting )

(* goto rewrite all modules to use rresult? 
   (if yes, do after it all works again) *)

(*>goto 
  . make bind automatically catch exceptions?
    . think
*)
let handle_devices () ~settings =
  let rec aux = function
    | [] -> Ok (), settings
    | dev :: tl -> begin
        StateResult.return () ~settings:dev
        >>= Dev.find 
        >>= Dev.mount_smartly
        >>@ Exceptions.wrap_renew (fun e -> BeforeMounting e)

        >>= Media.search 
        >>? Rc2.print_if_debug

        >>= fun ~settings media -> 
        StateResult.return () ~settings
        >> (S.read @@ Media.dirs_fix)
        >> (S.read @@ Media.transfer media)
        >> (S.read @@ Media.cleanup media)
        >> (S.read @@ Dev.unmount)
        |> handle_errors_and_unmount
        >>= aux tl
      end in
  aux settings.devices



(**Run `getmed with super-user rights (for blkid and mounting)*)

let getmed ~settings ~cmdline_args = 
  begin 
    StateResult.return ~settings ()
    >>= S.lift Rc2.find 
    >>= Rc2.update
    (*goto make rc be in a wrapper with extra fields - check zim?*)
    >>= Rc2.update_cli ~args:cmdline_args
    >>? Settings.print_if_debug
    >>= handle_devices
  end |> handle_result


let _ = 
  getmed 
    ~settings:(Rc2.std)
    ~cmdline_args:(Args.handle_all ())


