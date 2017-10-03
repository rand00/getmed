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
open Exceptions
open Rc2.T
open File.Infix
open StateResult.Infix
module S = StateResult.Settings

(* goto rewrite all modules to use rresult? *)

let handle_errors = function 
  | ((Ok _), dev) ->
    begin
      Msg.term `Major "handler" 
        [ "Ran succesfully for device '"; dev.name;"'." ];
      Ok ()
    end
  | (Bad DeviceNotPresent, dev) ->
    begin
      Msg.term `Notif "handler"
        [ "Trying next device instead." ];
      Ok ()
    end
  | (Bad (BeforeMounting exn), dev) ->
    begin
      Msg.term `Error "handler" [
        "Failed on device '"; dev.name; "' with the ";
        "error: \n\t";
        Printexc.to_string exn;
      ];
      exit 1
    end
  | (Bad exn_after_mounting, dev) ->
    begin
      Msg.term `Error "handler" [
        "Failed on device '"; dev.name; "' with the ";
        "error: \n\t";
        Printexc.to_string exn_after_mounting;
      ];
      Dev.unmount ~settings:dev () |> ignore;
      exit 1
    end

let bind_result f v = Result.Monad.bind v f 

(*>goto 
  . make bind automatically catch exceptions?
    . think
*)
let handle_devices ~(settings:Rc2.config) () =
  let getmed_settings = settings in
  let rec loop devices () =
    match devices with
    | dev :: tl when dev.active ->
      Msg.term `Major "handler" [
        "Starting transfer for device '";
        dev.name;
        "'."
      ];
      begin
        StateResult.return ~settings:dev ()
        >>= Dev.find 
        >>= Dev.mount_smartly
        >>@ Exceptions.wrap_renew (fun e -> BeforeMounting e)

        >>= Media.search 
        >>? Rc2.print_dev_config ~debug:getmed_settings.debug

        >>= fun ~settings media -> 
        StateResult.return () ~settings
        >> S.read @@ Media.dirs_fix
        >> S.read @@ Media.transfer media
        >> S.read @@ Media.cleanup media
        >> S.read @@ Dev.unmount
      end
      |> handle_errors
      |> bind_result (loop tl)
    | _ -> Ok ()
  in
  loop settings.devices ()

let print_success ~settings () = Ok (
    let msg = match List.exists (fun d -> d.active) settings.devices with
      | false -> "No active devices."
      | true  -> "Ran succesfully for active devices."
    in
    Msg.term `Major "main" [ msg ]
  ), settings  

let getmed ~(settings:Rc2.config) ~cli_args = 
  begin match Args.split_config cli_args with
    | Some path, cli_args -> Ok (path, cli_args), settings
    | None     , cli_args -> 
      let r = Rc2.find () |> Result.map (fun path -> path, cli_args)
      in r, settings
  end
  >>= fun ~settings (path, cli_args) -> (
    Rc2.read_from_file ~settings path 
    >>= Args.update_rc cli_args 
    >>= S.read handle_devices 
    >>= print_success
  )

let _ = 
  getmed 
    ~settings:(Rc2.std)
    ~cli_args:(Args.handle_all ())


