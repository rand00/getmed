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

let handle_errors (result, settings) =
  let s = settings in 
  let msg = Msg.term ~colors:s.colors in 
  match result with 
  | Ok v ->
    begin
      msg `Major "handler" 
        [ "Ran succesfully for device '"; s.device.name;"'." ];
      Ok (Some v)
    end
  | Bad (BeforeMounting DeviceNotPresent) ->
    begin
      msg `Notif "handler"
        [ "Trying next device instead." ];
      Ok None
    end
  | Bad (BeforeMounting exn) ->
    begin
      msg `Error "handler" [
        "Failed on device '"; s.device.name; "' with the ";
        "error: \n\t";
        Printexc.to_string exn;
      ];
      exit 1
    end
  | Bad MediaNotPresent ->
    begin
      msg `Notif "handler" [
        "No media was present on device '"; s.device.name; "'."
      ];
      Dev.unmount ~settings:s () |> ignore;
      Ok None
    end
  | Bad exn_after_mounting ->
    begin
      msg `Error "handler" [
        "Failed on device '"; s.device.name; "' with the ";
        "error: \n\t";
        Printexc.to_string exn_after_mounting;
      ];
      Dev.unmount ~settings:s () |> ignore;
      exit 1
    end

let bind_result f v = Result.Monad.bind v f 

(*>goto 
  . make bind automatically catch exceptions?
    . think
*)
let handle_devices ~(settings:Rc2.config) () =
  let gs = settings in
  let rec loop devices dirs_transferred_to_earlier =
    match devices with
    | dev :: tl when dev.active -> begin
      Msg.term ~colors:gs.colors `Major "handler" [
        "Starting transfer for device '";
        dev.name;
        "'."
      ];
      let settings = Rc2.device_wrap gs dev in
      begin
        Dev.find ~settings () 
        >>= Dev.mount_smartly
        >>@ Exceptions.wrap_renew (fun e -> BeforeMounting e)
        >>= Media.search 
        >>? Rc2.print_config_debug

        >>= fun ~settings media -> 
        StateResult.return () ~settings
        >> S.read @@ Media.dirs_fix
        >>= fun ~settings dirs_to_transfer_to -> 
        StateResult.return () ~settings
        >> S.read @@ Media.transfer media 
        >> S.read @@ Media.cleanup media
        >> S.read @@ Dev.unmount
        >|= fun () -> dirs_to_transfer_to
      end
      |> handle_errors 
      |> bind_result @@ function
      | None -> loop tl dirs_transferred_to_earlier
      | Some dirs_transferred_to_now ->
        let dirs_transferred_to = begin match dirs_transferred_to_earlier with
          | Some l -> l @ dirs_transferred_to_now
          | None -> dirs_transferred_to_now
        end
        in
        loop tl (Some dirs_transferred_to)
    end
    | _ :: tl -> loop tl dirs_transferred_to_earlier
    | _ -> Ok dirs_transferred_to_earlier
  in
  loop settings.devices None

let getmed (settings:Rc2.config) =
  S.read handle_devices ~settings ()








