open Batteries
open BatExt
open Exceptions
open Rc2

(*note: this module exists because of Rc2 circular dep. with Msg*)

let validate_field res_ref test error_msg =
  test || (
    Msg.term `Error "validate RC" ~colors:Rc2.Default.colors
      [ "Field 'name' should be a non-empty string." ];
    res_ref := BatResult.Bad RcValidationError;
    false
  )

let validate_settings s =
  let res_ref = ref @@ Ok () in
  let validate_device d =
    not d.active ||
    validate_field res_ref
      (d.name <> "") 
      [ "Field 'name' should be a non-empty string." ]
    &&
    validate_field res_ref
      (List.for_all 
         (function
           | `Uuid "" | `Label "" -> false
           | _ -> true
         )
         d.device_match
      ) 
      [ "Field 'device_match' should contain a non-empty string." ]
    &&
    validate_field res_ref
      (d.mount_path <> "") 
      [ "Field 'mount_path' should be a non-empty string." ]
  in
  if List.for_all validate_device s.devices then
    Ok ()
  else
    !res_ref

(*goto continue writing interface*)
let read_from_file ~settings file =
  let open Rresult in
  (try Ok (Yojson.Safe.from_file ~fname:"rc-file" file)
   with Yojson.Json_error s -> Error s)
  >>= config_of_yojson
  (*< test this for when we have extra fields *)
  |> function
  | Result.Ok settings' -> (
      Msg.term `Notif "update RC" ~colors:Rc2.Default.colors
        [ "Sucesfully parsed config-file." ];
      match validate_settings settings' with
      | BatResult.Ok () as r ->
        let s =
          { settings' with
            colors = settings'.colors |> Default.with_colors }
        in r, s
      | BatResult.Bad _ as r -> r, settings
    )
  | Result.Error e ->
    BatResult.Bad (RcParseError e), settings

