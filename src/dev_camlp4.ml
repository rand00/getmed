let get_dir_if_mounted dev dir =
  Ok (Unix.command_getlines "mount")
  >>= (fun mount_lines -> 
      try 
        Enum.find (Pcre.pmatch ~pat:("^" ^ dev)) mount_lines
        |> function 
        | <:re< 
                [^" "]* 
                 " on " 
                ( [^" "]{1+} as dir_existing ) >> -> 
          ( Msg.term `Notif "mount"
              [ "Device '"; dev; "' already mounted at '";
                dir_existing; "'. Will use existing ";
                "mountpoint." ];
            Ok (`Dont_mount dir_existing) )
        | _ -> Ok (`Mount dir) (*will not happen*)

      with Not_found -> 
        Ok (`Mount dir)
    )
