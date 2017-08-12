
open Batteries
open BatExt

(*goto todo
  . Rewrite semantics and names
  . Rewrite to use new Result compatibility module + Rresult 
  . Use new semantics (+? named functions instead of infix) in getmed 
*)

let return ~settings v = (Ok v, settings)

let bind f (result, settings) = 
  match result with 
  | Ok v  -> f ~settings v
  | (Bad _) as bad -> (bad, settings)

(*goto this one could be dangerous, as we silently ignore a value*)
let run f (result, settings) =
  match result with
  | Ok _ -> f ~settings ()
  | (Bad _) as bad -> (bad, settings)

let read f v = f v; v

let bind_result f (result, settings) = 
  match result with 
  | Ok v  -> (f v, settings)
  | (Bad _) as bad -> (bad, settings)


(*
let bind_result f (result, settings) = (*goto weird name as value is mapped - this is bind_result*)
  match result with 
  | Ok v  -> (f v ~settings, settings)
  | (Bad _) as bad -> (bad, settings)
*)

(*note (read_ignore_val / >>! ) should just be functions taking unit argument instead*)

let inject v (result, settings) =
  match result with
  | Ok _ -> (v, settings)
  | (Bad _) as bad -> (bad, settings)

let map_result_bad f (result, settings) =
  match result with
  | (Ok _) as ok   -> (ok, settings)
  | (Bad e) -> (Bad (f e), settings)


module Settings = struct

  let lift f ~settings v = (f v, settings)

  let read f ~settings v = (f ~settings v, settings)

  let bind f ~settings v = (f v)

  let write = bind

end

module Infix = struct 

  let ( >> ) v f = run f v 
  let ( >>= ) v f = bind f v
  let ( >>! ) v f = run f v
  let ( >>? ) v f = read f v
  (*let ( >>+ ) v f = bind_settings f v*)
  let ( >>@ ) v f = map_result_bad f v (*goto find better symbol for this or use func*)

end




