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

let termwrap 
    ?(initial_nonwrap=0) 
    (*options for textwrap >>*)
    ?(initial_indent="  ") 
    ?(subsequent_indent="  ") 
    ?(drop_whitespace=true)
    ?(replace_whitespace=true)
    ss = 
  String.concat "" ss
  |> (fun str -> 
      let wrap_len = Unix.term_ncolumns () in
      let (init_line, rest_str) = 
        (match initial_nonwrap with
         | 0 -> [], str
         | n -> 
           let str_len = String.length str in
           let init_maxlen = (min (wrap_len - n) str_len) in
           let init_len = 
             (if str_len > init_maxlen then
                String.rfind_from str (init_maxlen -1) " " 
              else 
                str_len )
           in ( [ String.sub str 0 init_len ], String.lchop ~n:init_len str ))
      in
      let open Wrapper in
      String.concat "\n"
        (List.flatten
           [ init_line;
             rest_str |> wrap 
               (make 
                  ~initial_indent 
                  ~subsequent_indent
                  ~drop_whitespace
                  ~replace_whitespace
                  wrap_len)
           ]))


let term typ title ss = 
  String.concat "" 
    [ "getmed:";
      (match typ with 
       | `Error -> "ERROR:"
       | `Notif -> "");
      title ^ ":\n";
      (termwrap ss) ]
  |> print_endline


let human_readable_bytes' bytes =
  let r p = float @@ Int.pow 10 p in
  let kb = r 3
  and mb = r 6
  and gb = r 9
  and tb = r 12
  and bytes = float bytes
  in
  let amount, category =
    if bytes > tb then bytes /. tb, "TB"
    else if bytes > gb then bytes /. gb, "GB"
    else if bytes > mb then bytes /. mb, "MB"
    else if bytes > kb then bytes /. kb, "KB"
    else bytes, "B"
  in ((Printf.sprintf "%.0f" amount), category)

let human_readable_bytes bytes =
  let amount, unit = human_readable_bytes' bytes in
  amount ^ unit
    
(** Progress printing*)

(*goto exchange prev_len with size of terminal
  - for rewriting to new progress function + new cp
  or just print init len term
*)
let progress_old ~full_size ~trans_size ~prev_len file = 
  let open Media_types in
  let f = Float.of_int in
  let progress_len =
    (f trans_size /. f full_size) *. 15. 
    |> Int.of_float in
  let progress_bar = String.make progress_len '|' in
  let final_string =
    Printf.sprintf "> [%-15s] [%4s / %4s] Transferring '%s'  \r" 
      progress_bar
      (human_readable_bytes (trans_size+file.size))
      (human_readable_bytes full_size)
      file.path in
  begin
    print_string @@ String.make prev_len ' ' ^ "\r";
    print_string final_string;
    flush stdout;
  end;
  String.length final_string

let human_readable_time ~pr_second p =
  let second = pr_second in
  let minute = second * 60 in
  let hour = minute * 60 in
  let day = hour * 24 in
  let year = day * 365 in
  let s = Printf.sprintf
  in
  if p >= year then
    s "%d year(s), %d day(s)"
      (p/year) ((p mod year) / day)
  else if p >= day then
    s "%d day(s), %d hour(s), %d minute(s)"
      (p/day) ((p mod day) / hour) ((p mod hour) / minute)
  else if p >= hour then
    s "%d hour(s), %d minute(s), %d second(s)"
      (p/hour) ((p mod hour) / minute) ((p mod minute) / second)
  else if p >= minute then
    s "%d minute(s), %d second(s)"
      (p/minute) ((p mod minute) / second)
  else 
    s "%1.1f second(s)" (float p /. float second)

(* goto use 
   . LTerm.clear_line
   . LTerm.print[l]s (print styled on stdout)
   . LTerm_text.eval [...]
*)

(*goto make like old progress output *)
let progress
    ~start_time
    ~full_transfer_size
    ~prev_transf
    ~file
    fo_transf =
  let open Media_types in
  let f = Float.of_int 
  and s = Printf.sprintf
  in
  let transferred = prev_transf + fo_transf in
  let pct_transferred =
    f transferred *. 100. /. f full_transfer_size in
  let time_spent = Unix.gettimeofday () -. start_time in
  let time_overall = (100. /. pct_transferred) *. time_spent in
  let time_left = ((100. -. pct_transferred) /. 100.) *. time_overall in
  let transfer_speed =
    let s_num, s_unit =
      f transferred /. time_spent
      |> Int.of_float
      |> human_readable_bytes'
    in
    (s_num, (s_unit^"/Second"))
  in
  let progress_bar =
    let len =
      (f transferred /. f full_transfer_size) *. 15. 
      |> Int.of_float in
    String.make len '|' in
  let c i = LTerm_style.index i in
  let c1 (*anglebrackets*) = c 1 in (*goto supply as theme - set in config*)
  let c2 (*special text*) = c 2 in
  let c3 (*numbers*) = c 3 in
  let markup_box s = LTerm_text.(List.flatten [
      [ B_bold true; B_fg c1; S "["; E_fg; E_bold; ];
      s;
      [ B_bold true; B_fg c1; S "]"; E_fg; E_bold; ]
    ]) 
  and markup_num2 (n,u) (n', u') = LTerm_text.([
    B_fg c3; S n; E_fg; B_fg c2; S u; S "/"; E_fg; 
    B_fg c3; S n'; E_fg; B_fg c2; S u'; E_fg;
  ])
  and markup_num (n,u) = LTerm_text.([
    B_fg c3; S n; E_fg; B_fg c2; S u; E_fg; 
  ]) in
  let progress_markup = LTerm_text.(List.flatten [
      markup_box [S (s "%-15s" progress_bar)];
      markup_box (
        markup_num2
          (human_readable_bytes' transferred)
          (human_readable_bytes' full_transfer_size)
      );
      markup_box (markup_num transfer_speed);
      markup_box [
        S (s "ETR:%s"
             (Int.of_float time_left
              |> human_readable_time ~pr_second:1 ))
      ];
    ])
  in
  begin
    let open Lwt in
    (*LTerm.print @@ String.make 80 ' ' ^ "\r" >>= fun () ->*)
    Lazy.force LTerm.stdout >>= 
    LTerm.clear_line_prev >>= fun () ->
    LTerm.prints @@ LTerm_text.eval progress_markup
  end
  |> Lwt_main.run
(*<goto do we want other stuff to be in the lwt monad?*)



