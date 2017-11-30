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
open Media_types

(*
(*goto put into rc as theme*)
let c i = LTerm_style.index i 
let c1 (*anglebrackets*) = c 1 
let c2 (*special text*) = c 2 
let c3 (*numbers*) = c 3 
*)

let get_c = Rc2.color_to_lterm

let term ~colors typ place_of_call ss =
  let open LTerm_text in
  let wrap_in_color color s_text =
    B_fg color :: s_text @ [E_fg] in
  let head_str = "getmed:" ^ place_of_call ^ ":" in
  let msg_markup = LTerm_text.(List.flatten [
      (match typ with
       | `Error -> wrap_in_color (get_c `TextWarning colors) [ S head_str ]
       | `Major -> wrap_in_color (get_c `TextSpecial colors) [ S head_str ]
       | `Notif -> [ S head_str ]
      );
      (match typ with 
       | `Error -> [ S "error:" ]
       | `Major | `Notif -> []
      );
      [ S (ss |> String.concat "") ]
    ])
  in
  LTerm.printls @@ LTerm_text.eval msg_markup
  |> Lwt_main.run

let term_file_copy ~colors file =
  begin
    let file_markup = LTerm_text.([
        S "Copying '";
        B_fg (get_c `TextSpecial colors);
        S file.path;
        E_fg;
        S "'"
      ]) |> LTerm_text.eval in
    let open Lwt in
    Lazy.force LTerm.stdout >>= fun stdout ->
    LTerm.clear_line_next stdout >>= fun () ->
    LTerm.fprintls stdout @@ file_markup >>= fun () ->
    LTerm.flush stdout
  end
  |> Lwt_main.run

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

let human_readable_time ~pr_second p =
  let second = pr_second in
  let minute = second * 60 in
  let hour = minute * 60 in
  let day = hour * 24 in
  let year = day * 365 in
  let s = Printf.sprintf in
  let plur s n = if n > 1 then s ^ "s" else s 
  in
  if p >= year then
    let yrs = p/year 
    and days = (p mod year) / day in
    [ `D (s "%d " yrs);
      `S (s "%s, " (plur "year" yrs));
      `D (s "%d " days);
      `S (s "%s" (plur "day" days)) ]
  else if p >= day then
    let days = p/day 
    and hours = (p mod day) / hour 
    and minutes = (p mod hour) / minute in
    [ `D (s "%d " days);
      `S (s "%s, " (plur "day" days));
      `D (s "%d " hours);
      `S (s "%s, " (plur "hour" hours));
      `D (s "%d " minutes);
      `S (s "%s" (plur "minute" minutes)); ]
  else if p >= hour then
    let hours = (p mod day) / hour 
    and minutes = (p mod hour) / minute
    and seconds = (p mod minute) / second in
    [ `D (s "%d " hours);
      `S (s "%s, " (plur "hour" hours));
      `D (s "%d " minutes);
      `S (s "%s, " (plur "minute" minutes));
      `D (s "%d " seconds);
      `S (s "%s" (plur "second" seconds)); ]
  else if p >= minute then
    let minutes = (p mod hour) / minute
    and seconds = (p mod minute) / second in
    [ `D (s "%d " minutes);
      `S (s "%s, " (plur "minute" minutes));
      `D (s "%d " seconds);
      `S (s "%s" (plur "second" seconds)); ]
  else
    let seconds = float p /. float second in
    [ `D (s "%1.1f " seconds);
      `S (s "%s" (plur "second" (p/second))); ]

let style_digit_or_string d_color e = LTerm_text.(
    match e with 
      `D d -> [B_fg d_color; S d; E_fg]
    | `S s -> [(*B_fg s_color;*) S s; (* E_fg *)] 
)

let style_digits_or_strings d_color l =
  CCList.flat_map (style_digit_or_string d_color) l


let progress
    ~colors
    ~start_time
    ~full_transfer_size
    ~prev_transf
    ~file
    fo_transf =
  let open Media_types in
  let f = Float.of_int 
  and s = Printf.sprintf
  in
  let c_sym = (get_c `Symbol colors)
  and c_num = (get_c `Number colors)
  and c_tsp = (get_c `TextSpecial colors)
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
  let markup_box s = LTerm_text.(List.flatten [
      [ B_bold true; B_fg c_sym; S "["; E_fg; E_bold; ];
      s;
      [ B_bold true; B_fg c_sym; S "]"; E_fg; E_bold; ]
    ]) 
  and markup_num2 (n,u) (n', u') = LTerm_text.([
      B_fg c_num; S n; E_fg;
      (*B_fg c_tsp;*) S u; S "/"; (*E_fg; *)
      B_fg c_num; S n'; E_fg;
      (* B_fg c_tsp; *) S u'; (* E_fg; *)
    ])
  and markup_num (n,u) = LTerm_text.([
      B_fg c_num; S n; E_fg; (*B_fg c_tsp;*) S u; (*E_fg; *)
    ]) in
  let progress_markup = LTerm_text.(List.flatten [
      markup_box [S (s "%-15s" progress_bar)];
      markup_box (
        markup_num2
          (human_readable_bytes' transferred)
          (human_readable_bytes' full_transfer_size)
      );
      markup_box (markup_num transfer_speed);
      markup_box @@ List.flatten [
        [ S (s "ETR:") ];
        Int.of_float time_left
        |> human_readable_time ~pr_second:1
        |> style_digits_or_strings c_num (*goo*)
      ];
      [ S "\r" ];
    ])
  in
  begin
    let open Lwt in
    (*LTerm.print @@ String.make 80 ' ' ^ "\r" >>= fun () ->*)
    Lazy.force LTerm.stdout >>= fun stdout ->
    LTerm_text.eval progress_markup
    |> LTerm.fprints stdout >>= fun () -> 
    LTerm.flush stdout
  end
  |> Lwt_main.run



