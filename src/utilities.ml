open BatUnix
open BatEnum
open BatString
open Printf

let actual_date =
  let t = BatUnix.localtime (BatUnix.time ()) in
  let (day, month, year) = (t.tm_mday, t.tm_mon, t.tm_year) in
  let str_day = string_of_int day in
  let str_mon = string_of_int (month+1) in
  let str_year = string_of_int (year+1900) in
  let ac_date = BatString.concat "/" [str_day; str_mon; str_year] in
  let ac_time = BatString.concat ":" [(string_of_int t.tm_hour);
                (string_of_int t.tm_min); (string_of_int t.tm_sec)] in
  ac_time,ac_date

let to_file ~text:te ~file:name =
  let t,d = actual_date in
  (*let title = BatString.concat "" [name; "-"; t; "-"; d] in*)
  let channel = open_out name in
  output_string channel te;
  close_out channel
;;
