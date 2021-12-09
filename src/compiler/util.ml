
(** Keep options *)
let rec list_keep_some = function
  | [] -> []
  | None :: l -> list_keep_some l
  | Some x :: tl -> x :: list_keep_some tl

let map_opt ~f = function
  | None -> None
  | Some x -> Some (f x)
