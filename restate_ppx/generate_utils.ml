open Ppxlib
open Parsetree

let mkloc txt loc = { Location.txt; loc }
let mknoloc txt = mkloc txt Location.none
let annotation_name = "restate"

let attribute_by_name attributes name =
  let filtered =
    attributes |> List.filter (fun { attr_name = { Location.txt } } -> txt = name)
  in
  match filtered with
  | [] -> Ok None
  | [ attribute ] -> Ok (Some attribute)
  | _ -> Error ("Too many occurrences of \"" ^ name ^ "\" attribute")
;;

let is_annotated attributes =
  match attribute_by_name attributes annotation_name with
  | Ok (Some _) -> true
  | _ -> false
;;
