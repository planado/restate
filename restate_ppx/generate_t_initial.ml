open Ppxlib
open Parsetree
open Ast_helper
open Generate_utils

let get_pat_name type_name = type_name ^ "_default"

let generate_pat type_name =
  Pat.constraint_
    (Pat.var (mknoloc (get_pat_name type_name)))
    (Typ.poly [] (Typ.constr (mknoloc (Lident type_name)) []))
;;

let expr type_name =
  let expr =
    Exp.apply
      (Exp.ident (mknoloc (Ldot (Lident "Obj", "magic"))))
      [ Asttypes.Nolabel, Exp.construct (mknoloc (Lident "()")) None ]
  in
  let typ = Typ.constr (mknoloc (Lident type_name)) [] in
  Exp.constraint_ expr typ
;;

let map_type_decl { ptype_name = { txt = type_name } } =
  let pat = generate_pat type_name in
  let expr = expr type_name in
  [ Vb.mk pat expr ]
;;

let generate { pstr_desc } =
  match pstr_desc with
  | Pstr_type (_rec_flag, type_decls) ->
    let values =
      type_decls
      |> List.map (fun ({ ptype_attributes } as type_decl) ->
           if is_annotated ptype_attributes then map_type_decl type_decl else [])
      |> List.concat
    in
    (match values with
     | [] -> []
     | _ -> [ Str.value Nonrecursive values ])
  | _ -> []
;;
