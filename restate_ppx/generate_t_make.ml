open Ppxlib
open Parsetree
open Ast_helper
open Generate_utils

let get_fn_name type_name = type_name ^ "_make"

let get_fn_params label_decls =
  label_decls
  |> List.map (fun { pld_name; pld_type } -> Pat.constraint_ (Pat.var pld_name) pld_type)
;;

let get_fn_body type_name params =
  let record_fields =
    params
    |> List.filter_map (fun { ppat_desc } ->
         match ppat_desc with
         | Ppat_constraint ({ ppat_desc = Ppat_var { txt = pat_name } }, _) ->
           let longident = mknoloc (Lident pat_name) in
           let expr = Exp.ident longident in
           Some (longident, expr)
         | _ -> None)
  in
  let expr = Exp.record record_fields None in
  let typ = Typ.constr (mknoloc (Lident type_name)) [] in
  Exp.constraint_ expr typ
;;

let generate_make name params body_expr =
  let pat = Pat.var (mknoloc name) in
  let expr =
    List.fold_right
      (fun param expr -> Exp.fun_ Asttypes.Nolabel None param expr)
      params
      body_expr
  in
  Vb.mk pat expr
;;

let map_type_decl decl =
  let { ptype_name = { txt = type_name }; ptype_manifest; ptype_kind } = decl in
  (* https://github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L490 *)
  match ptype_manifest, ptype_kind with
  | None, Ptype_record label_decls ->
    let name = get_fn_name type_name in
    let params = get_fn_params label_decls in
    let body = get_fn_body type_name params in
    [ generate_make name params body ]
  | _ -> []
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
