open Ppxlib
open Parsetree
open Ast_helper
open Generate_utils

let map_ptyp_desc ptyp_desc =
  Ptyp_constr (mknoloc (Ldot (Lident "ReState", "t")), [ Typ.mk ptyp_desc ])
;;

let map_pld_type ({ ptyp_desc } as pld_type) =
  { pld_type with ptyp_desc = map_ptyp_desc ptyp_desc }
;;

let map_label_decl ({ pld_type } as label_decl) =
  { label_decl with pld_type = map_pld_type pld_type }
;;

let map_type_decl type_decl =
  let { ptype_manifest; ptype_kind } = type_decl in
  (* https://github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli#L490 *)
  match ptype_manifest, ptype_kind with
  | None, Ptype_record label_decls ->
    { type_decl with ptype_kind = Ptype_record (List.map map_label_decl label_decls) }
  | _ -> type_decl
;;

let generate ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (rec_flag, type_decls) ->
    let type_decls =
      type_decls
      |> List.map (fun ({ ptype_attributes } as type_decl) ->
           if is_annotated ptype_attributes then map_type_decl type_decl else type_decl)
    in
    Str.type_ rec_flag type_decls
  | _ -> structure_item
;;
