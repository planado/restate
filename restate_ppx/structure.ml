let map_structure_item mapper structure_item =
  let structure_item = Generate_t.generate structure_item in
  [ mapper#structure_item structure_item ]
  @ Generate_t_make.generate structure_item
  @ Generate_t_initial.generate structure_item
;;
