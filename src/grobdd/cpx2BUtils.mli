open Cpx2BTypes

val arity_block :   block -> int
val arity_edge  : 'i edge -> int
val arity_node  : 'i tacx -> int
val neg_block   : block -> block
val cneg_block  : bool -> block -> block
val neg_edge    : 'i edge -> 'i edge
val cneg_edge   : bool -> 'i edge -> 'i edge
val ( => )      : bool -> bool -> bool
val make_nP     : int -> elem list
val make_nS     : int -> elem list
val isS	        : elem -> bool
val isntS       : elem -> bool
val isP	        : elem -> bool
val isntP       : elem -> bool
val isX	        : elem -> bool
val isntX       : elem -> bool
val shiftX      : int -> elem -> elem
val hasS        : block -> bool
val mod2        : int -> bool
val block_arity : block -> int
val edge_arity  : 'i edge -> int
val spx_is_const : elem list -> bool
val block_is_const : block -> bool option
val edge_is_const  : 'i edge -> bool option
val block_c0_to_block_spx : int -> block_spx
val block_id_to_block_spx : int -> int -> block_spx
val block_choice_to_block_spx : int -> block_choice -> block_spx
val push_S_block : block -> block
val push_S_edge : 'i edge -> 'i edge
val push_P_block_choice : block_choice -> block_choice
val push_P_block : block -> block
val push_P_edge : 'i edge -> 'i edge
val minicheck_spx : elem list -> bool
val get_spx_tag_from_spx : elem list -> spx_tag
val check_spx_tag : spx_tag -> elem list -> bool
val get_spx_tag : int -> block_choice -> spx_tag
val get_maxX_block : block -> int option
val in_block_spx : block_spx -> bool list -> bool option
val in_block_choice : block_choice -> bool list -> bool option
val in_block : block -> bool list -> bool option
val spx_is_contig : block_spx -> bool -> bool
val assert_block_spx : int -> block_spx -> bool -> unit
val check_block_spx : int -> block_spx -> bool -> bool
val assert_block_choice : int -> bool -> block_choice -> unit
val check_block_choice : int -> bool -> block_choice -> bool
val assert_block : block -> bool -> unit
val check_block : block -> bool -> bool
val check_edge : 'i edge -> bool
val reduce_block_spx : bool -> int -> bool -> bool -> elem list -> block
val reduce_block_choice : bool -> int -> bool -> block_choice -> block
val reduce_block : block -> bool -> block
val reduce_edge : 'i edge -> 'i edge
val spx_liste_to_block : bool -> bool -> elem list -> block
val spx_liste_to_edge : bool -> bool -> (elem list * 'i next) -> 'i edge
val push_X0_block_spx : bool -> bool -> bool -> int -> block_spx -> bool -> block
val push_X0_block_choice : bool -> bool -> bool -> int -> bool -> block_choice -> block
val push_X0_block : bool -> bool -> block -> bool -> block
val push_X0_edge : bool -> bool -> 'i edge -> 'i edge
val count_nS_spx : elem list -> int
val count_nS_block_choice : block_choice -> int
val count_nS_block : block -> int
val count_nS_edge : 'i edge -> int
val make_block_S : bool -> int -> block
val make_block_C0 : bool -> int -> block
val make_edge_C0 : bool -> int -> 'i edge
val make_block_P : bool -> int -> bool -> block
val cmake_nS : bool -> block -> block
val compose_block_spx_spx : block_spx -> block_spx -> block_spx
val compose_block : block -> block -> bool -> block
val compose_edge : block -> 'i edge -> 'i edge
val compose_merge : block -> 'i emerge -> 'i emerge
val compose_merge_node_edge : block -> ('i edge, 'i) Utils.merge -> 'i edge
val compose_merge3 : block -> 'i emerge3 -> 'i emerge3
val arity_merge : 'i emerge -> int
val arity_merge3 : 'i emerge3 -> int
val check_mergeC : 'i emerge -> bool
val check_mergeAX : 'i emerge -> bool
val check_merge3 : 'i emerge3 -> bool
val get_root : bool -> 'i edge -> 'i edge
val neg : 'i edge -> 'i edge
val cneg : bool -> 'i edge -> 'i edge
val peval_edge  : peval  -> 'i edge -> 'i edge * opeval
val opeval_edge : opeval -> 'i edge -> 'i edge * opeval
val check_peval : peval -> bool
val compose_peval : peval -> peval -> peval
val compose_opeval : opeval -> opeval -> opeval
val peval_pedge   : peval  -> 'i pedge -> 'i pedge
val opeval_pedge  : opeval -> 'i pedge -> 'i pedge
val peval_pnode   : peval  -> 'i pnode -> 'i pnode
val opeval_pnode  : opeval -> 'i pnode -> 'i pnode
val peval_pnodeC  : peval  -> 'i pnode -> ('i pedge, 'i pnode) Utils.merge
val select : bool -> 'i node -> 'i edge
