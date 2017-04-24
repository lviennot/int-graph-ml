(* Laurent Viennot, Inria 2017 *)

(** Compute the skeleton of a tree, see :
    Beyond Highway Dimension: Small Distance Labels Using Tree Skeletons
    Adrian Kosowski, Laurent Viennot
    https://arxiv.org/abs/1609.00512
*)

module Make (Traversal : sig
               type t
               type vertex = int
               type weight
               val dist : t -> vertex -> weight
               val visit_time : t -> vertex -> int
               val visit_at : t -> int -> vertex
               val visit_nb : t -> int
               val parent : t -> vertex -> vertex
               val parent_dist : t -> vertex -> weight
             end)
            (W : sig
               type t
               val to_float : t -> float
             end with type t = Traversal.weight) = struct

  module T = Traversal

  type t = {
    mutable n : int; (* Size of skeleton. *)
    dist : float array;
    furthest : int array;
    visit_at : int array; (* Can differ from traversal. *)
    visit_time : int array;
    nsons : int array;
  }

  let trav_metric u w v = W.to_float w
         
  let hop_metric u w v = 1.

  let no_vertex = -1
    
  let create n =
    { n = 0;
      dist = Array.make n max_float;
      furthest = Array.init n (fun i -> i);
      visit_at = Array.make n no_vertex;
      visit_time = Array.make n max_int;
      nsons = Array.make n 0;
    }

  let dist t u = t.dist.(u)
  let visit_at t i = t.visit_at.(i)
  let visit_time t u = t.visit_time.(u)
    
  let of_traversal t ?(alpha = 0.5) ?(edge_metric = trav_metric)
      ?(last_visit_nb = 0) trav = 
    let n_tree = T.visit_nb trav - last_visit_nb in

    (* Distance in the tree is computed from [edge_metric].  *)
    for i = last_visit_nb to n_tree - 1 do
      let u = T.visit_at trav i in
      let p = T.parent trav u in
      t.dist.(u) <-
        if i = last_visit_nb then 0.
        else t.dist.(p) +. edge_metric p (T.parent_dist trav u) u
    done;
    let dist_par u = t.dist.(u) -. t.dist.(T.parent trav u) in
    
    (* The reach of [u] is the distance from [u] to the furthest 
       descendant of [u]. *)
    let dist_furth u = t.dist.(t.furthest.(u)) -. t.dist.(u) in
    for i = n_tree - 1 downto last_visit_nb + 1 do
      let u = T.visit_at trav i in
      let p = T.parent trav u in
      if (dist_par u) +. (dist_furth u) > (dist_furth p) then
        t.furthest.(p) <- t.furthest.(u);
    done;

    (* The sekeleton is made of edges p-->u with sufficient reach. *)
    let long_reach u =
      (* Buggy since we modify t.dist :
         dist_par u +. dist_furth u > alpha *. t.dist.(T.parent trav u) *)
      t.dist.(t.furthest.(u)) > (1. +. alpha) *. t.dist.(T.parent trav u)
    in
    let visit_at = Array.make n_tree no_vertex and n_skel = ref 0 in
    let add u =
      visit_at.(!n_skel) <- u;
      incr n_skel;
    in
    let r = T.visit_at trav last_visit_nb in
    add r;
    for i = last_visit_nb + 1 to n_tree - 1 do
      let u = T.visit_at trav i in
      if long_reach u  then begin
        add u;
        let d_prune = (t.dist.(u) +. dist_furth u) /. (1. +. alpha) in
        t.dist.(u) <- min t.dist.(u) d_prune;
        let p = T.parent trav u in
        t.nsons.(p) <- t.nsons.(p) + 1;
      end
    done;
    t.n <- !n_skel;
    let visit_at = Array.sub visit_at 0 !n_skel in
    (* Sort according to corrected distances. *)
    Array.stable_sort (fun u v -> compare t.dist.(u) t.dist.(v)) visit_at;
    Array.blit visit_at 0 t.visit_at last_visit_nb !n_skel;
    for i = 0 to !n_skel - 1 do t.visit_time.(visit_at.(i)) <- i done;
    ()

  let n t = t.n

  let width ?(last_visit_nb = 0) t =
    let w = ref 1 (* root *) and wmax = ref 0 in
    for i = last_visit_nb to t.n - 1 do
      let u = t.visit_at.(i) in
      w := !w + t.nsons.(u) - 1;
      if !w > !wmax then wmax := !w;
    done;
    !wmax

  let integrated_width ?(last_visit_nb = 0) t trav =
    let sw = ref 0. in
    for i = last_visit_nb + 1 to t.n - 1 do
      let u = t.visit_at.(i) in
      let p = T.parent trav u in
      if t.dist.(u) = 0. then () (* root *)
      else if t.dist.(p) = 0. then  sw := !sw +. log t.dist.(u)
      else sw := !sw +. log t.dist.(u) -. log t.dist.(p)
    done;
    !sw
      
end
