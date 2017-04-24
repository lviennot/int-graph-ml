(* Laurent Viennot, Inria 2017 *)

(** Dijkstra traversal of a graph. *)

module Make
  (Graph : sig
    type t
    type vertex = int
    type weight
    val n : t -> int
    val iter_wgt_succ : (weight -> vertex -> unit) -> t -> vertex -> unit
    val no_vertex : vertex
  end)
  (WgtArr : GenArray.Type with type elt = Graph.weight)
  (W : sig
    type t
    val zero : t
    val infinity : t
    val add : t -> t -> t
    val compare : t -> t -> int
  end with type t = Graph.weight)
  =
struct

  module G = Graph

  type vertex = int
  type weight = G.weight
      
  module Heap = Heap.Make (struct
    type t = G.vertex * G.weight * int (* hops *)
    let compare (u, du, hu) (v, dv, hv) =
      let c = W.compare du dv in
      if c <> 0 then c else
        let c = hu - hv in
        if c <> 0 then c else u - v
  end)

  module Queue = GenArray.QueueOf (struct type t = G.vertex end)
    
  type t = {
    n : int;
    dist : WgtArr.t;
    hop_dist : int array;
    mutable visit_nb : int;
    visit_at : int array;
    visit_time : int array;
    parent : int array;
    parent_dist : WgtArr.t;
    queue : Heap.t;
  }

  let infinity = max_int
      
  let create n = {
    n = n;
    dist = WgtArr.make n W.infinity;
    hop_dist = Array.make n infinity;
    visit_nb = 0;
    visit_at = Array.make n G.no_vertex;
    visit_time = Array.make n infinity;
    parent = Array.init n (fun i -> i);
    parent_dist = WgtArr.make n W.zero;
    queue = Heap.create n;
  }

  let dist t u = WgtArr.get t.dist u
  let visit_time t u = t.visit_time.(u)
  let visit_at t i = t.visit_at.(i)
  let visit_nb t = t.visit_nb
  let parent t u = t.parent.(u)
  let parent_dist t u = WgtArr.get t.parent_dist u

  let clear ?(last_visit_nb=0) t =
    Heap.clear t.queue; 
    for i=last_visit_nb to t.visit_nb - 1 do
      let u = t.visit_at.(i) in
      WgtArr.set t.dist u W.infinity;
      t.hop_dist.(u) <- infinity;
      t.visit_at.(i) <- G.no_vertex;
      t.visit_time.(u) <- infinity;
    done;
    t.visit_nb <- last_visit_nb

      
  let dijkstra t g sources =
    assert (t.n >= G.n g);

    let nq = ref 0 in
    let add par dpar v dv hv =
      incr nq;
      Heap.add t.queue (v, dv, hv);
      WgtArr.set t.dist v dv;
      t.hop_dist.(v) <- hv;
      t.parent.(v) <- par;
      WgtArr.set t.parent_dist v dpar;
    in

    List.iter (fun s -> add s W.zero s W.zero 0) sources;
    
    while not (Heap.is_empty t.queue) do
      let u, du, hu = Heap.pop_min t.queue in
      if du = WgtArr.get t.dist u && hu = t.hop_dist.(u) then begin
        assert (t.visit_time.(u) = infinity);
        t.visit_at.(t.visit_nb) <- u;
        t.visit_time.(u) <- t.visit_nb;
        t.visit_nb <- t.visit_nb + 1;
        
        G.iter_wgt_succ (fun w v ->
          assert (W.compare w W.zero >= 0);
          let dv = W.add du w and hv = hu + 1 in
          assert (W.compare dv W.infinity < 0);
          let closer = W.compare dv (WgtArr.get t.dist v) in
          if closer < 0 || (closer = 0 && hv < t.hop_dist.(v)) then
            add u w v dv hv
        ) g u;
      end
    done;

    !nq

    
end
  
