(* Laurent Viennot, Inria 2017 *)

(** Dijkstra traversal of a graph. *)

module Make
  (Graph : sig
    type t
    type vertex = int
    type weight
    val n : t -> int
    val iter_wgt_succ : (weight -> vertex -> unit) -> t -> vertex -> unit
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
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
  module Stack = GenArray.StackOf (struct type t = G.vertex end)
    
  type t = {
    n : int;
    dist : WgtArr.t;
    hop_dist : int array;
    mutable visit_nb : int;
    visit_at : int array;
    visit_time : int array;
    parent : int array;
    parent_dist : WgtArr.t;
    heap : Heap.t;
    queue : Queue.t;
    visit_end : int array; (* for dfs *)
    stack : Stack.t;
  }

  let infinity = max_int
      
  let create n = {
    n = n;
    dist = WgtArr.make n W.infinity;
    hop_dist = Array.make n infinity;
    visit_nb = 0;
    visit_at = Array.make n G.no_vertex;
    visit_time = Array.make n infinity;
    parent = Array.make n G.no_vertex;
    parent_dist = WgtArr.make n W.infinity;
    heap = Heap.create 2;
    queue = Queue.create 2;
    visit_end = Array.make n infinity;
    stack = Stack.create 2;
  }

  let dist t u = WgtArr.get t.dist u
  let visit_time t u = t.visit_time.(u)
  let visit_at t i = t.visit_at.(i)
  let visit_nb t = t.visit_nb
  let parent t u = t.parent.(u)
  let parent_dist t u = WgtArr.get t.parent_dist u
  let visit_end t u = t.visit_end.(u)

  (** Returns path from root to [u] as a list of node indexes. *)
  let path t u =
    let rec path acc u =
      let p = parent t u in
      if p = u then (u :: acc) (* root *)
      else path (u :: acc) p
    in path [] u

  (** Return edges of the path from root to [u]. *)
  let edge_path t u =
    let rec path acc p u =
      if p = u then acc (* root *)
      else path ((p, parent_dist t u, u) :: acc) (parent t p) p
    in path [] (parent t u) u

  let clear ?(last_visit_nb=0) t =
    for i=last_visit_nb to t.visit_nb - 1 do
      let u = t.visit_at.(i) in
      WgtArr.set t.dist u W.infinity;
      t.hop_dist.(u) <- infinity;
      t.visit_at.(i) <- G.no_vertex;
      t.visit_time.(u) <- infinity;
      t.parent.(u) <- G.no_vertex;
      WgtArr.set t.parent_dist u W.infinity;
      t.visit_end.(u) <- infinity;
    done;
    t.visit_nb <- last_visit_nb


  (** Dijkstra traversal, returns the number of visited nodes. *)
  let dijkstra t g ?(filter = fun _ _ _ _ _ _ -> true) sources =
    assert (t.n >= G.n g);

    let last_visit_nb = t.visit_nb in
    let add par dpar v dv hv =
      Heap.add t.heap (v, dv, hv);
      WgtArr.set t.dist v dv;
      t.hop_dist.(v) <- hv;
      t.parent.(v) <- par;
      WgtArr.set t.parent_dist v dpar;
    in

    List.iter (fun s -> add s W.zero s W.zero 0) sources;
    
    while not (Heap.is_empty t.heap) do
      let u, du, hu = Heap.pop_min t.heap in
      if du = WgtArr.get t.dist u && hu = t.hop_dist.(u) then begin
        assert (t.visit_time.(u) = infinity);
        t.visit_at.(t.visit_nb) <- u;
        t.visit_time.(u) <- t.visit_nb;
        t.visit_nb <- t.visit_nb + 1;
        
        G.iter_wgt_succ (fun w v ->
          assert (W.compare w W.zero >= 0);
          let dv = W.add du w and hv = hu + 1 in
          assert (W.compare dv W.infinity < 0 && W.compare dv W.zero >= 0);
          let closer = W.compare dv (WgtArr.get t.dist v) in
          if (closer < 0 || (closer = 0 && hv < t.hop_dist.(v)))
            && filter u du hu v dv hv
          then
            add u w v dv hv
        ) g u;
      end
    done;

    (t.visit_nb - last_visit_nb)

      
  (** BFS traversal, returns the number of visited nodes. *)
  let bfs t g sources =
    assert (t.n >= G.n g);

    let last_visit_nb = t.visit_nb in
    let add par dpar v dv hv =
      Queue.add t.queue v;
      WgtArr.set t.dist v dv;
      t.hop_dist.(v) <- hv;
      t.parent.(v) <- par;
      WgtArr.set t.parent_dist v dpar;
    in

    List.iter (fun s -> add s W.zero s W.zero 0) sources;
    
    while not (Queue.is_empty t.queue) do
      let u = Queue.pop t.queue in
      let du = WgtArr.get t.dist u and hu = t.hop_dist.(u) in
      assert (t.visit_time.(u) = infinity);
      t.visit_at.(t.visit_nb) <- u;
      t.visit_time.(u) <- t.visit_nb;
      t.visit_nb <- t.visit_nb + 1;
        
      G.iter_wgt_succ (fun w v ->
        if t.parent.(v) = G.no_vertex then begin (* not in queue nor visited *)
          let dv = W.add du w and hv = hu + 1 in
          add u w v dv hv
        end
      ) g u;
    done;
    
    (t.visit_nb - last_visit_nb)


  (** Assumes symmetric graph. *)
  let connected_components t g =
    clear t;
    let n = G.n g in
    let ccs = ref [] in
    for u = 0 to n - 1 do
      if t.visit_time.(u) = infinity then begin (* not yet visited *)
          let nvis = bfs t g [u] in
          let cc = ref [] in
          for i = t.visit_nb - 1 downto t.visit_nb - nvis do
            cc := t.visit_at.(i) :: !cc
          done;
          ccs := !cc :: !ccs;
      end
    done;
    !ccs
    
      
  exception Cycle of G.vertex * G.vertex

  (* Returns a path from [v] to [u] when [uv] is a back edge. *)
  let cycle t u v =
    let rec iter acc u' =
      let acc = u' :: acc in
      if u' = v then acc else begin
          assert (u' <> t.parent.(u')); (* no cycle otherwise ! *)
          iter acc t.parent.(u')
        end
    in
    iter [] u
                   
  (** DFS traversal, returns the number of visited nodes. *)
  let dfs ?(check_dag=false) t g s =
    assert (t.n >= G.n g);

    let last_visit_nb = t.visit_nb and vis_end = ref t.visit_nb in
    let add par v =
      Stack.add t.stack v;
      t.parent.(v) <- par;
    in

    add s s;
    
    while not (Stack.is_empty t.stack) do
      let u = Stack.pop t.stack in
      if t.visit_time.(u) = infinity then begin (* not yet visited *)
          t.visit_at.(t.visit_nb) <- u;
          t.visit_time.(u) <- t.visit_nb;
          t.visit_nb <- t.visit_nb + 1;
          
          add t.parent.(u) u; (* for ending of visit *)
          G.iter_succ (fun v ->
              if check_dag (* cycle if v is being visited : *)
                 && t.visit_time.(v) <> infinity && t.visit_end.(v) = infinity
              then raise (Cycle (u, v));
              if t.visit_time.(v) = infinity then (* not visited *)
                add u v
            ) g u;
        end else if t.visit_end.(u) = infinity then begin (* end visit *)
          t.visit_end.(u) <- !vis_end;
          incr vis_end;
        end
    done;
    
    (t.visit_nb - last_visit_nb)

  (** Returns a topological ordering [ord] of a dag [g], i.e. an ordering
      such that for [uv] in [E(g)], [u] appears before [v] in [ord]. *)
  let topological_ordering t g =
    clear t;
    let n = G.n g in
    for u = 0 to n - 1 do
      if t.visit_time.(u) = infinity then begin (* not yet visited *)
        try
          ignore (dfs ~check_dag:true t g u);
        with Cycle (u, v) ->
          Printf.eprintf "Cycle: ";
          List.iter (fun u ->
              Printf.eprintf "%d %d %d\n" u t.visit_time.(u) t.visit_end.(u)
            ) (u :: cycle t u v);
          Printf.eprintf "%d\n" v;
          (* failwith *) Printf.eprintf "Graph is not a dag!\n";
          raise (Cycle (u, v))
      end
    done;
    let ord = Array.make n (-1) in
    for u = 0 to n - 1 do ord.(n - 1 - t.visit_end.(u)) <- u done;
    ord


  (** Returns the height of each node in the partial order resulting from
  transitive closure of [g]. *)
  let dag_height t g =
    let n = G.n g in
    let h = Array.make n 0 in
    let ord = topological_ordering t g in
    for i = 0 to n - 1 do
      let u = ord.(i) in
      let hu1 = h.(u) + 1 in
      G.iter_succ (fun v ->
          h.(v) <- max h.(v) hu1
        ) g u;
    done;
    h
    
end
  

module IntWeight = struct
  type t = int
  let zero = 0
  let infinity = max_int
  let add = (+)
  let compare w w' = w - w'
end
