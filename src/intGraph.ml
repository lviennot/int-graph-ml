
(** Basic graph structure where vertices are integers from [0] to [n-1]. 
    First use module LabelSet to create a graph with labeled vertices. *)
module Make
  (IntArr : GenArray.ExpandType with type elt = int)
  (LongIntArray : GenArray.ExpandType with type elt = int)
  (WgtArr : GenArray.ExpandType) :
sig
  type t
  type vertex = int
  type weight
  val of_edges : ?n_estim:int ->
    ((vertex -> weight -> vertex -> unit) -> unit) -> t
  val reverse : t -> t
  val n : t -> int
  val m : t -> int
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_wgt_succ : (weight -> vertex -> unit) -> t -> vertex -> unit
  val iter : (vertex -> weight -> vertex -> unit) -> t -> unit
  val no_vertex : vertex

  val sort : t -> t
  val sorted_of_edges : ?n_estim:int ->
    ((vertex -> weight -> vertex -> unit) -> unit) -> t
  val mem_edge : t -> vertex -> vertex -> bool

  type edge = int
  val iter_out_edges : (edge -> unit) -> t -> vertex -> unit
  val edge_src : t -> edge -> vertex
  val edge_dst : t -> edge -> vertex
  val edge_wgt : t -> edge -> weight
end with type weight = WgtArr.elt = struct

  module I = IntArr (* stores vertex numbers *)
  module L = LongIntArray (* stores edge numbers *)
  module W = WgtArr (* stores edge weights *)

  type vertex = int
  type weight = W.elt
    
  type t = {
    n : int;
    m : int;
    sdeg : L.t; (* Prefix sum of degrees. *)
    dst : I.t; (* Adjacencies in a flat array. *)
    wgt: W.t; (* Corresponding weights. *)
    sorted: bool; (* Adjacency lists are sorted. *)
  }

  let n g = g.n
  let m g = g.m

  let no_vertex = -1

  let vtx_cmp u v = u - v

  let of_edges ?(n_estim=16) iter =

    (* Degrees : *)
    let n = ref 0 in
    let deg = ref (L.create n_estim) and dft_wgt = ref None in
    iter (fun u w v ->
      if !dft_wgt = None then dft_wgt := Some w ;
      if u >= !n then n := u + 1 ;
      if v >= !n then n := v + 1 ;
      if !n > L.length !deg then deg := L.expand !deg !n 0 ;
      L.set !deg u (1 + L.get !deg u) ;
    ) ;
    let n = !n and deg = !deg and dft_wgt = !dft_wgt in

    (* Prefix sums of degrees : *)
    let sdeg = L.make (n+1) 0 and m = ref 0 in
    for u=0 to n-1 do
      L.set sdeg u !m ;
      m := !m + (L.get deg u) ;
    done ;
    let m = !m in
    L.set sdeg n m ;

    (* Adjacencies : *)
    let dst = I.make m no_vertex in
    let wgt =
      match dft_wgt with None -> W.create n_estim | Some w -> W.make m w in
    let sorted = ref true in
    for u=0 to n-1 do L.set deg u 0 done ; (* reuse *)
    iter (fun u w v ->
      let du = L.get deg u in (* deg for edges inserted so far *)
      let e = L.get sdeg u + du in
      I.set dst e v ;
      W.set wgt e w ;
      L.set deg u (du+1) ;
      if !sorted && du > 0 && vtx_cmp (I.get dst (e-1)) v > 0 then
        sorted := false ;
    ) ;

    { n; m; sdeg; dst; wgt; sorted = !sorted; }

  let iter_succ f g u =
    for e = L.get g.sdeg u to L.get g.sdeg (u+1) - 1 do
      f (I.get g.dst e)
    done
    
  let iter_wgt_succ f g u =
    for e = L.get g.sdeg u to L.get g.sdeg (u+1) - 1 do
      f (W.get g.wgt e) (I.get g.dst e)
    done

  let iter f g =
    for u = 0 to g.n - 1 do
      for e = L.get g.sdeg u to L.get g.sdeg (u+1) - 1 do
        f u (W.get g.wgt e) (I.get g.dst e)
      done
    done

  (** Vertex sorted adjacency lists. *)
      
  let reverse g =
    of_edges ~n_estim:g.n (fun f -> iter (fun u w v -> f v w u) g)

  (** Sort adjacency lists according to destination number (in linear time)
      but makes copies of the graph. *)
  let sort g =
    if g.sorted then g else reverse (reverse g)
      
  let sorted_of_edges ?(n_estim=16) iter =
    let rev = of_edges ~n_estim (fun f -> iter (fun u w v -> f v w u)) in
    reverse rev (* reverse is equivalent to bucket sorting *)

  (** Dichotomic search of [v] in [a] between [l] (inclusive) 
      and [r] (exclusive). If [v] is present, returns the largest index
      associated to [v]. *)
  module Dicho (A : GenArray.Type) = struct
    type res = At of int | After of int | All_smaller | All_bigger | Empty
    let bsearch cmp a l0 r0 v =
      if l0 >= r0 then Empty else begin
        let l = ref l0 and r = ref r0 in
        while !l + 1 < !r do
          let m = (!l + !r) / 2 in (* l+1 <= m <= r-1 *)
          let v' = A.get a m in
          let c = cmp v v' in
          if c < 0 then r := m (* on the left, gives r >= l+1 *)
          else if c > 0 then l := m+1 (* on the right, only case produc. l>=r *)
          else l := m (* found, need rightmost *)
        done ;
        (* We have l0 <= l <= r <= r0 and l >= r-1 and l0 < r,
           implying l = r-1 or l = r *)
        let l = !l in
        if l >= r0 then All_smaller
        else
          let c = cmp v (A.get a l) in
          if c < 0 then (if l > l0 then After (l - 1) else All_bigger)
          else if c > 0 then (if l+1 < r0 then After l else All_smaller)
          else At l
      end
  end

  module IDicho = Dicho (I)
    
  let find_edge g u v =
    if not g.sorted then
      failwith "IntGraph.Make.find_edge: graph is not sorted!" ;
    let l = L.get g.sdeg u and r = L.get g.sdeg (u+1) in
    match IDicho.bsearch vtx_cmp g.dst l r v with
      | IDicho.At e -> e
      | _ -> raise Not_found

  let mem_edge g u v =
    try ignore (find_edge g u v) ; true with Not_found -> false

  let weight_edge g u v =
    W.get g.wgt (find_edge g u v)
      
  (* Edges have indexes also. *)
      
  type edge = int

  let edge_dst g e = I.get g.dst e
  let edge_wgt g e = W.get g.wgt e
    
  let iter_out_edges f g u =
    for e = L.get g.sdeg u to (L.get g.sdeg (u+1)) - 1 do
      f e
    done

      
  module LDicho = Dicho (L)
  let edg_cmp e e' = e - e'
    
  let edge_src g e =
    if g.m <= 0 then failwith "IntGraph.Make.src: empty graph" 
    else match LDicho.bsearch edg_cmp g.sdeg 0 (g.n + 1) e with
      | LDicho.At u | LDicho.After u -> u
      | _ -> failwith "IntGraph.Make.src: not an edge index."

  (** Edges a la ocamlgraph.

  type edge = vertex * weight * vertex

  let iter_succ_e f g u =
    for e = L.get g.sdeg u to L.get g.sdeg (u+1) - 1 do
      f (u, W.get g.wgt e, I.get g.dst e)
    done
    
  let iter_edges f g =
    for u = 0 to g.n - 1 do
      iter_succ_e (f u) g u
    done

  let iter_vertex f g =
    for u = 0 to g.n - 1 do f u done

  let of_iter_edges iter_edges =
    let iter f = iter_edges (fun (u, w, v) -> f u v w) in
    of_edges iter 
      
  *)
    
end

module IA = GenArray.OfInt
module I32A = GenArray.OfInt32

module IntWeighted = Make (IA) (IA) (IA)

module IntWeightedCompact = Make (I32A) (IA) (I32A)

module Unweighted = Make (IA) (IA) (GenArray.OfOne)

module UnweightedCompact = Make (I32A) (IA) (GenArray.OfOne)
  

