module type ArrayT = sig
  type t
  type elt
  val make : int -> elt -> t
  val create : int -> t
  val length : t -> int
  val set : t -> int -> elt -> unit
  val get : t -> int -> elt
  val blit : t -> int -> t -> int -> int -> unit
end
module type ExpandArray = sig
  include ArrayT

  (** [expand a len df] should return an array of length [len' >= c * len]
      for some constant [c], filled with [a] plus extra cells up to index
      [len - 1] set to [dft]. Array [a] can be destroyed or reused and
      is not accessed after. *)
  val expand : t -> int -> elt -> t
end
  

module EdgeList
  (IntArr : ExpandArray with type elt = int) (WgtArr : ExpandArray) :
sig
  type t
  type vertex = int
  type weight
  val add : t -> vertex -> weight -> vertex -> unit
  val create : ?m_estim:int -> unit -> t
  val n : t -> int
  val m : t -> int
  val iter : (vertex -> weight -> vertex -> unit) -> t -> unit
  val edge : t -> int -> (vertex * weight * vertex)
  val no_vertex : vertex
end with type weight = WgtArr.elt = struct

  module I = IntArr
  module W = WgtArr

  type vertex = int
  type weight = W.elt
    
  type t = {
    mutable n : int;
    mutable m : int;
    mutable src : I.t;
    mutable dst : I.t;
    mutable wgt : W.t;
  }

  let n g = g.n
  let m g = g.m

  let create ?(m_estim=16) () = {
    n = 0;
    m = 0;
    src = I.create m_estim ;
    dst = I.create m_estim ;
    wgt = W.create m_estim ;
  }

  let no_vertex = -1

  let add g u w v =
    if u >= g.n then g.n <- 1 + u ;
    if v >= g.n then g.n <- 1 + v ;
    let m = g.m in
    if m >= I.length g.src then begin
      g.src <- I.expand g.src (m+1) no_vertex ;
      g.dst <- I.expand g.dst (m+1) no_vertex ;
      g.wgt <- W.expand g.wgt (m+1) w ;      
    end ;
    I.set g.src m u ;
    I.set g.dst m v ;
    W.set g.wgt m w ;
    g.m <- m + 1

  let iter f g =
    for e = 0 to g.m - 1 do
      f (I.get g.src e) (W.get g.wgt e) (I.get g.dst e)
    done

  let edge g e = I.get g.src e, W.get g.wgt e, I.get g.dst e
      
end


module IGraph
  (LongIntArray : ExpandArray with type elt = int)
  (IntArr : ExpandArray with type elt = int)
  (WgtArr : ExpandArray) :
sig
  type t
  type vertex = int
  type weight
  val of_edges : ?n_estim:int ->
    ((vertex -> weight -> vertex -> unit) -> unit) -> t
  val reverse : t -> t
  val sort : t -> t
  val sorted_of_edges : ?n_estim:int ->
    ((vertex -> weight -> vertex -> unit) -> unit) -> t
  val n : t -> int
  val m : t -> int
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_wgt_succ : (weight -> vertex -> unit) -> t -> vertex -> unit
  val iter : (vertex -> weight -> vertex -> unit) -> t -> unit
  val mem_edge : t -> vertex -> vertex -> bool
  type edge = int
  val iter_out_edges : (edge -> unit) -> t -> vertex -> unit
  val edge_src : t -> edge -> vertex
  val edge_dst : t -> edge -> vertex
  val edge_wgt : t -> edge -> weight
  val no_vertex : vertex
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
  module Dicho (A : ArrayT) = struct
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
    if not g.sorted then failwith "IGraph.mem_edge: graph is not sorted!" ;
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
    if g.m <= 0 then failwith "IGraph.src: empty graph" 
    else match LDicho.bsearch edg_cmp g.sdeg 0 (g.n + 1) e with
      | LDicho.At u | LDicho.After u -> u
      | _ -> failwith "IGraph.src: not an edge index."

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

  
(** Set of labels that are numbered from [0] to [n-1] as they are inserted. *)
module LabelSet (Label : sig type t end) = struct

  type index = int
  type label = Label.t

  open Batteries
      
  type t = { mutable n : int ;
             ltoi : (label, index) Hashtbl.t ;
             itol : label DynArray.t ;
           }
      
  let create ?(n_estim=16) () = {
    n = 0 ;
    ltoi = Hashtbl.create n_estim ;
    itol = DynArray.make n_estim ;
  }

  let add t u =
    try
      Hashtbl.find t.ltoi u
    with Not_found ->
      let i = t.n in
      Hashtbl.add t.ltoi u i ;
      DynArray.add t.itol u ;
      t.n <- t.n + 1 ;
      i

  let n t = t.n
    
  let index t u = Hashtbl.find t.ltoi u

  let label t i = DynArray.get t.itol i
    
end


module ExpArray (A: ArrayT) : sig
  include ExpandArray
end with type elt = A.elt = struct
  include A
  let expand a len dft =
    let len_a = length a in
    assert (len > len_a) ;
    let len =
      if len < 1024*1024 then max 16 (2*len)
      else 8 * (1 + 3 * len / 16)
    in
    let b = make len dft in
    blit a 0 b 0 len_a ;
    b
  (* Much slower :  
  type t = int * A.t
  type elt = A.elt
  let make n e = n, make n e
  let create n = n, create 0
  let length (_, a) = length a
  let set (_, a) = set a
  let get (_,a) = get a
  let blit (_, a) pos (_, b) = blit a pos b *)
end

module IntArray =  ExpArray (struct (* Classic int arrays. *)
  include Array
  type elt = int
  type t = elt array
  let create _ = [||]
end)

module UniformArray (E : sig type t val elt : t end) = struct
  type t = unit
  type elt = E.t
  let unique_val = E.elt
  let make _ _ = ()
  let create _ = ()
  let length _ = max_int
  let set _ _ _ = ()
  let get _ _ = unique_val
  let blit _ _ _ _ _ = ()
  let expand a _ _ = a
end

module UnifIntArray = UniformArray (struct type t = int let elt = 1 end)
  
module BatIntDynArray = struct
  open Batteries
  module A = BatDynArray
  type elt = int
  type t = elt A.t
  let make n e =
    let a = A.make n in
    for i=0 to n-1 do A.add a e done ;
    a
  let create _ = A.create ()
  let length = A.length
  let set = A.set
  let get = A.get
  let blit = A.blit
  let expand a len dft =
    let len_a = length a in
    assert (len > len_a) ;
    let len = 8 * (1 + 3 * len / 16) in
    for i=len_a to len-1 do A.add a dft done ;
    a
end
  
module Int32Array = struct 
  module B = Bigarray
  module BA1 = B.Array1
  type t = (int32, B.int32_elt, B.c_layout) BA1.t
  type elt = int
  let create n = BA1.create B.int32 B.c_layout n
  let make n e =
    let e = Int32.of_int e in
    let t = BA1.create B.int32 B.c_layout n in
    for i=0 to n-1 do t.{i} <- e done ;
    t
  let get t i = Int32.to_int (BA1.get t i)
  let set t i e = BA1.set t i (Int32.of_int e)
  let length = BA1.dim
  let blit s os t ot len =
    let s = BA1.sub s os len and t = BA1.sub t ot len in
    BA1.blit s t
  let expand a len dft =
    let len_a = length a in
    assert (len > len_a) ;
    (* Bigarrays are intended to be big. *)
    let len = max (1024 * 1024) (8 * (1 + 3 * len / 16)) in
    let b = BA1.create B.int32 B.c_layout len in
    blit a 0 b 0 len_a ;
    let dft = Int32.of_int dft in
    for i=len_a to len-1 do b.{i} <- dft done ;
    b
end

(** Heap implementation in a vector. Minimum gets first out. *)
module HeapOfArray (A : ExpandArray)  (E : sig
  type t
  val compare : t -> t -> int
end with type t = A.elt)
= struct

  type t = { mutable v : A.t ; mutable back : int ; }
  type elt = A.elt

  let create n =
    { v = A.create n ; back = -1 ; }

  let is_empty t = t.back < 0

  let clear t =
    t.back <- -1

  let add t e = 
    t.back <- t.back + 1 ;
    if t.back >= A.length t.v then
      t.v <- A.expand t.v (t.back + 1) e ;
    let rec moveup i =
      if i = 0 then A.set t.v i e else begin
        let i' = (i+1) / 2 - 1 in (* parent *)
        let e' = A.get t.v i' in
        if E.compare e e' > 0 then A.set t.v i e else begin
          A.set t.v i e' ;
          moveup i'
        end
      end
    in moveup t.back

  let push = add

  let peek_min t = 
    if is_empty t then raise Not_found ;
    A.get t.v 0

  let pop_min t = 
    if is_empty t then raise Not_found ;
    let e_min = A.get t.v 0 in
    if t.back = 0 then t.back <- -1 (* it was last element *) 
    else begin
      let e = A.get t.v t.back in
      t.back <- t.back - 1 ;
      let rec movedown i =
        let i1 = 2 * i + 1 in (* first son *)
        if i1 > t.back then A.set t.v i e else begin
          let i2 = i1 + 1 in (* second son *)
          let i' = 
            if i2 <= t.back && E.compare (A.get t.v i2) (A.get t.v i1) < 0
            then i2 else i1 in
          let e' = A.get t.v i' in
          if E.compare e e' < 0 then A.set t.v i e 
          else begin
            A.set t.v i e' ;
            movedown i'
          end
        end
      in movedown 0
    end ;
    e_min

  let size t = t.back

end


module IIArray = ExpArray (struct
  include Array
  type elt = int * int
  type t = elt array
  let create _ = [||]
end)
    
module Dijkstra
  (Graph : sig
    type t
    type vertex = int
    type weight
    val n : t -> int
    val iter_wgt_succ : (weight -> vertex -> unit) -> t -> vertex -> unit
  end)
  (WgtArr : ExpandArray with type elt = Graph.weight) = struct

  open Batteries
    
  module G = Graph

  module VWA = struct
    include Array
    type elt = G.vertex * G.weight
    type t = elt array
    let create _ = [||]
  end
    
  module Heap = HeapOfArray (ExpArray(VWA)) (struct
    type t = G.vertex * G.weight
    let compare (u, du) (v, dv) =
      let c = du - dv in
      if c <> 0 then c else u - v
  end)
    
  type t = {
    n : int;
    queue : Heap.t;
    in_queue: BitSet.t;
    dist : int array;
    mutable visit_nb : int;
    visit_ordering : int array;
    visit_time : int array;
  }

  let wgt_infinity = max_int
  let infinity = max_int
      
  let create n = {
    n = n ;
    queue = Heap.create n;
    in_queue = BitSet.create n;
    dist = Array.make n wgt_infinity;
    visit_nb = 0;
    visit_ordering = Array.make n G.no_vertex;
    visit_time = Array.make n infinity;
  }

  let dist t u = t.dist.(u)
  let visit_time t u = t.visit_time.(u)
  let visit_at t i = t.visit_ordering.(i)
  let visit_nb t = t.visit_nb

  let clear ?(last_visit_nb=0) t =
    Heap.clear t.queue; 
    for i=last_visit_nb to t.visit_nb - 1 do
      let u = t.visit_ordering.(i) in
      BitSet.unset t.in_queue u;
      t.dist.(u) <- wgt_infinity;
      t.visit_ordering.(i) <- G.no_vertex;
      t.visit_time.(u) <- infinity;
    done;
    t.visit_nb <- last_visit_nb

      
  let shortest_path_tree t g sources =
    assert (t.n >= G.n g);

    let nq = ref 0 in
    let add v dv =
      incr nq ;
      Heap.add t.queue (v, dv) ;
      BitSet.set t.in_queue v;
      t.dist.(v) <- dv ;
    in
    List.iter (fun s -> add s 0) sources ;
    
    while not (Heap.is_empty t.queue) do
      let u, du = Heap.pop_min t.queue in
      if du = t.dist.(u) then begin
        assert (t.visit_time.(u) = infinity);
        BitSet.unset t.in_queue u;
        t.visit_ordering.(t.visit_nb) <- u;
        t.visit_time.(u) <- t.visit_nb;
        t.visit_nb <- t.visit_nb + 1;

        G.iter_wgt_succ (fun w v ->
          assert (w >= 0);
          let dv = du + w in
          assert (dv < wgt_infinity) ;
          if dv < t.dist.(v) then begin
            t.dist.(v) <- dv ;
            add v dv
          end
        ) g u ;
      end
    done ;
    !nq
    
end



let () =
  Printexc.record_backtrace true ;
  try
    let module IA = IntArray in
    let module LA = IntArray in
    let module WA = IntArray in

    let t = ref (Sys.time ()) in
    let top msg =
      let t' = Sys.time () in
      Printf.eprintf "#  --   %s in %fs\n" msg (t' -. !t) ; flush stderr ;
      t := t'
    in
    
    let module E = EdgeList (IA) (WA) in
    let e = E.create () in

    let module LabS = LabelSet (struct type t = int end) in
    let lbl = LabS.create () in
    
    let lex = Lexing.from_channel stdin in
    begin try while true do
        match Ints.next lex, Ints.next lex, Ints.next lex with
          | Some u, Some v, Some w ->
            let u = LabS.add lbl u in let v = LabS.add lbl v in
            E.add e u w v
          | Some _, _, _ -> failwith "incomplete triple at the end"
          | _ -> raise Not_found
      done with Not_found -> () end ;
    let n = E.n e and m = E.m e in
    Printf.eprintf "# n=%d m=%d\n" n m ; flush stderr ;
    top "loaded" ;

    Random.init 1234 ;
    (*    
    let module B = Batteries in
    let perm_vtx = B.Enum.range 0 ~until:(n-1) |> B.Random.shuffle in
    let perm_edg = B.Enum.range 0 ~until:(m-1) |> B.Random.shuffle in
    top "perms" ;
    *)  

    let module G = IGraph (LA) (IA) (WA) in 
    let g = G.sorted_of_edges ~n_estim:n (fun f -> (* E.iter f e *)
      for i=0 to m-1 do
        let u,w,v = E.edge e i (*perm_edg.(i*) in
        assert (u < n && v < n) ;
        f u w v (*perm_vtx.(u) w perm_vtx.(v*)
      done
    ) in
    let n = G.n g and m = G.m g in
    
    (* ocamlgraph test : 
    top "IGraph adj" ;
    let module I = struct
      type t = int
      let compare i j = i - j
      let hash i = i
      let equal i j = i = j
      let default = 1
    end in
    let module G =
          Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (I) (I) in
    let g = G.create () in
    E.iter (fun u w v ->
      G.add_edge_e g (u, w, v)
    ) e ;
    G.add_edge_e g (0, 1, 0) ;
    let n = G.nb_vertex g and m = G.nb_edges g in *)

    Printf.eprintf "# n=%d m=%d\n" n m ;
    top "adj" ;

    Gc.compact () ;
    Printf.eprintf "# mem=%d\n"
      (8 * let s = Gc.quick_stat() in s.Gc.heap_words + s.Gc.stack_size) ;
    flush stderr ;
    top "compact" ;

    let lab = LabS.label lbl in
    let sum = ref 0 in
    G.iter (fun u w v ->
      (* Printf.printf "%d %d %d\n" (lab u) (lab v) w ; *)
      sum := !sum + w + v        
    ) g ;
    Printf.eprintf "# sum=%d\n" !sum;
    top "iter_edges" ;

    for u=0 to n-1 do
      G.iter_out_edges (fun e ->
        if u <> G.edge_src g e then begin
          Printf.eprintf " -------- u=%d ul=%d e=%d ----------\n"
            u (LabS.label lbl u) e ;
          for u=u-3 to u+1 do
            Printf.eprintf " --- u=%d ul=%d ---\n" u (LabS.label lbl u) ;
            G.iter_out_edges (fun e ->
              let u = G.edge_src g e in
              let v = G.edge_dst g e in
              Printf.eprintf "e=%d v=%d lv=%d   u=%d\n"
                e v (LabS.label lbl v) u ;
            ) g u ;
          done ;
          assert false ;
          end
      ) g u ;
    done ;
    top "edge_src";
    
    (*G.iter_edges_e (fun _ -> ()) g ;
    top "iter_edges" ;*)

    Printf.eprintf "# mem=%d\n"
      (8 * let s = Gc.quick_stat() in s.Gc.heap_words + s.Gc.stack_size) ;
    flush stderr ;
    top "mem";

    let count = ref 0 and test = 1000000 in
    for _=1 to test do
      let u = Random.int n in let v = Random.int n in
      if G.mem_edge g u v then begin
        incr count ;
        (* Printf.eprintf "# %d %d\n" u v ; *)
      end
    done ;
    Printf.eprintf "# nb edges found : %d / %d\n" !count test ; 
    top "mem_edge" ;

    let module Dij = Dijkstra (G) (WA) in
    let s = 0 and t = try int_of_string Sys.argv.(1) with _-> 100000 mod n in
    let dij = Dij.create n in
    top "dij create" ;
    
    let nq = Dij.shortest_path_tree dij g [s] in
    let d = Dij.dist dij t in
    Printf.eprintf "# Dijkstra enqueues : %d d(%d, %d) = %d\n" nq
      (lab s) (lab t) d ; 
    top "dijkstra" ;
    
    
    (* Comparison on New York :
                   time_adj          iter_edg   mem_edge(10^6) dij    mem
    OcamlGraph   : 0.94s             0.034s     0.62s                 150753480b

    IGraph       : 0.11s(+0.54sload) 0.015s                    0.25s  26263768b
                                                     BatHeap : 0.41s
     BatDynArray : 0.17s(+0.55sload) 0.020s     0.25s
          sorted : 0.21s(+0.54sload) 0.015s     0.23s                 26263768b
 int32 big array : 0.69s(+0.48sload) 0.093s     0.42s                 twice less
   int32 & array : 0.51s(+0.48sload) 0.064s     0.29s

    rev_graph.cc : 0.25s (+2.03s)    0.005s     0.35s          0.071s

       With permutations :
    OcamlGraph   : 0.63s             0.038s     0.63s
    IGraph       : 0.44s(+0.53s)
     BatDynArray : 
          sorted : 0.61s(+0.53s)     0.016s     0.23s
 int32 big array : 
   int32 & array : 1.06s(+0.47sload) 0.063s     0.29s

       On France n=3866333 m=9084548 :
       
                 : load    adj     itedg     memedg    dijkstra
  OcamlGraph     :         23.4s   1.0s      0.5s      
  OcamlGraph Lbl :         19.3s   0.86s     0.86s  

  IGraph         : 17s     5.5s    0.22s     0.34s     4.7s
  int32          : 14s     9s      0.76s     0.40s     
  IGraph  perms  :         11.3s   0.21s     0.34s
  int32   perms  :         17.5s   0.81s     0.42s 

    rev_graph.cc : 40s     5.4s    0.10s     0.36s     1.59s

1 3 20
1 2 10
2 4 50
3 5 10
5 7 10
5 9 10
5 6 10
6 4 10

    G.iter (Printf.eprintf "%d %d %d\n") g ;
    Printf.eprintf "# mem_edge : \n" ;
    List.iter
      (fun (u, v) -> Printf.eprintf "%d %d : %b\n" u v (G.mem_edge g u v))
      [5,5; 5,6; 5,7; 5,8; 5,9; 5,10;];
    *)
    
    ()
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    flush stderr ;
    exit 2
