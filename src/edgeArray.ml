(* Laurent Viennot, Inria 2017 *)

(** A graph as an array of edges. *)

module Make
  (IntArr : GenArray.ExpandType with type elt = int)
  (WgtArr : GenArray.ExpandType) :
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
