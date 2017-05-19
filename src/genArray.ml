(* Laurent Viennot, Inria 2017 *)

(** Generic module for arrays (standard Array or Bigarray). *)

module type Type = sig
  type t
  type elt
  val make : int -> elt -> t
  val create : int -> t
  val length : t -> int
  val set : t -> int -> elt -> unit
  val get : t -> int -> elt
  val blit : t -> int -> t -> int -> int -> unit
end

module type ExpandType = sig
  include Type

  (** [expand a len df] should return an array of length [len' >= c * len]
      for some constant [c], filled with [a] plus extra cells up to index
      [len - 1] set to [dft]. Array [a] can be destroyed or reused and
      is not accessed after. *)
  val expand : t -> int -> elt -> t

end

module MakeExpand (A : Type) : ExpandType
       with type elt = A.elt and type t = A.t
  = struct
  include A
  let expand a len dft =
    let len_a = length a in
    assert (len > len_a) ;
    let len = max 2 (2*len) in
    let b = make len dft in
    blit a 0 b 0 len_a ;
    b
end

module OfArray (Elt : sig type t end) = struct
  type elt = Elt.t
  type t = elt array
  let make = Array.make
  let create _ = [||]
  let length = Array.length
  let set = Array.set
  let get = Array.get
  let blit = Array.blit
end

module MakeOf (Elt : sig type t end) = MakeExpand (OfArray (Elt))

module OfInt =  MakeOf (struct type t = int end)

module OfInt32 = struct 
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


module Uniform (E : sig type t val elt : t end) = struct
  type t = unit
  type elt = E.t
  let unique_val = E.elt
  let make _ _ = ()
  let create _ = ()
  let length _ = max_int
  let set _ _ e = assert (e = E.elt)
  let get _ _ = unique_val
  let blit _ _ _ _ _ = ()
  let expand a _ _ = a
end

module OfOne = Uniform (struct type t = int let elt = 1 end)


(** Queue in an array. *)
module Queue (A : ExpandType) = struct
  type t = { mutable v : A.t ; mutable front : int ; mutable back : int ; }
  type elt = A.elt

  let create n =
    { v = A.create n ; front = 0 ; back = 0 ; }

  let is_empty t = t.front = t.back

  let clear t =
    t.front <- 0 ; t.back <- 0

  let add t e = 
    if t.back >= A.length t.v then
      t.v <- A.expand t.v (t.back + 1) e ;
    A.set t.v t.back e ;
    t.back <- t.back + 1

  let peek t =
    if is_empty t then raise Not_found ;
    A.get t.v t.front

  let compact t =
    let len = t.back - t.front in
    A.blit t.v t.front t.v 0 len ;
    t.front <- 0 ;
    t.back <- len

  let pop t =
    if is_empty t then raise Not_found ;
    let e = A.get t.v t.front in
    t.front <- t.front + 1 ;
    if t.front > t.back / 2 && t.back > (A.length t.v) / 2 then compact t ;
    e

  let size t = t.back - t.front
end
  
module QueueOf (E : sig
  type t
end)  = Queue (MakeOf (E))


(** Stack in an array. *)
module Stack (A : ExpandType) = struct
    type t = { mutable v : A.t ; mutable back : int ; }
  type elt = A.elt

  let create n = { v = A.create n ; back = -1 ; }

  let is_empty t = t.back < 0

  let clear t =
    t.back <- -1

  let add t e = 
    t.back <- t.back + 1 ;
    if t.back >= A.length t.v then
      t.v <- A.expand t.v (t.back + 1) e ;
    A.set t.v t.back e 

  let push = add

  let peek t = 
    if is_empty t then raise Not_found ;
    A.get t.v t.back

  let pop t = 
    if is_empty t then raise Not_found ;
    let e = A.get t.v t.back in
    t.back <- t.back - 1 ;
    e

  let size t = t.back + 1
end

module StackOf (E : sig
    type t
end)  = Stack (MakeOf (E))


(** Dichotomic search of [v] in [a] between [l] (inclusive) 
      and [r] (exclusive). If [v] is present, returns the largest index
      associated to [v]. *)
module Dicho (A : Type) = struct
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
