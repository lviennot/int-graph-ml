(* Laurent Viennot, Inria 2017 *)

(** Heap implementation in a vector. Minimum gets first out. *)
module OfArray (A : GenArray.ExpandType)  (E : sig
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


module Make (E : sig
  type t
  val compare : t -> t -> int
end)  = OfArray (GenArray.MakeExpand (GenArray.OfArray (E))) (E)


  
