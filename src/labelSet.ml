(** Set of labels that are numbered from [0] to [n-1] as they are inserted. *)
module Make (LabelArray : GenArray.ExpandType) = struct

  type index = int
  type label = LabelArray.elt

  module LabArr = LabelArray

  type t = { mutable n : int ;
             ltoi : (label, index) Hashtbl.t ;
             mutable itol : LabArr.t ;
           }
      
  let create ?(n_estim=16) () = {
    n = 0 ;
    ltoi = Hashtbl.create n_estim ;
    itol = LabArr.create n_estim ;
  }

  let add t u =
    try
      Hashtbl.find t.ltoi u
    with Not_found ->
      let i = t.n in
      Hashtbl.add t.ltoi u i ;
      if i >= LabArr.length t.itol then t.itol <- LabArr.expand t.itol (i+1) u ;
      LabArr.set t.itol i u ;
      t.n <- t.n + 1 ;
      i

  let n t = t.n
    
  let index t u = Hashtbl.find t.ltoi u

  let label t i = LabArr.get t.itol i
    
end
