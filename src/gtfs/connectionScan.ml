
type t = { n : int; (* number of stops *)
           max_transf : int;
           arr : int array array;
           from : int array array;
         }

let create n max_transf =
  { n;
    max_transf;
    arr = Array.init max_transf (fun _ -> Array.make n max_int);
    from = Array.init max_transf (fun _ -> Array.make n max_int);
  }

module G = Gtfs.G
module LST = Gtfs.LST
module LS = Gtfs.LS

let cmp_arr (s, d, trp, t, a) (s', d', trp', t', a') =
  if a <> a' then a' - a
  else if d <> d' then d' - d
  else if trp <> trp' then trp' - trp
  else if s <> s' then s' - s
  else t' - t

let connections (_, lst, _, ls, conn_trip, _) =
  let trips = LS.create () in
  let conn = Array.make (Hashtbl.length conn_trip) (0, 0, 0, 0, 0) in
  let i = ref 0 in
  Hashtbl.iter (fun (u, v) trp ->
      let s, t = LST.label lst u and s', t' = LST.label lst v in
      conn.(!i) <- (LS.index ls s, t, LS.add trips trp, LS.index ls s', t');
      incr i;
    ) conn_trip;
  Array.sort cmp conn;
  conn

module CDicho =
  GenArray.Dicho (GenArray.OfArray
                    (struct type t = int * int * int * int * int end))

let not_conn = -1
  
let scan t conn (_, lst, transf, ls, _, _) src t dst =
  let i_start = 0 in
  for ntrf = 0 to t.max_transf - 1 do
    t.arr.(ntrf).(src) <- t;
    t.from.(ntf).(src) <- not_conn;
  done;
  for i = i_start to Array.length conn - 1 do
    let s, d, trp', s', a = conn.(i) in
    for ntrf = 0 to t.max_transf - 1 do
      let t = t.arr.(s).(ntrf) in
      let ntrf =
        let frm = t.from.(s).(ntrf) in
        if frm = not_conn then ntrf + 1 else
          let _, _, trp, _, _ = conn.(frm) in
          if trp = trp' then ntrf else ntrf + 1
      in
      for ntrf' = ntrf to t.max_transf - 1 do
        if a < t.arr.(ntrf').(s') then begin
            t.arr.(ntrf').(s') <- a;
            t.from.(ntrf').(s') <- i;
          end
      done
    done
  done;
  ()
