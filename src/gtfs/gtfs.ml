(* Laurent Viennot, Inria 2017 *)

module R = Rows
module StopArray = GenArray.MakeOf(struct type t = string end)
module LS = LabelSet.Make (StopArray) (* stops *)
module StopTimeArray = GenArray.MakeOf(struct type t = string * int end)
module LST = LabelSet.Make (StopTimeArray) (* stop,time pairs *)
module EG = EdgeArray.Make (GenArray.OfInt) (GenArray.OfInt)
module G = IntGraph.IntWeighted

let date_of_day i = (* format : yyyymmdd, ex : 20170611 *)
  let y = i / 10000 in
  let i = i - y * 10000 in
  let m = i / 100 in
  let d = i - m * 100 in
  y, m, d

  
  
let to_graphs gtfs_dir day_date =

  let wrong_row r = R.fprint_row stderr r; failwith "wrong row" in

  let services = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "calendar.txt" in
  assert (cols = [R.Ident "service_id";
                  R.Ident "monday";
                  R.Ident "tuesday";
                  R.Ident "wednesday";
                  R.Ident "thursday";
                  R.Ident "friday";
                  R.Ident "saturday";
                  R.Ident "sunday";
                  R.Ident "start_date";
                  R.Ident "end_date"]);
  let d_cmp i i' = compare (date_of_day i) (date_of_day i') in
  (* week day computation *)
  let y, m, d = date_of_day day_date in
  assert (y >= 2017);
  let wd_20170101 = 6 in
  let mdays = Array.of_list [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
  let year = Array.fold_left (+) 0 mdays in
  let nd = ref 0 in
  for y' = 2017 to y-1 do nd := !nd + year + if y' mod 4 = 0 then 1 else 0 done;
  for m' = 1 to m - 1 do nd := !nd + mdays.(m'-1) done;
  if y mod 4 = 0 && m > 2 then incr nd;
  nd := !nd + (d - 1);
  let wday = (wd_20170101 + !nd) mod 7 in
  Printf.eprintf "week day : %d (0 for monday, 1 for tuesday, ...)\n" wday;
  (* rows : *)
  List.iter (fun r ->
    match r with
      | [R.Int srv;
         R.Int mon; R.Int tue; R.Int wed; R.Int thu;
         R.Int fri; R.Int sat; R.Int sun;
         R.Int d_start; R.Int d_end;] ->
        let week = Array.of_list [mon; tue; wed; thu; fri; sat; sun] in
        if week.(wday) = 1
           && d_cmp d_start day_date <= 0 && d_cmp day_date d_end <= 0 
        then Hashtbl.add services srv ()
      | r -> wrong_row r
  ) rows;
  (* exceptions : *)
  let cols, rows = R.read gtfs_dir "calendar_dates.txt" in
  assert (cols = [R.Ident "service_id";
                  R.Ident "date";
                  R.Ident "exception_type"]);
  List.iter (fun r ->
    match r with
      | [R.Int srv; R.Int d; R.Int exc] ->
        if d = day_date then begin
          if exc = 1 then Hashtbl.add services srv ()
          else if exc = 2 then Hashtbl.remove services srv
        end
      | r -> wrong_row r
  ) rows;
  Printf.eprintf "%d services\n" (Hashtbl.length services); flush stderr;

  let trips = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "trips.txt" in
  if cols = [R.Ident "route_id";
             R.Ident "service_id";
             R.Ident "trip_id";
             R.Ident "trip_headsign";
             R.Ident "trip_short_name";
             R.Ident "direction_id";
             R.Ident "shape_id"] (* ratp *)
  then begin
    List.iter (fun r ->
      match r with
        | [R.Int rte; R.Int srv; R.Int trp; _; _; R.Int dir; _] ->
          if Hashtbl.mem services srv then
            Hashtbl.add trips (string_of_int trp) (string_of_int rte, srv, dir)
        | r -> wrong_row r
    ) rows;
  end else begin
    assert (cols = [R.Ident "route_id";
             R.Ident "service_id";
             R.Ident "trip_id";
             R.Ident "trip_headsign";
             R.Ident "direction_id";
             R.Ident "block_id";]); (* stif *)
    List.iter (fun r ->
      match r with
        | [R.Ident rte; R.Int srv; R.Ident trp; _; R.Int dir; _] ->
          if Hashtbl.mem services srv then
            Hashtbl.add trips trp (rte, srv, dir)
        | r -> wrong_row r
    ) rows;
  end;
  Printf.eprintf "%d trips\n" (Hashtbl.length trips); flush stderr;

  let trf_ls = LS.create () in 
  let cols, rows = R.read gtfs_dir "stops.txt" in
  if cols = [R.Ident "stop_id";
             R.Ident "stop_code";
             R.Ident "stop_name";
             R.Ident "stop_desc";
             R.Ident "stop_lat";
             R.Ident "stop_lon";
             R.Ident "location_type";
             R.Ident "parent_station";] (* ratp *)
  then begin
    List.iter (fun r ->
      match r with
        | [R.Int s; _; _; _; _; _; _; _] ->
          ignore (LS.add trf_ls (string_of_int s))
        | r -> wrong_row r
    ) rows;
  end else begin
    assert (cols = [R.Ident "stop_id";
             R.Ident "stop_name";
             R.Ident "stop_desc";
             R.Ident "stop_lat";
             R.Ident "stop_lon";
             R.Ident "zone_id";
             R.Ident "stop_url";
             R.Ident "location_type";
             R.Ident "parent_station";]); (* stif *)
    List.iter (fun r ->
      match r with
        | [R.Ident s; _; _; _; _; _; _; _; _] -> ignore (LS.add trf_ls s)
        | r -> wrong_row r
    ) rows;
  end;
  Printf.eprintf "stops: n = %d\n" (LS.n trf_ls);
  flush stderr;
  
  let trf_eg = EG.create () in
  let cols, rows = R.read gtfs_dir "transfers.txt" in
  assert (cols = [R.Ident "from_stop_id";
                  R.Ident "to_stop_id";
                  R.Ident "transfer_type";
                  R.Ident "min_transfer_time";]);
  let str = string_of_int in
  List.iter (fun r ->
    match r with
      | [R.Ident tl; R.Ident hd; R.Int typ; R.Int tm;] ->
        EG.add trf_eg (LS.add trf_ls tl) 1 (LS.add trf_ls hd);
        EG.add trf_eg (LS.add trf_ls hd) 1 (LS.add trf_ls tl);
      | [R.Int tl; R.Int hd; R.Int typ; R.Int tm;] ->
        EG.add trf_eg (LS.add trf_ls (str tl)) 1 (LS.add trf_ls (str hd));
        EG.add trf_eg (LS.add trf_ls (str hd)) 1 (LS.add trf_ls (str tl));
      | r -> wrong_row r
  ) rows;
  Printf.eprintf "transfer graph: n = %d  m = %d\n" (LS.n trf_ls) (EG.m trf_eg);
  flush stderr;
  
  let trip_stops = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "stop_times.txt" in
  assert (cols = [R.Ident "trip_id";
                  R.Ident "arrival_time";
                  R.Ident "departure_time";
                  R.Ident "stop_id";
                  R.Ident "stop_sequence";
                  R.Ident "stop_headsign";
                  R.Ident "shape_dist_traveled"] (* ratp *)
         || cols = [R.Ident "trip_id";
                    R.Ident "arrival_time";
                    R.Ident "departure_time";
                    R.Ident "stop_id";
                    R.Ident "stop_sequence";
                    R.Ident "stop_headsign";
                    R.Ident "pickup_type";
                    R.Ident "drop_off_type";]); (* stif *)
  List.iter (fun r ->
    match r with
      | R.Int trp :: R.Time arr :: R.Time dep :: R.Int stp :: R.Int seq :: _ ->
        if Hashtbl.mem trips (string_of_int trp) then
          Hashtbl.add trip_stops (string_of_int trp) (seq, arr, str stp, dep)
      | R.Ident trp :: R.Time arr :: R.Time dep :: R.Ident stp :: R.Int seq :: _
        ->
        if Hashtbl.mem trips trp then
          Hashtbl.add trip_stops trp (seq, arr, stp, dep)
      | r -> wrong_row r
  ) rows;

  let eg = EG.create () and lst = LST.create () in
  Hashtbl.iter (fun trp _ ->
    let stops = Hashtbl.find_all trip_stops trp in
    match List.sort (fun (s, _, _, _) (s', _, _, _) -> s - s') stops with
      | [] -> assert false
      | first :: stops ->
        let rec iter (seq, arr, stp, dep) = function
          | [] -> ()
          | (seq', arr', stp', dep' as stop') :: stops ->
            assert (seq < seq' && dep <= arr');
            EG.add eg (LST.add lst (stp, dep)) 1 (LST.add lst (stp', arr'));
            iter stop' stops
        in iter first stops
  ) trips;
  Printf.eprintf "%d stop times\n" (Hashtbl.length trip_stops);
  Printf.eprintf "connection graph: n = %d  m = %d\n" (LST.n lst) (EG.m eg);
  flush stderr;

  let igraph eg = G.of_edges ~n_estim:(EG.n eg) (fun f -> EG.iter f eg) in
  (igraph eg), lst, (igraph trf_eg), trf_ls

  
module Trav = Traversal.Make (G) (GenArray.OfInt) (Traversal.IntWeight)
            
let time_ordering conn lst =
  let n = G.n conn in
  let trv = Trav.create n in
  let ext = Trav.dag_extension trv conn in
  G.iter (fun u _ v -> assert (ext.(u) < ext.(v))) conn ;
  let stops = Array.make n ("",0) in
  for u = 0 to n - 1 do stops.(ext.(u)) <- LST.label lst u done;
  Array.stable_sort (fun (_,t) (_,t') -> t - t') stops;
  let ord = Array.make n (-1) in
  for i = 0 to n - 1 do
    let u = LST.index lst stops.(i) in
    ord.(i) <- u;
    ext.(u) <- i;
  done;
  G.iter (fun u _ v -> assert (ext.(u) < ext.(v))) conn ;
  ord

(** Returns an array with all time events for a given stop. *)
let stop_times lst ls =
  let n = LS.n ls in
  let len = Array.make n 0 in
  for i = 0 to LST.n lst - 1 do
    let s, t = LST.label lst i in
    try
      let is = LS.index ls s in
      len.(is) <- len.(is) + 1
    with e -> Printf.eprintf "%s %d\n" s t; raise e
  done;
  let tms = Array.init n (fun is -> Array.make len.(is) 0) in
  for i = LST.n lst - 1 downto 0 do
    let s, t = LST.label lst i in
    let is = LS.index ls s in
    let it = len.(is) - 1 in
    len.(is) <- it;
    tms.(is).(it) <- t; 
  done;
  for is = 0 to LS.n ls - 1 do
    Array.sort (fun t t' -> t - t') tms.(is)
  done;
  tms


module IDicho = GenArray.Dicho (GenArray.OfInt)

  let next_stop_time ls st_tms s t =
    let cmp t t' = t - t' in
    let is = LS.index ls s in
    match IDicho.bsearch cmp st_tms.(is) 0 (Array.length st_tms.(is)) t with
    | IDicho.At it -> st_tms.(is).(it)
    | IDicho.After it -> st_tms.(is).(it + 1)
    | IDicho.All_smaller -> raise Not_found
    | IDicho.All_bigger -> st_tms.(is).(0)
    | IDicho.Empty -> raise Not_found
              
let time_expanded_graph (conn, lst, transf, ls) =

  let st_tms = stop_times lst ls in
  
  let eg = EG.create () in
  (* Connections : *)
  G.iter (fun u w v ->
    let _, t = LST.label lst u and _, t' = LST.label lst v in
    EG.add eg u (t' - t) v;
  ) conn;
  (* Wait at stop : *)
  for is = 0 to LS.n ls - 1 do
    let s = LS.label ls is in
    let t_end = Array.length st_tms.(is) in
    if t_end > 0 then begin
      let t = ref st_tms.(is).(0) in
      for it = 1 to t_end - 1 do
        let t' = st_tms.(is).(it) in
        EG.add eg (LST.index lst (s,!t)) (t' - !t) (LST.index lst (s,t'));
        t := t';
      done;
    end;
  done;
  (* Transfers : *)
  let dij = Trav.create (LS.n ls) in
  for is = 0 to LS.n ls - 1 do
    let s = LS.label ls is in
    let nvis = Trav.dijkstra dij transf [is] in
    for vt = 1 to nvis - 1 do
      let is' = Trav.visit_at dij vt in
      let s' = LS.label ls is' in
      let dt = Trav.dist dij is' in
      for it = 0 to Array.length st_tms.(is) - 1 do
        let t = st_tms.(is).(it) in
        try
          let t' = next_stop_time ls st_tms s' (t + dt) in
          EG.add eg (LST.index lst (s,t)) (t' - t) (LST.index lst (s',t'));
        with Not_found -> () (* no next connection *)
      done;
    done;
    Trav.clear dij;
  done;
  
  G.of_edges ~n_estim:(EG.n eg) (fun f -> EG.iter f eg)
