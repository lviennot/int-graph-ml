(* Laurent Viennot, Inria 2017 *)

(** Construct graphs from General Transit Feed Specification (GTFS) files.
    See https://developers.google.com/transit/gtfs/reference/ *)

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

type col_type = Ident | Int | Float | Time

let string_of_col_type = function
  | Ident -> "Ident" | Int -> "Int" | Float -> "Float" | Time -> "Time"

let row_select first_row sel =
  let cells = Array.make (List.length sel) R.Empty in
  let to_sel = Array.make (List.length first_row) (-1, "", Ident) in
  let id c = match c with R.Ident s -> s | _ -> assert false in
  let cols = List.mapi (fun i c -> id c, i) first_row in
  List.iteri (fun i (typ, c) -> to_sel.(List.assoc c cols) <- i, c, typ) sel;
  fun row ->
  let n = ref 0 in
  List.iteri (fun i cell ->
      let j, col, typ = to_sel.(i) in
      if j >= 0 then begin
          incr n;
          cells.(j) <-
            match typ, cell with
            | Ident, R.Ident _ -> cell
            | Ident, R.Int i -> R.Ident (string_of_int i)
            | Ident, R.Float f -> R.Ident (string_of_float f)
            | Int, R.Int _ -> cell
            | Int, R.Empty -> R.Int 0
            | Float, R.Float _ -> cell
            | Time, R.Time _ -> cell
            | _ -> failwith (Printf.sprintf "Gtfs: bad %s cell (%s) : %s"
                           col (string_of_col_type typ) (R.cell_to_string cell))
        end
    ) row;
  if !n <> Array.length cells then failwith "Gtfs: Wrong number of cells.";
  Array.to_list cells
  
let to_graphs gtfs_dir day_date t_from t_to =

  let wrong_row r = R.fprint_row stderr r; failwith "wrong row" in

  let services = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "calendar.txt" in
  let read_row = row_select cols [ Ident, "service_id";
                                   Int, "monday";
                                   Int, "tuesday";
                                   Int, "wednesday";
                                   Int, "thursday";
                                   Int, "friday";
                                   Int, "saturday";
                                   Int, "sunday";
                                   Int, "start_date";
                                   Int, "end_date"] in
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
    match read_row r with
      | [R.Ident srv;
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
  let read_row = row_select cols [Ident, "service_id";
                                  Int, "date";
                                  Int, "exception_type"] in
  List.iter (fun r ->
    match read_row r with
      | [R.Ident srv; R.Int d; R.Int exc] ->
        if d = day_date then begin
          if exc = 1 then Hashtbl.add services srv ()
          else if exc = 2 then Hashtbl.remove services srv
        end
      | r -> wrong_row r
  ) rows;
  Printf.eprintf "%d services\n" (Hashtbl.length services); flush stderr;

  let trips = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "trips.txt" in
  let read_row = row_select cols [Ident, "route_id";
                                  Ident, "service_id";
                                  Ident, "trip_id";
                                  Int, "direction_id"] in
  List.iter (fun r ->
      match read_row r with
      | [R.Ident rte; R.Ident srv; R.Ident trp; R.Int dir;] ->
         if Hashtbl.mem services srv then
           Hashtbl.add trips trp (rte, srv, dir)
      | r -> wrong_row r
    ) rows;
  Printf.eprintf "%d trips\n" (Hashtbl.length trips); flush stderr;

  let trf_ls = LS.create () in 
  let cols, rows = R.read gtfs_dir "stops.txt" in
  let read_row = row_select cols [Ident, "stop_id"] in
  List.iter (fun r ->
      match read_row r with
        | [R.Ident s;] ->
          ignore (LS.add trf_ls s)
        | r -> wrong_row r
    ) rows;
  Printf.eprintf "stops: n = %d\n" (LS.n trf_ls);
  flush stderr;
  
  let trf_eg = EG.create () in
  let trf_edges = Hashtbl.create 10000 in
  let cols, rows = R.read gtfs_dir "transfers.txt" in
  let read_row = row_select cols [Ident, "from_stop_id";
                                  Ident, "to_stop_id";
                                  Int, "transfer_type";
                                  Int, "min_transfer_time";] in
  List.iter (fun r ->
    match read_row r with
    | [R.Ident tl; R.Ident hd; R.Int typ; R.Int tm;] ->
       let tm = if typ = 1 then 0 else tm in
       if typ <> 3 then begin
           let s = LS.add trf_ls tl and s' = LS.add trf_ls hd in
           EG.add trf_eg s tm s';
           Hashtbl.add trf_edges (s, s') tm;
         end
    | r -> wrong_row r
    ) rows;
  EG.iter (fun s tm s' ->
      if not (Hashtbl.mem trf_edges (s', s)) then EG.add trf_eg s' tm s
    ) trf_eg;
  Printf.eprintf "transfer graph: n = %d  m = %d\n" (LS.n trf_ls) (EG.m trf_eg);
  flush stderr;
  
  let trip_stops = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "stop_times.txt" in
  let read_row = row_select cols [Ident, "trip_id";
                                  Time, "arrival_time";
                                  Time, "departure_time";
                                  Ident, "stop_id";
                                  Int, "stop_sequence";] in
  let nc = ref 0 in
  List.iter (fun r ->
      incr nc;
      let in_t_wind arr dep = t_from <= dep && arr <= t_to  in
      match read_row r with
      | [R.Ident trp; R.Time arr; R.Time dep; R.Ident stp; R.Int seq;] ->
         if Hashtbl.mem trips trp && in_t_wind arr dep then
           Hashtbl.add trip_stops trp (seq, arr, stp, dep)
      | r -> wrong_row r
    ) rows;
  Printf.eprintf "%d connections read\n" !nc;
  flush stderr;

  let eg = EG.create () and lst = LST.create () in
  let trp_of_conn = Hashtbl.create 100000 in
  Hashtbl.iter (fun trp _ ->
    let stops = Hashtbl.find_all trip_stops trp in
    match List.sort (fun (s, _, _, _) (s', _, _, _) -> s - s') stops with
      | [] -> ()
      | first :: stops ->
        let rec iter (seq, arr, stp, dep) = function
          | [] -> ()
          | (seq', arr', stp', dep' as stop') :: stops ->
            assert (seq < seq' && dep <= arr');
            let u = LST.add lst (stp, dep) and v = LST.add lst (stp', arr') in
            EG.add eg u (arr' - dep) v;
            Hashtbl.add trp_of_conn (u,v) trp;
            iter stop' stops
        in iter first stops
  ) trips;
  Printf.eprintf "%d stop times\n" (Hashtbl.length trip_stops);
  Printf.eprintf "connection graph: n = %d  m = %d\n" (LST.n lst) (EG.m eg);
  flush stderr;

  let igraph eg = G.of_edges ~n_estim:(EG.n eg) (fun f -> EG.iter f eg) in
  (igraph eg), lst, (igraph trf_eg), trf_ls, trp_of_conn

  
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
              
let time_expanded_graph (conn, lst, transf, ls, _) =

  let st_tms = stop_times lst ls in
  
  let eg = EG.create () in
  (* Connections : *)
  G.iter (EG.add eg) conn;
  (* Wait at stop : *)
  for is = 0 to LS.n ls - 1 do
    let s = LS.label ls is in
    let t_end = Array.length st_tms.(is) in
    if t_end > 0 then begin
      let t = ref st_tms.(is).(0) in
      for it = 1 to t_end - 1 do
        let t' = st_tms.(is).(it) in
        if t' > !t then
          EG.add eg (LST.index lst (s,!t)) (t' - !t) (LST.index lst (s,t'));
        t := t';
      done;
    end;
  done;
  (* Transfers : *)
  let trf_edges = Hashtbl.create (G.m transf) in
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
          let u = LST.index lst (s,t) and v = LST.index lst (s',t') in
          EG.add eg u (t' - t) v;
          Hashtbl.add trf_edges (u,v) ();
        with Not_found -> () (* no next connection *)
      done;
    done;
    Trav.clear dij;
  done;
  
  G.of_edges ~n_estim:(EG.n eg) (fun f -> EG.iter f eg), trf_edges
