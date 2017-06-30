(* Laurent Viennot, Inria 2017 *)

(** Construct graphs from General Transit Feed Specification (GTFS) files.
    See https://developers.google.com/transit/gtfs/reference/ *)

module R = Rows
module Lab = LabelSet.Make(GenArray.MakeOf(struct type t = string end))

module Day = struct

  type t = int * int * int (* year, month, day, ex : 2017, 12, 31 *)
  
  let of_int i = (* format : yyyymmdd, ex : 20171231 *)
    let y = i / 10000 and i = i mod 10000 in
    let m = i / 100 and d = i mod 100 in
    y, m, d

  let to_int (y, m, d) =
    y * 10000 + m * 100 + d

  let months = Array.of_list [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]
            
  let year = Array.fold_left (+) 0 months

  let next (y, m, d) =
    if d < months.(m - 1) then y, m, d + 1 else begin
        let d' = 1 in
        let m', carry = if m = 12 then 1, true else m + 1, false in
        let y' = if carry then y + 1 else y in
        y', m', d'
      end

  let prev (y, m, d) =
    if d > 1 then y, m, d - 1 else begin
        let m', carry = if m = 1 then 12, true else m - 1, false in
        let d' = months.(m' - 1) in
        let y' = if carry then y - 1 else y in
        y', m', d'
      end

  let compare (y, m, d) (y', m', d') =
    if y = y' && m = m' && d = d' then 0
    else if y < y' || (y = y' && m < m') || (y = y' && m = m' && d < d') then -1
    else 1

  let max d d' = if compare d d' >= 0 then d else d'

  let min d d' = if compare d d' <= 0 then d else d'

  let bisextile y = if y mod 4 = 0 then 1 else 0
               
  let days_from_2001 (y, m, d) =
    assert (y >= 2001);
    let nd = ref 0 in
    for y' = 2001 to y-1 do
      nd := !nd + year + bisextile y'
    done;
    for m' = 1 to m - 1 do nd := !nd + months.(m'-1) done;
    if y mod 4 = 0 && m > 2 then incr nd;
    !nd + (d - 1)
               
  let week_day d = (* returns 0 for monday, 1 for tuesday, ... *)
    let wd_20010101 = 0 in
    (wd_20010101 + days_from_2001 d) mod 7

  let day_secs = 24 * 60 * 60

  type time = int (* in seconds from start of 2001 *)
               
  let time d sec = (days_from_2001 d) * day_secs + sec

  let of_time t =
    let nd = ref (t / day_secs) and y = ref 2001 in
    while !nd >= year + bisextile !y do
      nd := !nd - (year + bisextile !y);
      incr y;
    done;
    let m = ref 1 in
    while !nd >= months.(!m - 1) + if !m = 2 then bisextile !y else 0 do
      nd := !nd - (months.(!m - 1) + if !m = 2 then bisextile !y else 0);
      incr m;
    done;
    !y, !m, !nd + 1

  type hms = int * int * int (* hours, minutes, seconds *)
    
  let hms_of_time t =
    let s = t mod day_secs in
    let h = s / 3600 and s = s mod 3600 in
    let m = s / 60 and s = s mod 60 in
    h, m, s

  type dtime = int
    
  let dtime_of_hms (h, m, s) = h * 3600 + m * 60 + s

  let to_string t =
    let h, m, s = hms_of_time t in
    let y, m, d = of_time t in
    Printf.sprintf "%04d%02d%02d:%02d:%02d:%02d" y m d h m s
                 
end

type col_type = Ident | String | Int | IntDefault of int | Float | Time

let string_of_col_type = function
  | Ident -> "Ident" | String -> "String"
  | Int -> "Int" | IntDefault dft -> Printf.sprintf "IntDefault(%d)" dft
  | Float -> "Float" | Time -> "Time"

let row_select first_row sel =
  let cells = Array.make (List.length sel) R.Empty in
  let to_sel = Array.make (List.length first_row) (-1, "", Ident) in
  let default = ref [] in
  let id c = match c with R.Ident s -> s | _ -> assert false in
  let cols = List.mapi (fun i c -> id c, i) first_row in
  List.iteri (fun i (typ, c) ->
      try to_sel.(List.assoc c cols) <- i, c, typ
      with Not_found ->
        match typ with
        | IntDefault dft -> default := (i, R.Int dft) :: !default
        | _ -> failwith (Printf.sprintf "missing field : %s" c)
    ) sel;
  let convert col typ cell =
    match typ, cell with
    | Ident, R.Ident _ -> cell
    | Ident, R.Int i -> R.Ident (string_of_int i)
    | Ident, R.Float f -> R.Ident (string_of_float f)
    | String, R.String _ -> cell
    | String, R.Ident s -> R.String s
    | String, R.Int i -> R.String (string_of_int i)
    | String, R.Float f -> R.String (string_of_float f)
    | Int, R.Int _ -> cell
    | Int, R.Empty -> R.Int 0
    | IntDefault _, R.Int _ -> cell
    | Float, R.Float _ -> cell
    | Time, R.Time _ -> cell
    | _ -> failwith (Printf.sprintf "Gtfs: bad %s cell (%s) : %s"
                       col (string_of_col_type typ) (R.cell_to_string cell))
  in
  fun row ->
  let n = ref 0 in
  List.iteri (fun i cell ->
      let j, col, typ = to_sel.(i) in
      if j >= 0 then begin
          incr n;
          cells.(j) <- convert col typ cell
        end
    ) row;
  List.iter (fun (j, dft) ->
      incr n;
      cells.(j) <- dft
    ) !default;
  if !n <> Array.length cells then failwith "Gtfs: Wrong number of cells.";
  Array.to_list cells

type id = string
  
type t = {
    services : (id, Day.t list (* days of activity *)) Hashtbl.t;
    routes : (id, string * string * int * string * string
             (* short_name, long_name, type, color, text_color *)) Hashtbl.t;
    trips : (id, id * id * int (* route, service, direction *)) Hashtbl.t;
    stops : (id, string * float * float (* name, lat, lon *)) Hashtbl.t;
    transfers : (id * id, int * int (* from, to -> type, min_time *)) Hashtbl.t;
    stop_times : (id, (Day.dtime * int * id * Day.dtime * int) list
               (* trip_id -> [arr, drop, stop_id, dep, pick; ...] *)) Hashtbl.t;
  }

(** Read GTFS information from files in [gtfs_Dir] ([calendar.txt, routes.txt,
 trips.txt, ...]) from day [day_from] at time [t_from] to [day_to] at [t_to]
 (example of day,time format: 20170611,07:00:00). *)
let of_dir gtfs_dir day_from t_from day_to t_to =

  let wrong_row r = R.fprint_row stderr r; failwith "wrong row" in

  let day_from = Day.of_int day_from and day_to = Day.of_int day_to in
  
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
  (* week day computation *)
  let wday = Day.week_day day_from in
  Printf.eprintf "week day from : %d (0 for monday, 1 for tuesday, ...)\n" wday;
  (* rows : *)
  List.iter (fun r ->
    match read_row r with
      | [R.Ident srv;
         R.Int mon; R.Int tue; R.Int wed; R.Int thu;
         R.Int fri; R.Int sat; R.Int sun;
         R.Int d_start; R.Int d_end;] ->
         let week = Array.of_list [mon; tue; wed; thu; fri; sat; sun] in
         let d_start = Day.of_int d_start and d_end = Day.of_int d_end in
         let d_start = Day.max d_start day_from
         and d_end = Day.min d_end day_to in
         if Day.compare d_start d_end <= 0 then begin
             let days = ref [] and d = ref d_end in
             while Day.compare !d d_start >= 0 do
               if week.(Day.week_day !d) = 1 then days := !d :: !days;
               d := Day.prev !d;
             done;
             Hashtbl.add services srv !days;
           end
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
       let d = Day.of_int d in
       if Day.compare d day_from >= 0 && Day.compare d day_to <= 0 then begin
           let days = try Hashtbl.find services srv with Not_found -> [] in
           let days =
             if exc = 1 then d :: days
             else if exc = 2
             then List.filter (fun d' -> Day.compare d d' <> 0) days
             else failwith "Bad format: exception not in {0,1}."
           in
           Hashtbl.replace services srv days
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

  let routes = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "routes.txt" in
  let read_row = row_select cols [Ident, "route_id";
                                  String, "route_short_name";
                                  String, "route_long_name";
                                  Int, "route_type";
                                  String, "route_color";
                                  String, "route_text_color";] in
  List.iter (fun r ->
      match read_row r with
      | [R.Ident rid; R.String short_name; R.String long_name;
         R.Int typ; R.String color; R.String text_color;] ->
         Hashtbl.add routes rid (short_name, long_name, typ, color, text_color);
      | r -> wrong_row r
    ) rows;
  Printf.eprintf "%d routes\n" (Hashtbl.length routes);
  flush stderr;

  let stops = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "stops.txt" in
  let read_row = row_select cols [Ident, "stop_id";
                                  String, "stop_name";
                                  Float, "stop_lat";
                                  Float, "stop_lon";] in
  List.iter (fun r ->
      match read_row r with
        | [R.Ident s; R.String name; R.Float lat; R.Float lon] ->
          Hashtbl.add stops s (name, lat, lon)
        | r -> wrong_row r
    ) rows;
  Printf.eprintf "%d stops\n" (Hashtbl.length stops);
  flush stderr;
  
  let transfers = Hashtbl.create 16 in
  let cols, rows = R.read gtfs_dir "transfers.txt" in
  let read_row = row_select cols [Ident, "from_stop_id";
                                  Ident, "to_stop_id";
                                  Int, "transfer_type";
                                  Int, "min_transfer_time";] in
  List.iter (fun r ->
    match read_row r with
    | [R.Ident s; R.Ident s'; R.Int typ; R.Int tm;] ->
       let tm = if typ = 1 then 0 else tm in
       let tm = if typ = 3 then 365 * 24 * 60 * 60 else tm in
       Hashtbl.add transfers (s, s') (typ, tm);
    | r -> wrong_row r
    ) rows;
  Printf.eprintf "%d transfers\n" (Hashtbl.length transfers);
  flush stderr;
  
  let trip_stops = Hashtbl.create 16 in
  let nrows = ref 0 in
  let t_from = Day.time day_from t_from and t_to = Day.time day_to t_to in
  let cols, rows = R.read gtfs_dir "stop_times.txt" in
  let read_row = row_select cols [Ident, "trip_id";
                                  Time, "arrival_time";
                                  Time, "departure_time";
                                  Ident, "stop_id";
                                  Int, "stop_sequence";
                                  IntDefault 0, "pickup_type";
                                  IntDefault 0, "drop_off_type";] in
  List.iter (fun r ->
      match read_row r with
      | [R.Ident trp; R.Time arr; R.Time dep; R.Ident stp; R.Int seq;
         R.Int pick; R.Int drop] ->
         incr nrows;
         begin try
           let _, srv, _ = Hashtbl.find trips trp in
           let days = Hashtbl.find services srv in
           let days = List.map (fun d -> Day.to_int d) days in
           let d_min = List.fold_left min 30000101 days in
           let d_max = List.fold_left max 20010101 days in
           let t_arr = Day.time (Day.of_int d_min) arr
           and t_dep = Day.time (Day.of_int d_max) dep in
           if t_dep >= t_from && t_arr <= t_to then
             Hashtbl.add trip_stops trp (seq, arr, drop, stp, dep, pick);
         with Not_found -> () end
      | r -> wrong_row r
    ) rows;
  Printf.eprintf "%d/%d stop times\n" (Hashtbl.length trip_stops) !nrows;
  flush stderr;

  let stop_times = Hashtbl.create 16 in
  let nc = ref 0 in
  Hashtbl.iter (fun trp _ ->
      let stops = Hashtbl.find_all trip_stops trp in
      let stops =
        List.sort (fun (s,_,_,_,_,_) (s',_,_,_,_,_) -> s-s') stops in
      let rec rmseqs acc prev stops =
        match stops with
        | [] -> List.rev acc
        | (s,arr,drp,stp,dep,pck) as cur :: stops ->
           assert (match prev with Some (s',_,_,_,_,_) ->
             if s' <> s-1 then begin
                 List.iter (fun (s,arr,drp,stp,dep,pck) ->
                     Printf.eprintf "%d,%d,%d,%s,%s,%d\n"
                                    s arr drp stp (Day.to_string dep) pck;
                   ) stops;
                 failwith (Printf.sprintf
                             "bad seq : %s,%d,%d" trp s' s);
               end;
             s'=s-1 | _-> true);
           rmseqs ((arr,drp,stp,dep,pck) :: acc) (Some cur) stops
      in
      Hashtbl.add stop_times trp (rmseqs [] None stops);
      nc := !nc + List.length stops - 1;
  ) trips;
  Printf.eprintf "%d connections\n" !nc;
  flush stderr;

  { services; routes; trips; stops; transfers; stop_times; }
  
  
module StopArray = GenArray.MakeOf(struct type t = string end)
module LS = LabelSet.Make (StopArray) (* stops *)
module StopTimeArray = GenArray.MakeOf(struct type t = string * int end)
module LST = LabelSet.Make (StopTimeArray) (* stop,time pairs *)
module EG = EdgeArray.Make (GenArray.OfInt) (GenArray.OfInt)
module G = IntGraph.IntWeighted

  
let to_graphs gtfs =

  let trf_ls = LS.create () in
  Hashtbl.iter (fun stp _ -> ignore (LS.add trf_ls stp)) gtfs.stops;

  let trf_max = ref 0 and conn_max = ref 0 in
  let trf_eg = EG.create () in 
  Hashtbl.iter (fun (s, s') (typ, tm) ->
    if typ <> 3 then begin
      let s = LS.add trf_ls s and s' = LS.add trf_ls s' in
      EG.add trf_eg s tm s';
      EG.add trf_eg s' tm s;
      if tm > !trf_max then trf_max := tm
    end
  ) gtfs.transfers;
  
  let eg = EG.create () and lst = LST.create () in
  let trp_of_conn = Hashtbl.create 100000 in
  Hashtbl.iter (fun trp stops ->
   let _, srv, _ = Hashtbl.find gtfs.trips trp in
   let days = Hashtbl.find gtfs.services srv in
   let rec iter d = function
     | (arr, drp, stp, dep, pck) :: (arr', drp', stp', dep', pck' as s')
       :: stops ->
        assert (dep <= arr'); (* sometimes equal *)
        let arr' = Day.time d arr' and dep = Day.time d dep in
        let u = LST.add lst (stp, dep)
        and v = LST.add lst (stp', arr') in
        assert (drp <> 1 && pck <> 1 && drp' <> 1 && pck' <> 1);
        EG.add eg u (arr' - dep) v;
        if arr' - dep > !conn_max then conn_max := arr' - dep;
        Hashtbl.add trp_of_conn (u,v) trp;
        iter d (s' :: stops)
     | _ -> ()
   in 
   List.iter (fun d -> iter d stops) days;
  ) gtfs.stop_times;
  Printf.eprintf "connection graph: n = %d  m = %d, trf_max=%d conn_max=%d\n"
    (LST.n lst) (EG.m eg) !trf_max !conn_max;
  flush stderr;

  let route_of_trip trp =
    let rte, srv, dir = Hashtbl.find gtfs.trips trp in
    let short, long, _, _, _ = Hashtbl.find gtfs.routes rte in
    rte, short, long, srv, dir
  in
  
  let igraph eg n =
    G.of_edges ~n_estim:n (fun f -> EG.iter f eg) in

  (igraph eg (LST.n lst)), lst, (igraph trf_eg (LS.n trf_ls)), trf_ls,
  (Hashtbl.find trp_of_conn), route_of_trip

  
module Trav = Traversal.Make (G) (GenArray.OfInt) (Traversal.IntWeight)
            
let time_ordering conn lst =
  let n = G.n conn in
  let trv = Trav.create n in
  let ord = Trav.topological_ordering trv conn in
  let ext = Array.make n (-1) in
  for i = 0 to n - 1 do ext.(ord.(i)) <- i done;
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
              
let time_expanded_graph (conn, lst, transf, ls, _, _) =

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
  
  G.of_edges ~n_estim:(LST.n lst) (fun f -> EG.iter f eg), trf_edges
