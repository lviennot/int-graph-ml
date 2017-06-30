(* Laurent Viennot, Inria 2017 *)

exception No_trip

let () =
  Printexc.record_backtrace true ;
  try

    let t = ref (Sys.time ()) in
    let top msg =
      let t' = Sys.time () in
      Printf.eprintf "#  --   %s in %fs\n" msg (t' -. !t) ; flush stderr ;
      t := t'
    in

    let next_arg = let i = ref 0 in fun () -> incr i; Sys.argv.(!i) in
    let gtfs_dir = next_arg () in
    let day_from = int_of_string (next_arg ()) in
    let t_from = Rows.time_of_string (next_arg ()) in
    let day_to = int_of_string (next_arg ()) in
    let t_to = Rows.time_of_string (next_arg ()) in
    
    let module G = Gtfs.G in
    let module EG = Gtfs.EG in
    let module LS = Gtfs.LS in
    let module LST = Gtfs.LST in
    let module R = Rows in
    let module Trav =
          Traversal.Make (G) (GenArray.OfInt) (Traversal.IntWeight) in

    let gtfs = Gtfs.of_dir gtfs_dir day_from t_from day_to t_to in

    (* Get stop sequence for a list of routes : *)
    let trips_of = Hashtbl.create (Hashtbl.length gtfs.Gtfs.routes) in
    Hashtbl.iter (fun trp (rid, _, _) ->
      let l = try Hashtbl.find trips_of rid with Not_found -> [] in
      Hashtbl.replace trips_of rid (trp :: l);
    ) gtfs.Gtfs.trips;
    let liste_rid =
      let l = ref [] in
      try
        let f = open_in (next_arg ()) in
        while true do
          l := (input_line f) :: !l
        done;
        !l
      with
      | End_of_file -> !l
      | Invalid_argument _ ->
         Hashtbl.iter (fun rid _ -> l := rid :: !l) gtfs.Gtfs.routes;
         !l
    in
    List.iter (fun rid ->
      try
        let short, long, _, _, _ = Hashtbl.find gtfs.Gtfs.routes rid in
        Printf.eprintf "Route %s (%s, %s)\n" rid short long; flush stderr;
        let trips =
          try Hashtbl.find trips_of rid
          with Not_found -> raise No_trip in
        let ls = LS.create () and eg = EG.create () in
        List.iter (fun trp ->
            let stops = Hashtbl.find gtfs.Gtfs.stop_times trp in
            let r, _, dir = Hashtbl.find gtfs.Gtfs.trips trp in
            assert (r = rid);
            let stops =
              if dir <> 0 then [] else stops in
            let stops = List.map (fun (_,_,s,_,_) -> s) stops in
            let rec iter = function
              | s :: s' :: stops ->
                if s = s' then begin
                     Printf.eprintf "double_stop: %s %s\n" rid s;
                   end;
                 (* assert (s <> s'); *)
                 if s <> s' then
                   EG.add eg (LS.add ls s) 1 (LS.add ls s')
              | _ -> ()
            in
            iter stops;
          ) trips;
        (* topo sort *)
        let g = G.of_edges ~n_estim:(LS.n ls) (fun f -> EG.iter f eg) in
        let trav = Trav.create (G.n g) in
        let ext = Trav.topological_ordering trav g in
        let ord = Array.make (G.n g) (-1) in
        for u = 0 to G.n g - 1 do ord.(ext.(u)) <- u done;
        for i = 0 to G.n g - 1 do
          Printf.printf "%s,%d,%s\n" rid i (LS.label ls ord.(i));
        done;
      with
      | Trav.Cycle _ -> Printf.eprintf "cycle: %s\n" rid;
      | Not_found -> Printf.eprintf "not_found: %s\n" rid;
      | No_trip -> Printf.eprintf "no_trip: %s\n" rid;
      ) liste_rid;
    
    exit 0;
    

    let (conn, lst, transf, ls, edge_trip, trip_route) as gtfs_graphs =
      Gtfs.to_graphs gtfs in
    top "gtfs read";

    (*let svtx u =
      let s,t = LST.label lst u in
      Printf.sprintf "%d = %d,%s" u s (R.cell_to_string (R.Time t))
    in
     List.iter (fun u -> Printf.eprintf "%s\n" (svtx u)) [1430;]; *)
    (* let ord = Gtfs.time_ordering conn lst in *)
    let trav = Trav.create (G.n transf) in
    let ccs = Trav.connected_components trav transf in
    let ccs = List.rev_map List.length ccs in
    Printf.eprintf "%d transfer components, sizes: min=%d avg=%d max=%d\n"
                   (List.length ccs) (List.fold_left min max_int ccs)
                   (List.fold_left (+) 0 ccs / List.length ccs)
      (List.fold_left max 0 ccs);
    top "transfer connected components";
    
    let g, trf_edges = Gtfs.time_expanded_graph gtfs_graphs in
    Printf.eprintf "Time expanded graph : n = %d m = %d\n" (G.n g) (G.m g);
    top "time expanded graph";

    let dij = Trav.create (G.n g) in
    let arr = Array.make (LS.n ls) max_int in
    let by_trp = Array.make (LS.n ls) "" in
    let nedg = ref 0 and tmax = ref 0 in
    let clear () =
      let nvis = Trav.visit_nb dij in
      for vt = 0 to nvis - 1 do
        let u = Trav.visit_at dij vt in
        let s, _ = LST.label lst u in
        arr.(LS.index ls s) <- max_int;
      done;
      nedg := 0; tmax := 0;
      Trav.clear dij;
    in
    let filter u _ _ v dt _ =
      incr nedg;
      let trp = try edge_trip (u, v) with Not_found -> "" in
      let (* su, tu = LST.label lst v and *) sv, tv = LST.label lst v in
      let (* iu = LS.index ls su and *) iv = LS.index ls sv in
      let follow =
        tv < arr.(iv)
        (*&& (trp = "" || by_trp.(iu) = "" || trp = by_trp.(iu)
            || tv >= tu + 1)*)
      in
      if follow then
        (arr.(iv) <- tv; by_trp.(iv) <- trp; if dt > !tmax then tmax := dt);
      follow
    in
    for _ = 1 to 10 do
      let src = Random.int (LST.n lst) in
      let s, t = LST.label lst src in
      let nvis = Trav.dijkstra dij g ~filter [src] in
      Printf.eprintf "    %s,%d : nvis = %d  nedg = %d  tmax=%d\n"
        s t nvis !nedg !tmax;
      clear ();
    done;
    top "temporal dijkstra";

    let module Skel = Skeleton.Make (Trav) (Skeleton.IntWeight) in
    let skel = Skel.create (G.n g) in
    for _ = 1 to 10 do
      let src = Random.int (LST.n lst) in
      let s, t = LST.label lst src in
      let nvis = Trav.dijkstra dij g ~filter [src] in
      Printf.eprintf "    %s,%d : nvis = %d  nedg = %d  tmax = %d\n"
        s t nvis !nedg !tmax;
      Skel.of_traversal skel ~alpha:0.5 ~edge_metric:(fun _ _ _ -> 1.) dij;
      Printf.eprintf "Skeleton width = %d, integr width = %f, skel size = %d\n"
        (Skel.width skel) (Skel.integrated_width skel dij) (Skel.n skel); 
      clear ();
    done;
    top "skeleton";

    let n_iter = 10 in
    let st_tms = Gtfs.stop_times lst ls in
    let all_path_nb = ref 0 and all_wgt_path_nb = ref 0 in
    let all_trf_pat_nb = ref 0 in
    for _ = 1 to n_iter do
      let src =  Random.int (LST.n lst) and dst = Random.int (LST.n lst) in
      let path_sum = ref 0 and uniq_sum = ref 0 and n_path = ref 0 in
      let h_path = Hashtbl.create 16 and ht_path = Hashtbl.create 16 in
      let trf_pat = Hashtbl.create 16 and trf_pat_sum = ref 0 in
      let station_of u = LST.label lst u |> fst in
      let rec transfers acc epath =
        match epath with
        | [] -> acc
        | (u, _, v) :: epath ->
           let acc =
             if Hashtbl.mem trf_edges (u, v) then
               let s = station_of u and s' = station_of v in (s, s') :: acc
             else acc
           in
           transfers acc epath
      in
      let s = station_of src and d = station_of dst in
      let is = LS.index ls s and id = LS.index ls d in
      let edges = Hashtbl.create 15000 and lprof = LS.create () in
      for it = 0 to Array.length st_tms.(is) - 1 do
        let t = st_tms.(is).(it) in
        let ist = LST.index lst (s, t) in
        let _ = Trav.dijkstra dij g ~filter [ist] in
        (* Printf.eprintf "    %s,%d : nvis = %d nedg = %d dt_max=%d\n"
          s t nvis !nedg !tmax; *)
        if arr.(id) <> max_int then begin
          let du = LST.index lst (d, arr.(id)) in
          (* Gtfs.next_stop_time ls st_tms d (t + arr.(id)) in *)
          let path = Trav.path dij du in
          let path = List.map station_of path in
          if not (Hashtbl.mem h_path path) then
            uniq_sum := !uniq_sum + List.length path - 1;
          Hashtbl.replace h_path path ();
          let path = Trav.edge_path dij du in
          (* print some paths : *)
          (* if s = "StopPoint:59:5231702" && d = "StopPoint:59:4208562" then begin *)
          if s = "3893342" && d = "4687350" then begin
              Printf.eprintf "  >";
              let t = ref 0 in
              List.iter (fun (u, dt, v) ->
                  t := !t + dt;
                  try
                    let _, short, _, _, _ =
                      trip_route (edge_trip (u,v)) in
                    Printf.eprintf " %s" short;
                  with Not_found ->
                    if Hashtbl.mem trf_edges (u, v) then Printf.eprintf " -"
                    else Printf.eprintf " .";
                ) path;
              Printf.eprintf " (%ds)\n" !t;
            end;
          (* transfer pattern : *)
          let pat = transfers [] path in
          Hashtbl.replace trf_pat pat ();
          trf_pat_sum := !trf_pat_sum + (List.length pat);
          let path =
            List.map (fun (u, w, v) -> station_of u, w, station_of v) path in
          path_sum := !path_sum + List.length path;
          incr n_path;
          Hashtbl.replace ht_path path ();
          (* Add s -- t path to station graph : *)
          List.iter (fun (s, dt, s') ->
              let s = LS.add lprof s and s' = LS.add lprof s' in
              if not (Hashtbl.mem edges (s, s')) then
                Hashtbl.add edges (s, s') dt;
            ) path;
          end;
        clear ();
      done;
      let gprof = G.of_edges ~n_estim:(Hashtbl.length edges)
          (fun f -> Hashtbl.iter (fun (s, s') dt -> f s dt s') edges) in 
      Printf.eprintf
        "  %s->%s union n=%d m=%d, path_len: sum=%d avg=%d pat=%d \
            n_paths: unw=%d wgt=%d trfpat=%d n=%d\n"
        s d (G.n gprof) (G.m gprof)
        !uniq_sum (!path_sum / max 1 !n_path) (!trf_pat_sum / max 1 !n_path)
        (Hashtbl.length h_path) (Hashtbl.length ht_path)
        (Hashtbl.length trf_pat) (!n_path);
      flush stderr;
      if !n_path > 0 then begin
          all_path_nb := !all_path_nb + (Hashtbl.length h_path);
          all_wgt_path_nb := !all_wgt_path_nb + (Hashtbl.length ht_path);
          all_trf_pat_nb := !all_trf_pat_nb + (Hashtbl.length trf_pat);
        end;
    done;
    Printf.printf
      "avg_nb_path = %d  avg_nb_wgt_path = %d avg_nb_trf_pat = %d\n"
      (!all_path_nb / n_iter) (!all_wgt_path_nb / n_iter)
      (!all_trf_pat_nb / n_iter);
    top "profile";
    
    ()
    
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ());
    flush stderr;
    exit 2
