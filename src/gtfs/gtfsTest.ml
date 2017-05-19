(* Laurent Viennot, Inria 2017 *)


let () =
  Printexc.record_backtrace true ;
  try

    let t = ref (Sys.time ()) in
    let top msg =
      let t' = Sys.time () in
      Printf.eprintf "#  --   %s in %fs\n" msg (t' -. !t) ; flush stderr ;
      t := t'
    in

    let gtfs_dir = Sys.argv.(1) in
    let day_date = int_of_string Sys.argv.(2) in
    let (conn, lst, transf, ls) as gtfs = Gtfs.to_graphs gtfs_dir day_date in
    top "gtfs read";
    
    let module G = Gtfs.G in
    let module EG = Gtfs.EG in
    let module LS = Gtfs.LS in
    let module LST = Gtfs.LST in
    let module R = Rows in
    let module Trav =
          Traversal.Make (G) (GenArray.OfInt) (Traversal.IntWeight) in
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
    
    let g = Gtfs.time_expanded_graph gtfs in
    Printf.eprintf "Time expanded graph : n = %d m = %d\n" (G.n g) (G.m g);
    top "time expanded graph";
    
    let dij = Trav.create (G.n g) in
    let arr = Array.make (LS.n ls) max_int in
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
    let filter _ _ _ v dt _ =
      incr nedg;
      let s, tv = LST.label lst v in
      let is = LS.index ls s in
      if dt > !tmax then tmax := dt;
      let follow = tv < arr.(is) in
      if follow then arr.(is) <- tv;
      follow
    in
    for _ = 1 to 10 do
      let src = Random.int (LST.n lst) in
      let s, t = LST.label lst src in
      let nvis = Trav.dijkstra dij g ~filter [src] in
      Printf.eprintf "    %s,%d : nvis = %d nedg = %d dt_max=%d\n"
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
      Printf.eprintf "    %s,%d : nvis = %d nedg = %d dt_max=%d\n"
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
    for _ = 1 to n_iter do
      let src =  Random.int (LST.n lst) and dst = Random.int (LST.n lst) in
      let path_sum = ref 0 and uniq_sum = ref 0 and n_path = ref 0 in
      let h_path = Hashtbl.create 16 and ht_path = Hashtbl.create 16 in
      let station_of u = LST.label lst u |> fst in
      let s = station_of src and d = station_of dst in
      let is = LS.index ls s and id = LS.index ls d in
      let edges = Hashtbl.create 15000 and lprof = LS.create () in
      for it = 0 to Array.length st_tms.(is) - 1 do
        let t = st_tms.(is).(it) in
        let ist = LST.index lst (s, t) in
        let _ = Trav.dijkstra dij g ~filter [ist] in
        (* Printf.eprintf "    %d,%d : nvis = %d nedg = %d dt_max=%d\n"
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
      Printf.eprintf "  %s->%s union n=%d m=%d, path_len: sum=%d avg=%d \
                      n_paths: unw=%d wgt=%d\n"
                     s d (G.n gprof) (G.m gprof)
                     !uniq_sum (!path_sum / (max 1 !n_path))
                     (Hashtbl.length h_path) (Hashtbl.length ht_path);
      flush stderr;
      all_path_nb := !all_path_nb + (Hashtbl.length h_path);
      all_wgt_path_nb := !all_wgt_path_nb + (Hashtbl.length ht_path);
    done;
    Printf.printf "avg_nb_path = %d  avg_nb_wgt_path = %d\n"
                  (!all_path_nb / n_iter) (!all_wgt_path_nb / n_iter);
    top "profile";
    
    ()
    
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ());
    flush stderr;
    exit 2
