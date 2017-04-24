

let () =
  Printexc.record_backtrace true ;
  try
    let module IA = GenArray.OfInt in
    let module LA = GenArray.OfInt in
    let module WA = GenArray.OfInt in
    let module W =
        struct
          type t = int
          let to_float = float_of_int
          let zero = 0
          let infinity = max_int
          let add = (+)
          let compare w w' = w - w'
        end
    in

    let t = ref (Sys.time ()) in
    let top msg =
      let t' = Sys.time () in
      Printf.eprintf "#  --   %s in %fs\n" msg (t' -. !t) ; flush stderr ;
      t := t'
    in
    
    let module E = EdgeArray.Make (IA) (WA) in
    let e = E.create () in

    let module LabS = LabelSet.Make (LA) in
    let lbl = LabS.create () in
    
    let lex = Lexing.from_channel stdin in
    begin try while true do
        match Ints.next lex, Ints.next lex, Ints.next lex with
          | Some u, Some v, Some w ->
            let u = LabS.add lbl u in let v = LabS.add lbl v in
            E.add e u w v
          | Some _, _, _ -> failwith "incomplete triple at the end"
          | _ -> raise Not_found
      done with Not_found -> () end ;
    let n = E.n e and m = E.m e in
    Printf.eprintf "# n=%d m=%d\n" n m ; flush stderr ;
    top "loaded" ;

    Random.init 1234 ;
    (*    
    let module B = Batteries in
    let perm_vtx = B.Enum.range 0 ~until:(n-1) |> B.Random.shuffle in
    let perm_edg = B.Enum.range 0 ~until:(m-1) |> B.Random.shuffle in
    top "perms" ;
    *)  

    let module G = IntGraph.Make (IA) (LA) (WA) in 
    let g = G.sorted_of_edges ~n_estim:n (fun f -> (* E.iter f e *)
      for i=0 to m-1 do
        let u,w,v = E.edge e i (*perm_edg.(i*) in
        assert (u < n && v < n) ;
        f u w v (*perm_vtx.(u) w perm_vtx.(v*)
      done
    ) in
    let n = G.n g and m = G.m g in
    
    (* ocamlgraph test : 
    top "Make adj" ;
    let module I = struct
      type t = int
      let compare i j = i - j
      let hash i = i
      let equal i j = i = j
      let default = 1
    end in
    let module G =
          Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (I) (I) in
    let g = G.create () in
    E.iter (fun u w v ->
      G.add_edge_e g (u, w, v)
    ) e ;
    G.add_edge_e g (0, 1, 0) ;
    let n = G.nb_vertex g and m = G.nb_edges g in *)

    Printf.eprintf "# n=%d m=%d\n" n m ;
    top "adj" ;

    Gc.compact () ;
    Printf.eprintf "# mem=%d\n"
      (8 * let s = Gc.quick_stat() in s.Gc.heap_words + s.Gc.stack_size) ;
    flush stderr ;
    top "compact" ;

    let lab = LabS.label lbl in
    let sum = ref 0 in
    G.iter (fun u w v ->
      (* Printf.printf "%d %d %d\n" (lab u) (lab v) w ; *)
      sum := !sum + w + v        
    ) g ;
    Printf.eprintf "# sum=%d\n" !sum;
    top "iter_edges" ;

    for u=0 to n-1 do
      G.iter_out_edges (fun e ->
        if u <> G.edge_src g e then begin
          Printf.eprintf " -------- u=%d ul=%d e=%d ----------\n"
            u (LabS.label lbl u) e ;
          for u=u-3 to u+1 do
            Printf.eprintf " --- u=%d ul=%d ---\n" u (LabS.label lbl u) ;
            G.iter_out_edges (fun e ->
              let u = G.edge_src g e in
              let v = G.edge_dst g e in
              Printf.eprintf "e=%d v=%d lv=%d   u=%d\n"
                e v (LabS.label lbl v) u ;
            ) g u ;
          done ;
          assert false ;
          end
      ) g u ;
    done ;
    top "edge_src";
    
    (*G.iter_edges_e (fun _ -> ()) g ;
    top "iter_edges" ;*)

    Printf.eprintf "# mem=%d\n"
      (8 * let s = Gc.quick_stat() in s.Gc.heap_words + s.Gc.stack_size) ;
    flush stderr ;
    top "mem";

    let count = ref 0 and test = 1000000 in
    for _=1 to test do
      let u = Random.int n in let v = Random.int n in
      if G.mem_edge g u v then begin
        incr count ;
        (* Printf.eprintf "# %d %d\n" u v ; *)
      end
    done ;
    Printf.eprintf "# nb edges found : %d / %d\n" !count test ; 
    top "mem_edge" ;

    let module Trav = Traversal.Make (G) (LA) (W) in
    let t = 100000
    and s = try LabS.index lbl (int_of_string Sys.argv.(1)) with _-> 0 in
    let dij = Trav.create n in
    top "dij create" ;
    
    let nq = Trav.dijkstra dij g [s] in
    let d = Trav.dist dij t in
    Printf.eprintf "# Dijkstra enqueues : %d d(%d, %d) = %d\n" nq
      (lab s) (lab t) d ; 
    top "dijkstra" ;

    for u = 0 to n - 1 do
      let p = Trav.parent dij u and w = Trav.parent_dist dij u in
      assert (Trav.dist dij u = w + Trav.dist dij p);
    done;
    G.iter (fun u w v ->
      assert (Trav.dist dij v <= w + Trav.dist dij u);
    ) g ;
    top "dij verif";

    let module Skel = Skeleton.Make (Trav) (W) in
    let sk = Skel.create n in
    Skel.of_traversal sk dij;
    Printf.eprintf "# Skeleton width = %d, integr width = %f, skel size = %d\n"
      (Skel.width sk) (Skel.integrated_width sk dij) (Skel.n sk); 
    top "skeleton" ;
    
    (* Comparison on New York graph (9th DIMACS) :
                   time_adj          iter_edg   mem_edge(10^6) dij    mem
    OcamlGraph   : 0.94s             0.034s     0.62s                 150753480b

    IGraph       : 0.11s(+0.54sload) 0.015s                    0.25s  26263768b
                                                     BatHeap : 0.41s
     BatDynArray : 0.17s(+0.55sload) 0.020s     0.25s
          sorted : 0.21s(+0.54sload) 0.015s     0.23s                 26263768b
 int32 big array : 0.69s(+0.48sload) 0.093s     0.42s                 twice less
   int32 & array : 0.51s(+0.48sload) 0.064s     0.29s

    rev_graph.cc : 0.25s (+2.03s)    0.005s     0.35s          0.071s

       With permutations :
    OcamlGraph   : 0.63s             0.038s     0.63s
    IGraph       : 0.44s(+0.53s)
     BatDynArray : 
          sorted : 0.61s(+0.53s)     0.016s     0.23s
 int32 big array : 
   int32 & array : 1.06s(+0.47sload) 0.063s     0.29s

       On France n=3866333 m=9084548 :
       
                 : load    adj     itedg     memedg    dijkstra
  OcamlGraph     :         23.4s   1.0s      0.5s      
  OcamlGraph Lbl :         19.3s   0.86s     0.86s  

  IGraph         : 17s     5.5s    0.22s     0.34s     4.7s
  int32          : 14s     9s      0.76s     0.40s     
  IGraph  perms  :         11.3s   0.21s     0.34s
  int32   perms  :         17.5s   0.81s     0.42s 

    rev_graph.cc : 40s     5.4s    0.10s     0.36s     1.59s

1 3 20
1 2 10
2 4 50
3 5 10
5 7 10
5 9 10
5 6 10
6 4 10

    G.iter (Printf.eprintf "%d %d %d\n") g ;
    Printf.eprintf "# mem_edge : \n" ;
    List.iter
      (fun (u, v) -> Printf.eprintf "%d %d : %b\n" u v (G.mem_edge g u v))
      [5,5; 5,6; 5,7; 5,8; 5,9; 5,10;];
    *)
    
    ()
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    flush stderr ;
    exit 2
