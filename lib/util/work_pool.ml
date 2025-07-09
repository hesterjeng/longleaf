(* Work pool utility for parallel processing using Eio.Executor_pool *)

(* Helper function to chunk a list into smaller lists *)
let chunks_of ~len lst =
  let rec aux acc current_chunk current_len remaining =
    match remaining with
    | [] ->
      if List.is_empty current_chunk then List.rev acc
      else List.rev (List.rev current_chunk :: acc)
    | x :: xs ->
      if current_len >= len then aux (List.rev current_chunk :: acc) [ x ] 1 xs
      else aux acc (x :: current_chunk) (current_len + 1) xs
  in
  aux [] [] 0 lst

(* Main work pool module *)
module Work_pool = struct
  let parallel_map ~pool ~clock ?(num_domains = None) ?(log_performance = false) ~f
      items =
    let num_cores =
      match num_domains with
      | Some n -> n
      | None -> max 1 (Domain.recommended_domain_count () - 1)
    in

    let num_items = List.length items in

    (* Fallback to sequential for small workloads *)
    if num_items <= 10 || num_cores <= 1 then (
      if log_performance then
        Eio.traceln "Using sequential processing for %d items" num_items;
      List.map f items)
    else
      let start_time = Eio.Time.now clock in

      let chunk_size = max 1 (num_items / num_cores) in
      let chunks = chunks_of ~len:chunk_size items in

      (* Process all chunks in parallel using Eio.Executor_pool *)
      let results = 
        Eio.Switch.run (fun sw ->
          let promises = List.map (fun chunk ->
            Eio.Executor_pool.submit_fork ~sw pool ~weight:0.8 (fun () -> List.map f chunk)
          ) chunks in
          List.map (fun promise -> 
            match Eio.Promise.await promise with
            | Ok result -> result
            | Error exn -> raise exn
          ) promises
        )
      in

      let end_time = Eio.Time.now clock in

      if log_performance then
        Eio.traceln "Parallel processing: %d items, %d domains, %.3fs" num_items
          num_cores (end_time -. start_time);

      List.flatten results

  let parallel_filter_map ~pool ~clock ?(num_domains = None)
      ?(log_performance = false) ~f items =
    let num_cores =
      match num_domains with
      | Some n -> n
      | None -> max 1 (Domain.recommended_domain_count () - 1)
    in

    let num_items = List.length items in

    if num_items <= 10 || num_cores <= 1 then (
      if log_performance then
        Eio.traceln "Using sequential processing for %d items" num_items;
      List.filter_map f items)
    else
      let start_time = Eio.Time.now clock in

      let chunk_size = max 1 (num_items / num_cores) in
      let chunks = chunks_of ~len:chunk_size items in

      (* Process all chunks in parallel using Eio.Executor_pool *)
      let results = 
        Eio.Switch.run (fun sw ->
          let promises = List.map (fun chunk ->
            Eio.Executor_pool.submit_fork ~sw pool ~weight:0.8 (fun () -> List.filter_map f chunk)
          ) chunks in
          List.map (fun promise -> 
            match Eio.Promise.await promise with
            | Ok result -> result
            | Error exn -> raise exn
          ) promises
        )
      in

      let end_time = Eio.Time.now clock in

      if log_performance then
        Eio.traceln "Parallel processing: %d items, %d domains, %.3fs" num_items
          num_cores (end_time -. start_time);

      List.flatten results

  (* For cases where you need to collect results and errors *)
  let parallel_map_result ~pool ~clock ?(num_domains = None)
      ?(log_performance = false) ~f items =
    let num_cores =
      match num_domains with
      | Some n -> n
      | None -> max 1 (Domain.recommended_domain_count () - 1)
    in

    let num_items = List.length items in

    if num_items <= 10 || num_cores <= 1 then (
      if log_performance then
        Eio.traceln "Using sequential processing for %d items" num_items;
      List.map
        (fun item ->
          try Ok (f item) with
          | exn -> Error exn)
        items)
    else
      let start_time = Eio.Time.now clock in

      let chunk_size = max 1 (num_items / num_cores) in
      let chunks = chunks_of ~len:chunk_size items in

      (* Process all chunks in parallel using Eio.Executor_pool *)
      let results = 
        Eio.Switch.run (fun sw ->
          let promises = List.map (fun chunk ->
            Eio.Executor_pool.submit_fork ~sw pool ~weight:0.8 (fun () ->
              List.map
                (fun item ->
                  try Ok (f item) with
                  | exn -> Error exn)
                chunk)
          ) chunks in
          List.map (fun promise -> 
            match Eio.Promise.await promise with
            | Ok result -> result
            | Error exn -> raise exn
          ) promises
        )
      in

      let end_time = Eio.Time.now clock in

      if log_performance then
        Eio.traceln "Parallel processing: %d items, %d domains, %.3fs" num_items
          num_cores (end_time -. start_time);

      List.flatten results
end
