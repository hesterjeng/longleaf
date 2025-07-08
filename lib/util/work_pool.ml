(* Work pool utility for parallel processing using Eio domains *)

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
  let parallel_map ~eio_env ?(num_domains = None) ?(log_performance = false) ~f
      items =
    let domain_mgr = Eio.Stdenv.domain_mgr eio_env in
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
      let start_time = Eio.Time.now (Eio.Stdenv.clock eio_env) in

      let chunk_size = max 1 (num_items / num_cores) in
      let chunks = chunks_of ~len:chunk_size items in

      let results =
        List.map
          (fun chunk ->
            Eio.Domain_manager.run domain_mgr (fun () -> List.map f chunk))
          chunks
      in

      let end_time = Eio.Time.now (Eio.Stdenv.clock eio_env) in

      if log_performance then
        Eio.traceln "Parallel processing: %d items, %d domains, %.3fs" num_items
          num_cores (end_time -. start_time);

      List.flatten results

  let parallel_filter_map ~eio_env ?(num_domains = None)
      ?(log_performance = false) ~f items =
    let domain_mgr = Eio.Stdenv.domain_mgr eio_env in
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
      let start_time = Eio.Time.now (Eio.Stdenv.clock eio_env) in

      let chunk_size = max 1 (num_items / num_cores) in
      let chunks = chunks_of ~len:chunk_size items in

      let results =
        List.map
          (fun chunk ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                List.filter_map f chunk))
          chunks
      in

      let end_time = Eio.Time.now (Eio.Stdenv.clock eio_env) in

      if log_performance then
        Eio.traceln "Parallel processing: %d items, %d domains, %.3fs" num_items
          num_cores (end_time -. start_time);

      List.flatten results

  (* For cases where you need to collect results and errors *)
  let parallel_map_result ~eio_env ?(num_domains = None)
      ?(log_performance = false) ~f items =
    let domain_mgr = Eio.Stdenv.domain_mgr eio_env in
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
      let start_time = Eio.Time.now (Eio.Stdenv.clock eio_env) in

      let chunk_size = max 1 (num_items / num_cores) in
      let chunks = chunks_of ~len:chunk_size items in

      let results =
        List.map
          (fun chunk ->
            Eio.Domain_manager.run domain_mgr (fun () ->
                List.map
                  (fun item ->
                    try Ok (f item) with
                    | exn -> Error exn)
                  chunk))
          chunks
      in

      let end_time = Eio.Time.now (Eio.Stdenv.clock eio_env) in

      if log_performance then
        Eio.traceln "Parallel processing: %d items, %d domains, %.3fs" num_items
          num_cores (end_time -. start_time);

      List.flatten results
end
