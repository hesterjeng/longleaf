module Array1 = Bigarray.Array1
module Array2 = Bigarray.Array2

type data_matrix = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t
type int_matrix = (int, Bigarray.int_elt, Bigarray.c_layout) Array2.t

type t = {
  float_indicators : data_matrix;
  int_indicators : int_matrix;
  indicator_map : (Tacaml.t, int) Hashtbl.t;
  indicator_reverse : (int, Tacaml.t) Hashtbl.t;
  mutable next_float_slot : int;
  mutable next_int_slot : int;
}

let make size =
  {
    float_indicators =
      Array2.init Bigarray.float64 Bigarray.c_layout 50 size (fun _ _ ->
          Float.nan);
    int_indicators =
      Array2.init Bigarray.int Bigarray.c_layout 30 size (fun _ _ -> 0);
    indicator_map = Hashtbl.create 64;
    indicator_reverse = Hashtbl.create 64;
    next_float_slot = 0;
    next_int_slot = 0;
  }

let copy (x : t) =
  let new_float_indicators =
    Array2.create Bigarray.float64 Bigarray.c_layout
      (Array2.dim1 x.float_indicators)
      (Array2.dim2 x.float_indicators)
  in
  let new_int_indicators =
    Array2.create Bigarray.int Bigarray.c_layout
      (Array2.dim1 x.int_indicators)
      (Array2.dim2 x.int_indicators)
  in
  Array2.blit x.float_indicators new_float_indicators;
  Array2.blit x.int_indicators new_int_indicators;
  {
    float_indicators = new_float_indicators;
    int_indicators = new_int_indicators;
    indicator_map = Hashtbl.copy x.indicator_map;
    indicator_reverse = Hashtbl.copy x.indicator_reverse;
    next_float_slot = x.next_float_slot;
    next_int_slot = x.next_int_slot;
  }

let register_indicator (custom : t) (indicator : Tacaml.t) :
    (int, Error.t) result =
  (* Check if already registered *)
  match Hashtbl.find_opt custom.indicator_map indicator with
  | Some slot -> Result.return slot
  | None -> (
    (* Determine if this is a float or int indicator *)
    let output_spec = Tacaml.output indicator in
    match output_spec with
    | Tacaml.Output.Flag.FloatBAFlag _
    | Tacaml.Output.Flag.FloatBA2Flag _
    | Tacaml.Output.Flag.FloatBA3Flag _ ->
      (* Float indicator *)
      if custom.next_float_slot >= Array2.dim1 custom.float_indicators then
        Error.fatal "No more custom float indicator slots available"
      else
        let slot = custom.next_float_slot in
        Hashtbl.add custom.indicator_map indicator slot;
        Hashtbl.add custom.indicator_reverse slot indicator;
        custom.next_float_slot <- slot + 1;
        Result.return slot
    | Tacaml.Output.Flag.IntBAFlag _
    | Tacaml.Output.Flag.IntBA2Flag _ ->
      (* Int indicator *)
      if custom.next_int_slot >= Array2.dim1 custom.int_indicators then
        Error.fatal "No more custom int indicator slots available"
      else
        let slot = custom.next_int_slot in
        Hashtbl.add custom.indicator_map indicator slot;
        Hashtbl.add custom.indicator_reverse slot indicator;
        custom.next_int_slot <- slot + 1;
        Result.return slot)

let get_slot (custom : t) (indicator : Tacaml.t) : (int, Error.t) result =
  match Hashtbl.find_opt custom.indicator_map indicator with
  | Some slot -> Result.return slot
  | None -> Error.fatal "Custom indicator not registered"

let get (custom : t) (indicator : Tacaml.t) (index : int) : float =
  match Hashtbl.find_opt custom.indicator_map indicator with
  | Some slot -> (
    match Tacaml.output indicator with
    | Tacaml.Output.Flag.FloatBAFlag _
    | Tacaml.Output.Flag.FloatBA2Flag _
    | Tacaml.Output.Flag.FloatBA3Flag _ ->
      Array2.get custom.float_indicators slot index
    | Tacaml.Output.Flag.IntBAFlag _
    | Tacaml.Output.Flag.IntBA2Flag _ ->
      Float.of_int @@ Array2.get custom.int_indicators slot index)
  | None -> invalid_arg "CustomTacaml indicator not registered"

let set (custom : t) (indicator : Tacaml.t) (index : int) (value : float) : unit
    =
  match Hashtbl.find_opt custom.indicator_map indicator with
  | Some slot -> (
    match Tacaml.output indicator with
    | Tacaml.Output.Flag.FloatBAFlag _
    | Tacaml.Output.Flag.FloatBA2Flag _
    | Tacaml.Output.Flag.FloatBA3Flag _ ->
      Array2.set custom.float_indicators slot index value
    | Tacaml.Output.Flag.IntBAFlag _
    | Tacaml.Output.Flag.IntBA2Flag _ ->
      Array2.set custom.int_indicators slot index (Int.of_float value))
  | None -> invalid_arg "CustomTacaml indicator not registered"
