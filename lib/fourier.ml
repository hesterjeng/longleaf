open Owl
module Genarray = Dense.Ndarray.Generic

module Complex = struct
  include Complex

  let pp : t Format.printer =
   fun fmt x -> Format.fprintf fmt "(%f, %f)" x.re x.im

  let norm_complex x : t =
    let re = norm x in
    { re; im = 0.0 }

  let square x = Complex.mul x x
end

type t = (Complex.t, Bigarray.complex64_elt) Genarray.t

let pp : t Format.printer =
 fun fmt _ -> Format.fprintf fmt "<fourier transform>"

let empty : t = Dense.Ndarray.Generic.empty Bigarray.complex64 [||]

let mean_squared_error (config : Indicator_config.t) (fft1 : t) (fft2 : t) =
  if not config.fft then 0.0
  else
    let l1, l2 = (Genarray.shape fft1, Genarray.shape fft2) in
    if
      match (Array.get_safe l1 0, Array.get_safe l2 0) with
      | Some i, Some j -> i < 3 || j < 3
      | _ -> true
    then 0.0
    else
      let fft1, fft2 =
        Pair.map_same (Genarray.get_slice [ [ 0; 2 ] ]) (fft1, fft2)
      in
      let max_mse =
        let fft1_mag, fft2_mag =
          Pair.map_same (Genarray.map Complex.norm_complex) (fft1, fft2)
        in
        let fft1_sq, fft2_sq =
          Pair.map_same (Genarray.map Complex.square) (fft1_mag, fft2_mag)
        in
        let added = Genarray.add fft1_sq fft2_sq in
        let sum = Genarray.sum' added in
        sum.re /. 3.0
      in
      let conj = Genarray.conj fft2 in
      let minus = Genarray.sub fft1 conj in
      let magnitudes = Genarray.map Complex.norm_complex minus in
      let squared = Genarray.map Complex.square magnitudes in
      let summed = Genarray.sum' squared in
      let sum = Complex.norm summed in
      let final = sum /. 3.0 /. max_mse *. 10000.0 in
      (* Eio.traceln "%f" final; *)
      final

(* Fast fourier transform *)
let fft (config : Indicator_config.t) (l : Price_history.t) (last : Item.t) : t
    =
  if not config.fft then empty
  else
    let arr =
      Vector.map Item.last l |> Vector.to_array |> fun a ->
      Array.append a [| Item.last last |]
    in
    let bigarray = Genarray.of_array Float64 arr [| Array.length arr |] in
    let yf = Owl_fft.D.rfft ~axis:0 bigarray in
    (* Eio.traceln "fft: length : %a" (Array.pp Int.pp) (Genarray.shape yf); *)
    yf

(* Normalized maginitude of fourier transform *)
let fft_nm (config : Indicator_config.t) (yf : t) (l : Price_history.t) =
  if not config.fft then 0.0
  else
    let length = Vector.length l + 1 |> Float.of_int in
    let mag = Genarray.l2norm yf |> Genarray.sum' |> Complex.norm in
    mag /. length
