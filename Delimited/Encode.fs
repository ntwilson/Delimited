namespace Delimited

open System
open System.Collections.Generic
open System.Globalization

/// Columns in output order: header and how to produce that field.
type Encoder<'T> = (string * ('T -> string)) list

module Encode =
  let field (header: string) (codec: FieldCodec<'F>) (getter: 'T -> 'F) : Encoder<'T> =
    [ (header, getter >> codec.Encode) ]

  let columns (encoders: Encoder<'T> list) : Encoder<'T> = List.concat encoders

  let contramap (f: 'B -> 'A) (encoder: Encoder<'A>) : Encoder<'B> =
    encoder |> List.map (fun (header, get) -> header, f >> get)

  /// A missing key writes an empty field.
  let dictionary<'D when 'D :> IDictionary<string, string>> (headers: string list) : Encoder<'D> =
    headers
    |> List.map (fun h ->
      h,
      fun (d: 'D) ->
        match d.TryGetValue h with
        | true, v -> v
        | false, _ -> "")

  /// Works with ExpandoObject. Values use invariant-culture ToString; missing keys and nulls write an empty field.
  let objDictionary<'D when 'D :> IDictionary<string, obj>> (headers: string list) : Encoder<'D> =
    headers
    |> List.map (fun h ->
      h,
      fun (d: 'D) ->
        match d.TryGetValue h with
        | true, v -> Convert.ToString(v, CultureInfo.InvariantCulture)
        | false, _ -> "")

  /// Positional; a short row writes empty fields.
  let fields (headers: string list) : Encoder<string[]> =
    headers
    |> List.mapi (fun i h -> h, fun (row: string[]) -> row |> Array.tryItem i |> Option.defaultValue "")
