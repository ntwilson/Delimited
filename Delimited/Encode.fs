namespace Delimited

open System
open System.Collections.Generic
open System.Globalization

/// Writes one field's text.
type FieldEncoder<'a> = 'a -> string

/// Columns in output order: header and how to produce that field.
type RowEncoder<'a> = (string * ('a -> string)) list

module Encode =
  module Field =
    let contramap (f: 'b -> 'a) (encoder: FieldEncoder<'a>) : FieldEncoder<'b> = f >> encoder

  module Row =
    let contramap (f: 'b -> 'a) (encoder: RowEncoder<'a>) : RowEncoder<'b> =
      encoder |> List.map (fun (header, get) -> header, f >> get)

  let private inv = CultureInfo.InvariantCulture

  // EVERYTHING BELOW SHADOWS int, float, string, ETC. AS FUNCTIONS.
  let string: FieldEncoder<string> = id
  let int: FieldEncoder<int> = fun i -> i.ToString inv
  let int64: FieldEncoder<int64> = fun i -> i.ToString inv
  let float: FieldEncoder<float> = fun x -> x.ToString("R", inv)
  let decimal: FieldEncoder<decimal> = fun x -> x.ToString inv
  let bool: FieldEncoder<bool> = fun b -> if b then "true" else "false"
  let guid: FieldEncoder<Guid> = fun g -> g.ToString()
  let dateTime (format: string) : FieldEncoder<DateTime> = fun d -> d.ToString(format, inv)
  let dateTimeOffset (format: string) : FieldEncoder<DateTimeOffset> = fun d -> d.ToString(format, inv)

  /// UTC ends in "Z", Local in its offset, and Unspecified in nothing.
  let dateTimeIso: FieldEncoder<DateTime> = dateTime Iso8601.dateTimeFormat

  let dateTimeOffsetIso: FieldEncoder<DateTimeOffset> =
    dateTimeOffset Iso8601.dateTimeOffsetFormat

#if NET6_0_OR_GREATER
  let dateOnly (format: string) : FieldEncoder<DateOnly> = fun d -> d.ToString(format, inv)
  let timeOnly (format: string) : FieldEncoder<TimeOnly> = fun t -> t.ToString(format, inv)
  let dateOnlyIso: FieldEncoder<DateOnly> = dateOnly Iso8601.dateOnlyFormat
  let timeOnlyIso: FieldEncoder<TimeOnly> = timeOnly Iso8601.timeOnlyFormat
#endif

  /// None encodes to `missing`.
  let optionWith (missing: string) (encoder: FieldEncoder<'a>) : FieldEncoder<'a option> =
    function
    | Some v -> encoder v
    | None -> missing

  /// ValueNone encodes to `missing`.
  let voptionWith (missing: string) (encoder: FieldEncoder<'a>) : FieldEncoder<'a voption> =
    function
    | ValueSome v -> encoder v
    | ValueNone -> missing

  /// None encodes to an empty field.
  let option (encoder: FieldEncoder<'a>) : FieldEncoder<'a option> = optionWith "" encoder

  /// ValueNone encodes to an empty field.
  let voption (encoder: FieldEncoder<'a>) : FieldEncoder<'a voption> = voptionWith "" encoder

  let result (okEncoder: FieldEncoder<'a>) (errEncoder: FieldEncoder<'e>) : FieldEncoder<Result<'a, 'e>> =
    function
    | Ok a -> okEncoder a
    | Error e -> errEncoder e

  let choiceOf2 (left: FieldEncoder<'a>) (right: FieldEncoder<'b>) : FieldEncoder<Choice<'a, 'b>> =
    function
    | Choice1Of2 a -> left a
    | Choice2Of2 b -> right b

  let field (header: string) (encoder: FieldEncoder<'field>) (getter: 'record -> 'field) : RowEncoder<'record> =
    [ (header, getter >> encoder) ]

  let columns (encoders: RowEncoder<'a> list) : RowEncoder<'a> = List.concat encoders

  /// A missing key writes an empty field.
  let dictionary<'dict, 'a when 'dict :> IDictionary<string, 'a>>
    (headers: string list)
    (encoder: FieldEncoder<'a>)
    : RowEncoder<'dict> =
    headers
    |> List.map (fun h ->
      h,
      fun (d: 'dict) ->
        match d.TryGetValue h with
        | true, v -> encoder v
        | false, _ -> "")

  /// List a header once per column to write; its nth column writes the nth value. A missing key or value writes an
  /// empty field.
  let multiDictionary<'dict, 'a when 'dict :> IDictionary<string, 'a list>>
    (headers: string list)
    (encoder: FieldEncoder<'a>)
    : RowEncoder<'dict> =
    headers
    |> List.mapi (fun i h ->
      let occurrence = headers |> List.take i |> List.filter ((=) h) |> List.length

      h,
      fun (d: 'dict) ->
        match d.TryGetValue h with
        | true, values ->
          values
          |> List.tryItem occurrence
          |> Option.map encoder
          |> Option.defaultValue ""
        | false, _ -> "")

  /// Positional; a short row writes empty fields.
  let fields (headers: string list) : RowEncoder<string[]> =
    headers
    |> List.mapi (fun i h -> h, fun (row: string[]) -> row |> Array.tryItem i |> Option.defaultValue "")
