namespace Delimited

open System
open System.Collections.Generic

/// Header is None only when the file has no header row.
type Column = { Index: int; Header: string option }

type DecodeProblem =
  | ColumnNotFound of header: string
  | MissingField of Column * fieldCount: int
  | InvalidValue of Column * value: string * message: string
  | RowFailed of message: string

type DecodeError = { Row: int64; Problem: DecodeProblem }

module DecodeError =
  let private describeColumn (c: Column) =
    match c.Header with
    | Some h -> $"column %i{c.Index} (\"%s{h}\")"
    | None -> $"column %i{c.Index}"

  let describe (e: DecodeError) : string =
    let detail =
      match e.Problem with
      | ColumnNotFound header -> $"column \"%s{header}\" not found"
      | MissingField(c, fieldCount) -> $"%s{describeColumn c} is missing; the row has only %i{fieldCount} fields"
      | InvalidValue(c, value, message) -> $"%s{describeColumn c} has value \"%s{value}\": %s{message}"
      | RowFailed message -> message

    $"Row %i{e.Row}: %s{detail}"

type DecodeException(errors: DecodeError list) =
  inherit Exception(errors |> List.map DecodeError.describe |> String.concat Environment.NewLine)
  member _.Errors = errors

/// Decodes one row. Errors are a list so independent fields can all report at once.
type Decoder<'T> = Row -> Result<'T, DecodeError list>

module Decode =
  let private error (row: Row) (problem: DecodeProblem) =
    Error [ { Row = row.Number; Problem = problem } ]

  let succeed (value: 'T) : Decoder<'T> = fun _ -> Ok value

  let fail (message: string) : Decoder<'T> =
    fun row -> error row (RowFailed message)

  let private singleFieldAt (index: int) (codec: FieldCodec<'T>) (row: Row) =
    let column = { Index = index; Header = row.Headers |> Array.tryItem index }

    match Row.tryGet index row with
    | ValueNone -> Error { Row = row.Number; Problem = MissingField(column, row.FieldCount) }
    | ValueSome s ->
      match codec.Decode s with
      | Ok v -> Ok v
      | Error message -> Error { Row = row.Number; Problem = InvalidValue(column, s, message) }

  let private singleField (header: string) (codec: FieldCodec<'T>) =
    fun row ->
      match Row.tryOrdinal header row with
      | ValueSome i -> singleFieldAt i codec row
      | ValueNone -> Error { Row = row.Number; Problem = ColumnNotFound header }

  /// The named column, which must exist.
  let field header codec : Decoder<'a> =
    singleField header codec >> Result.mapError List.singleton

  /// None when the column is missing, the row is short, or the field is blank.
  let optionalField (header: string) (codec: FieldCodec<'T>) : Decoder<'T option> =
    fun row ->
      match singleField header (Field.option codec) row with
      | Ok v -> Ok v
      | Error { Problem = MissingField _ }
      | Error { Problem = ColumnNotFound _ } -> Ok None
      | Error e -> Error [ e ]

  /// The 0-based column, for files without headers.
  let index (i: int) (codec: FieldCodec<'T>) : Decoder<'T> =
    singleFieldAt i codec >> Result.mapError List.singleton

  let map (f: 'A -> 'B) (decoder: Decoder<'A>) : Decoder<'B> = decoder >> Result.map f

  /// Runs both decoders and keeps the errors from both.
  let map2 (f: 'A -> 'B -> 'C) (da: Decoder<'A>) (db: Decoder<'B>) : Decoder<'C> =
    fun row ->
      match da row, db row with
      | Ok a, Ok b -> Ok(f a b)
      | Error ea, Error eb -> Error(ea @ eb)
      | Error e, Ok _
      | Ok _, Error e -> Error e

  let andThen (f: 'A -> Decoder<'B>) (decoder: Decoder<'A>) : Decoder<'B> =
    fun row -> decoder row |> Result.bind (fun a -> f a row)

  /// The first decoder that succeeds; otherwise the errors from all of them.
  let oneOf (decoders: Decoder<'T> list) : Decoder<'T> =
    fun row ->
      let rec go errors =
        function
        | [] -> Error(List.rev errors |> List.concat)
        | (d: Decoder<'T>) :: rest ->
          match d row with
          | Ok v -> Ok v
          | Error e -> go (e :: errors) rest

      go [] decoders

  /// Every field in the row.
  let fields: Decoder<string[]> =
    fun row -> Ok(Array.init row.FieldCount row.GetString)

  /// Header -> value. Missing trailing fields are empty strings; a repeated header keeps its first value.
  let dictionary: Decoder<IReadOnlyDictionary<string, string>> =
    fun row ->
      if Array.isEmpty row.Headers then
        error row (RowFailed "Decode.dictionary needs headers")
      else
        row.Ordinals
        |> Seq.map (fun kv -> KeyValuePair(kv.Key, Row.tryGet kv.Value row |> ValueOption.defaultValue ""))
        |> fun kvs -> Ok(Dictionary(kvs, StringComparer.Ordinal) :> IReadOnlyDictionary<_, _>)

type DecoderBuilder() =
  member _.Return(value: 'T) : Decoder<'T> = Decode.succeed value
  member _.ReturnFrom(decoder: Decoder<'T>) : Decoder<'T> = decoder
  member _.Bind(decoder: Decoder<'A>, f: 'A -> Decoder<'B>) : Decoder<'B> = Decode.andThen f decoder
  member _.BindReturn(decoder: Decoder<'A>, f: 'A -> 'B) : Decoder<'B> = Decode.map f decoder

  member _.MergeSources(da: Decoder<'A>, db: Decoder<'B>) : Decoder<'A * 'B> = Decode.map2 (fun a b -> a, b) da db

  member _.Zero() : Decoder<unit> = Decode.succeed ()

[<AutoOpen>]
module DecoderBuilderInstance =
  /// `let!` sequences; `and!` runs fields independently and reports every failure.
  let decoder = DecoderBuilder()
