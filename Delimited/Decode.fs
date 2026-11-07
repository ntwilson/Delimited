namespace Delimited

open System
open System.Collections.Generic
open System.Globalization

/// Header is None only when the file has no header row.
type Column = { Index: int; Header: string option }

type DecodeProblem =
  | ColumnNotFound of header: string
  | MissingField of Column * fieldCount: int
  | InvalidValue of Column * value: string * message: string
  | RowFailed of message: string
  | NoHeaderRow

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
      | NoHeaderRow -> "the table has no header row, so columns can't be found by name"

    $"Row %i{e.Row}: %s{detail}"

type DecodeException(errors: DecodeError list) =
  inherit Exception(errors |> List.map DecodeError.describe |> String.concat Environment.NewLine)
  member _.Errors = errors

/// Parses one field's text.
type FieldDecoder<'a> = string -> Result<'a, string>

/// Decodes one row. Errors are a list so independent fields can all report at once.
type RowDecoder<'a> = Row -> Result<'a, DecodeError list>

module internal Results =
  let collect (values: 'a list, errors: 'e list) (result: Result<'a, 'e list>) =
    match result with
    | Ok v -> v :: values, errors
    | Error e -> values, List.rev e @ errors

  let finish (values: 'a list, errors: 'e list) = List.rev values, List.rev errors

  /// The values of the Ok results and the errors of the rest, each in order.
  let partition (results: Result<'a, 'e list> seq) =
    results |> Seq.fold collect ([], []) |> finish

module Decode =
  module Field =
    let map (f: 'a -> 'b) (decoder: FieldDecoder<'a>) : FieldDecoder<'b> = decoder >> Result.map f

    /// Like `map`, but may fail with a message.
    let andThen (f: 'a -> Result<'b, string>) (decoder: FieldDecoder<'a>) : FieldDecoder<'b> = decoder >> Result.bind f

    /// `first` if it succeeds, else `second`; when both fail, both messages joined.
    /// For more than two, use `List.reduce Decode.Field.either`.
    let either (first: FieldDecoder<'a>) (second: FieldDecoder<'a>) : FieldDecoder<'a> =
      fun s ->
        match first s with
        | Ok v -> Ok v
        | Error e1 ->
          match second s with
          | Ok v -> Ok v
          | Error e2 -> Error $"%s{e1}, or %s{e2}"

  module Row =
    let private error (row: Row) (problem: DecodeProblem) =
      Error [ { Row = row.Number; Problem = problem } ]

    let succeed (value: 'a) : RowDecoder<'a> = fun _ -> Ok value

    let fail (message: string) : RowDecoder<'a> =
      fun row -> error row (RowFailed message)

    let map (f: 'a -> 'b) (decoder: RowDecoder<'a>) : RowDecoder<'b> = decoder >> Result.map f

    /// Runs both decoders and keeps the errors from both.
    let map2 (f: 'a -> 'b -> 'c) (da: RowDecoder<'a>) (db: RowDecoder<'b>) : RowDecoder<'c> =
      fun row ->
        match da row, db row with
        | Ok a, Ok b -> Ok(f a b)
        | Error ea, Error eb -> Error(ea @ eb)
        | Error e, Ok _
        | Ok _, Error e -> Error e

    let andThen (f: 'a -> RowDecoder<'b>) (decoder: RowDecoder<'a>) : RowDecoder<'b> =
      fun row -> decoder row |> Result.bind (fun a -> f a row)

    /// `first` if it succeeds, else `second`; when both fail, the errors from both.
    /// For more than two, use `List.reduce Decode.Row.either`.
    let either (first: RowDecoder<'a>) (second: RowDecoder<'a>) : RowDecoder<'a> =
      fun row ->
        match first row with
        | Ok v -> Ok v
        | Error e1 ->
          match second row with
          | Ok v -> Ok v
          | Error e2 -> Error(e1 @ e2)

  let private inv = CultureInfo.InvariantCulture

  /// Builds a decoder from a .NET-style TryParse; `expected` describes the type in error messages.
  let ofTryParse (expected: string) (tryParse: string -> bool * 'a) : FieldDecoder<'a> =
    fun s ->
      match tryParse s with
      | true, v -> Ok v
      | false, _ -> Error $"Expected %s{expected} but got \"%s{s}\""

  // EVERYTHING BELOW SHADOWS int, float, string, ETC. AS FUNCTIONS.
  let string: FieldDecoder<string> = Ok

  let int: FieldDecoder<int> =
    ofTryParse "a 32-bit integer" (fun s ->
      Int32.TryParse(s, NumberStyles.Integer ||| NumberStyles.AllowThousands, inv))

  let int64: FieldDecoder<int64> =
    ofTryParse "a 64-bit integer" (fun s ->
      Int64.TryParse(s, NumberStyles.Integer ||| NumberStyles.AllowThousands, inv))

  let float: FieldDecoder<float> =
    ofTryParse "a number" (fun s -> Double.TryParse(s, NumberStyles.Float ||| NumberStyles.AllowThousands, inv))

  let decimal: FieldDecoder<decimal> =
    ofTryParse "a decimal" (fun s -> Decimal.TryParse(s, NumberStyles.Number, inv))

  let bool: FieldDecoder<bool> =
    ofTryParse "true or false" (fun s -> Boolean.TryParse s)

  let guid: FieldDecoder<Guid> = ofTryParse "a GUID" (fun s -> Guid.TryParse s)

  let dateTime (format: string) : FieldDecoder<DateTime> =
    ofTryParse $"a date/time formatted as %s{format}" (fun s ->
      DateTime.TryParseExact(s, format, inv, DateTimeStyles.None))

  let dateTimeOffset (format: string) : FieldDecoder<DateTimeOffset> =
    ofTryParse $"a date/time offset formatted as %s{format}" (fun s ->
      DateTimeOffset.TryParseExact(s, format, inv, DateTimeStyles.None))


  // WITHOUT RoundtripKind, A "Z" OR OFFSET SUFFIX WOULD BE CONVERTED TO THE MACHINE'S LOCAL TIME.
  /// Uses the invariant culture, so an ambiguous date like 03/04 is read month first.
  let dateTimeAuto: FieldDecoder<DateTime> =
    ofTryParse "a date/time" (fun s -> DateTime.TryParse(s, inv, DateTimeStyles.RoundtripKind))

  /// Uses the invariant culture, so an ambiguous date like 03/04 is read month first.
  let dateTimeOffsetAuto: FieldDecoder<DateTimeOffset> =
    ofTryParse "a date/time offset" (fun s -> DateTimeOffset.TryParse(s, inv, DateTimeStyles.None))

  /// A space may replace the "T". An offset is converted to UTC; without one, the Kind is Unspecified.
  let dateTimeIso: FieldDecoder<DateTime> =
    ofTryParse "an ISO 8601 date/time" (fun s ->
      DateTime.TryParseExact(s, Iso8601.dateTimeFormats, inv, DateTimeStyles.AdjustToUniversal))

  /// Like `dateTimeIso`, but requires an offset or "Z" and keeps it.
  let dateTimeOffsetIso: FieldDecoder<DateTimeOffset> =
    ofTryParse "an ISO 8601 date/time with an offset" (fun s ->
      DateTimeOffset.TryParseExact(s, Iso8601.dateTimeOffsetFormats, inv, DateTimeStyles.AssumeUniversal))

#if NET6_0_OR_GREATER
  let dateOnly (format: string) : FieldDecoder<DateOnly> =
    ofTryParse $"a date formatted as %s{format}" (fun s -> DateOnly.TryParseExact(s, format, inv, DateTimeStyles.None))

  let timeOnly (format: string) : FieldDecoder<TimeOnly> =
    ofTryParse $"a time formatted as %s{format}" (fun s -> TimeOnly.TryParseExact(s, format, inv, DateTimeStyles.None))

  /// Uses the invariant culture, so an ambiguous date like 03/04 is read month first.
  let dateOnlyAuto: FieldDecoder<DateOnly> =
    ofTryParse "a date" (fun s -> DateOnly.TryParse(s, inv, DateTimeStyles.None))

  let timeOnlyAuto: FieldDecoder<TimeOnly> =
    ofTryParse "a time" (fun s -> TimeOnly.TryParse(s, inv, DateTimeStyles.None))

  let dateOnlyIso: FieldDecoder<DateOnly> =
    ofTryParse "an ISO 8601 date" (fun s ->
      DateOnly.TryParseExact(s, Iso8601.dateOnlyFormats, inv, DateTimeStyles.None))

  /// May start with "T"; an offset is rejected, since TimeOnly can't hold it.
  let timeOnlyIso: FieldDecoder<TimeOnly> =
    ofTryParse "an ISO 8601 time" (fun s ->
      TimeOnly.TryParseExact(s, Iso8601.timeOnlyFormats, inv, DateTimeStyles.None))
#endif

  /// A blank field decodes to None.
  let option (decoder: FieldDecoder<'a>) : FieldDecoder<'a option> =
    fun s ->
      if String.IsNullOrWhiteSpace s then
        Ok None
      else
        decoder s |> Result.map Some

  /// A blank field decodes to ValueNone.
  let voption (decoder: FieldDecoder<'a>) : FieldDecoder<'a voption> =
    fun s ->
      if String.IsNullOrWhiteSpace s then
        Ok ValueNone
      else
        decoder s |> Result.map ValueSome

  /// Prefers Ok when both accept the field; reports the Ok decoder's error when neither does.
  let result (okDecoder: FieldDecoder<'a>) (errDecoder: FieldDecoder<'e>) : FieldDecoder<Result<'a, 'e>> =
    fun s ->
      match okDecoder s, errDecoder s with
      | Ok a, _ -> Ok(Ok a)
      | Error _, Ok e -> Ok(Error e)
      | Error e, Error _ -> Error e

  /// Prefers `left` when both accept the field; when neither does, both messages joined.
  let choiceOf2 (left: FieldDecoder<'a>) (right: FieldDecoder<'b>) : FieldDecoder<Choice<'a, 'b>> =
    Field.either (left |> Field.map Choice1Of2) (right |> Field.map Choice2Of2)

  let private singleFieldAt (index: int) (decoder: FieldDecoder<'a>) (row: Row) =
    let column =
      { Index = index; Header = row.Headers |> Option.bind (Array.tryItem index) }

    match Delimited.Row.tryGet index row with
    | ValueNone -> Error { Row = row.Number; Problem = MissingField(column, row.FieldCount) }
    | ValueSome s ->
      match decoder s with
      | Ok v -> Ok v
      | Error message -> Error { Row = row.Number; Problem = InvalidValue(column, s, message) }

  let private singleField (header: string) (decoder: FieldDecoder<'a>) =
    fun row ->
      match row.Headers, Delimited.Row.tryOrdinal header row with
      | None, _ -> Error { Row = row.Number; Problem = NoHeaderRow }
      | Some _, ValueSome i -> singleFieldAt i decoder row
      | Some _, ValueNone -> Error { Row = row.Number; Problem = ColumnNotFound header }

  /// The named column, which must exist.
  let field (header: string) (decoder: FieldDecoder<'a>) : RowDecoder<'a> =
    singleField header decoder >> Result.mapError List.singleton

  /// None when the column is missing, the row is short, or the field is blank. Fails if the table has no header row.
  let optionalField (header: string) (decoder: FieldDecoder<'a>) : RowDecoder<'a option> =
    fun row ->
      match singleField header (option decoder) row with
      | Ok v -> Ok v
      | Error { Problem = MissingField _ }
      | Error { Problem = ColumnNotFound _ } -> Ok None
      | Error e -> Error [ e ]

  /// The 0-based column, for files without headers.
  let index (i: int) (decoder: FieldDecoder<'a>) : RowDecoder<'a> =
    singleFieldAt i decoder >> Result.mapError List.singleton

  /// Every field in the row.
  let fields: RowDecoder<string[]> =
    fun row -> Ok(Array.init row.FieldCount row.GetString)

  /// The listed columns, keyed by header; each must exist. A repeated header is read once.
  let dictionaryOf (headers: string list) (decoder: FieldDecoder<'a>) : RowDecoder<Dictionary<string, 'a>> =
    fun row ->
      if row.Headers.IsNone then
        Error [ { Row = row.Number; Problem = NoHeaderRow } ]
      else
        let pairs, errors =
          headers
          |> List.distinct
          |> Seq.map (fun h -> field h decoder row |> Result.map (fun v -> KeyValuePair(h, v)))
          |> Results.partition

        match errors with
        | [] -> Ok(Dictionary(pairs, StringComparer.Ordinal))
        | errors -> Error errors

  /// Every column, keyed by header. A repeated header keeps its first value.
  let dictionary (decoder: FieldDecoder<'a>) : RowDecoder<Dictionary<string, 'a>> =
    fun row -> dictionaryOf (row.Headers |> Option.defaultValue [||] |> List.ofArray) decoder row

type RowDecoderBuilder() =
  member _.Return(value: 'a) : RowDecoder<'a> = Decode.Row.succeed value
  member _.ReturnFrom(decoder: RowDecoder<'a>) : RowDecoder<'a> = decoder

  member _.Bind(decoder: RowDecoder<'a>, f: 'a -> RowDecoder<'b>) : RowDecoder<'b> = Decode.Row.andThen f decoder

  member _.BindReturn(decoder: RowDecoder<'a>, f: 'a -> 'b) : RowDecoder<'b> = Decode.Row.map f decoder

  member _.MergeSources(da: RowDecoder<'a>, db: RowDecoder<'b>) : RowDecoder<'a * 'b> =
    Decode.Row.map2 (fun a b -> a, b) da db

  member _.Zero() : RowDecoder<unit> = Decode.Row.succeed ()

[<AutoOpen>]
module RowDecoderBuilderInstance =
  /// `let!` sequences; `and!` runs fields independently and reports every failure.
  let decoder = RowDecoderBuilder()
