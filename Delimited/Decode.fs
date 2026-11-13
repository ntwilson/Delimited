namespace Delimited

open System
open System.Collections.Generic
open System.Globalization
open System.Linq

/// Header is None only when the file has no header row.
type Column = { Index: int; Header: string option }

type DecodeProblem =
  | ColumnNotFound of header: string
  | MissingField of Column * fieldCount: int
  | InvalidValue of Column * value: string * message: string
  | RowFailed of message: string
  | NoHeaderRow
  /// The header names more than one column; `indexes` are 0-based.
  | AmbiguousHeader of header: string * indexes: int list
  /// `occurrence` is 0-based; `count` is how many columns have the header.
  | OccurrenceNotFound of header: string * occurrence: int * count: int

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
      | AmbiguousHeader(header, indexes) ->
        let columns = indexes |> List.map string |> String.concat ", "
        $"column \"%s{header}\" appears more than once (columns %s{columns}); use fieldOccurrence to pick one"
      | OccurrenceNotFound(header, occurrence, count) ->
        $"column \"%s{header}\" appears %i{count} time(s), so it has no occurrence %i{occurrence} (0-based)"

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

  /// An offset or "Z" in the text converts the value to UTC (Kind = Utc), dropping the offset; without one, the
  /// Kind is Unspecified. For data that has offsets, use `dateTimeOffset`, which keeps them.
  let dateTime (format: string) : FieldDecoder<DateTime> =
    ofTryParse $"a date/time formatted as %s{format}" (fun s ->
      DateTime.TryParseExact(s, format, inv, DateTimeStyles.AdjustToUniversal))

  /// Text without an offset is read as UTC.
  let dateTimeOffset (format: string) : FieldDecoder<DateTimeOffset> =
    ofTryParse $"a date/time offset formatted as %s{format}" (fun s ->
      DateTimeOffset.TryParseExact(s, format, inv, DateTimeStyles.AssumeUniversal))

  /// Uses the invariant culture, so an ambiguous date like 03/04 is read month first.
  /// An offset or "Z" in the text converts the value to UTC (Kind = Utc), dropping the offset; without one, the
  /// Kind is Unspecified. For data that has offsets, use `dateTimeOffsetAuto`, which keeps them.
  let dateTimeAuto: FieldDecoder<DateTime> =
    ofTryParse "a date/time" (fun s -> DateTime.TryParse(s, inv, DateTimeStyles.AdjustToUniversal))

  /// Uses the invariant culture, so an ambiguous date like 03/04 is read month first.
  /// Text without an offset is read as UTC.
  let dateTimeOffsetAuto: FieldDecoder<DateTimeOffset> =
    ofTryParse "a date/time offset" (fun s -> DateTimeOffset.TryParse(s, inv, DateTimeStyles.AssumeUniversal))

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

  // A NULL FIELD COUNTS AS BLANK, AND SURROUNDING WHITESPACE IS IGNORED ON BOTH SIDES.
  let private isMissing (missing: string) (s: string) =
    (if isNull s then "" else s.Trim()) = missing.Trim()

  /// A field matching `missing` decodes to None.
  let optionWith (missing: string) (decoder: FieldDecoder<'a>) : FieldDecoder<'a option> =
    fun s ->
      if isMissing missing s then
        Ok None
      else
        decoder s |> Result.map Some

  /// A field matching `missing` decodes to ValueNone.
  let voptionWith (missing: string) (decoder: FieldDecoder<'a>) : FieldDecoder<'a voption> =
    fun s ->
      if isMissing missing s then
        Ok ValueNone
      else
        decoder s |> Result.map ValueSome

  /// A blank field decodes to None.
  let option (decoder: FieldDecoder<'a>) : FieldDecoder<'a option> = optionWith "" decoder

  /// A blank field decodes to ValueNone.
  let voption (decoder: FieldDecoder<'a>) : FieldDecoder<'a voption> = voptionWith "" decoder

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

  /// Every index of the header; never empty when Ok.
  let private indexesOf (header: string) (row: Row) : Result<int[], DecodeError> =
    match row.Headers, Delimited.Row.ordinals header row with
    | None, _ -> Error { Row = row.Number; Problem = NoHeaderRow }
    | Some _, [||] -> Error { Row = row.Number; Problem = ColumnNotFound header }
    | Some _, indexes -> Ok indexes

  let private singleField (header: string) (decoder: FieldDecoder<'a>) =
    fun row ->
      match indexesOf header row with
      | Ok [| i |] -> singleFieldAt i decoder row
      | Ok indexes -> Error { Row = row.Number; Problem = AmbiguousHeader(header, List.ofArray indexes) }
      | Error e -> Error e

  /// The named column, which must exist exactly once.
  let field (header: string) (decoder: FieldDecoder<'a>) : RowDecoder<'a> =
    singleField header decoder >> Result.mapError List.singleton

  /// The `occurrence`th (0-based) column named `header`, for tables where a header repeats.
  let fieldOccurrence (header: string) (occurrence: int) (decoder: FieldDecoder<'a>) : RowDecoder<'a> =
    if occurrence < 0 then
      invalidArg (nameof occurrence) "must be 0 or more"

    fun row ->
      match indexesOf header row with
      | Ok indexes when occurrence < indexes.Length ->
        singleFieldAt indexes.[occurrence] decoder row |> Result.mapError List.singleton
      | Ok indexes ->
        Error
          [
            {
              Row = row.Number
              Problem = OccurrenceNotFound(header, occurrence, indexes.Length)
            }
          ]
      | Error e -> Error [ e ]

  /// None when the column is missing, the row is short, or the field matches `missing`. Fails if the table has no
  /// header row or the header repeats.
  let optionalFieldWith (missing: string) (header: string) (decoder: FieldDecoder<'a>) : RowDecoder<'a option> =
    fun row ->
      match singleField header (optionWith missing decoder) row with
      | Ok v -> Ok v
      | Error { Problem = MissingField _ }
      | Error { Problem = ColumnNotFound _ } -> Ok None
      | Error e -> Error [ e ]

  /// None when the column is missing, the row is short, or the field is blank. Fails if the table has no header row
  /// or the header repeats.
  let optionalField (header: string) (decoder: FieldDecoder<'a>) : RowDecoder<'a option> =
    optionalFieldWith "" header decoder

  /// The 0-based column, for files without headers.
  let index (i: int) (decoder: FieldDecoder<'a>) : RowDecoder<'a> =
    singleFieldAt i decoder >> Result.mapError List.singleton

  /// Every field in the row.
  let fields: RowDecoder<string[]> =
    fun row -> Ok(Array.init row.FieldCount row.GetString)

  let private keyedBy
    (headers: string list)
    (decodeHeader: string -> Row -> Result<'a, DecodeError list>)
    : RowDecoder<Dictionary<string, 'a>> =
    fun row ->
      if row.Headers.IsNone then
        Error [ { Row = row.Number; Problem = NoHeaderRow } ]
      else
        let pairs, errors =
          Enumerable.Distinct(headers, row.HeaderComparer)
          |> Seq.map (fun h -> decodeHeader h row |> Result.map (fun v -> KeyValuePair(h, v)))
          |> Results.partition

        match errors with
        | [] -> Ok(Dictionary(pairs, row.HeaderComparer))
        | errors -> Error errors

  let private allHeaders (row: Row) =
    row.Headers |> Option.defaultValue [||] |> List.ofArray

  /// The listed columns, keyed by header using `TableOptions.HeaderComparer`; each must exist exactly once.
  /// A header listed twice is read once. For headers that repeat in the table, use `multiDictionaryOf`.
  let dictionaryOf (headers: string list) (decoder: FieldDecoder<'a>) : RowDecoder<Dictionary<string, 'a>> =
    keyedBy headers (fun h -> field h decoder)

  /// Every column, keyed by header. Fails if any header repeats; for that, use `multiDictionary`.
  let dictionary (decoder: FieldDecoder<'a>) : RowDecoder<Dictionary<string, 'a>> =
    fun row -> dictionaryOf (allHeaders row) decoder row

  /// Like `dictionaryOf`, but each header maps to every column it names, in table order.
  let multiDictionaryOf (headers: string list) (decoder: FieldDecoder<'a>) : RowDecoder<Dictionary<string, 'a list>> =
    keyedBy headers (fun h row ->
      match indexesOf h row with
      | Error e -> Error [ e ]
      | Ok indexes ->
        let values, errors =
          indexes
          |> Seq.map (fun i -> singleFieldAt i decoder row |> Result.mapError List.singleton)
          |> Results.partition

        if List.isEmpty errors then Ok values else Error errors)

  /// Every column, keyed by header; a header that repeats maps to all its columns, in table order.
  let multiDictionary (decoder: FieldDecoder<'a>) : RowDecoder<Dictionary<string, 'a list>> =
    fun row -> multiDictionaryOf (allHeaders row) decoder row

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
