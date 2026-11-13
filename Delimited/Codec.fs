namespace Delimited

open System.Collections.Generic

/// Decodes a row to 'a and encodes columns from a 'record. Combine with `codec { }` until 'a is 'record.
type PartialCodec<'record, 'a> = { Decode: RowDecoder<'a>; Encode: RowEncoder<'record> }

/// Reads and writes whole rows as 'record.
type RowCodec<'record> = { Decode: RowDecoder<'record>; Encode: RowEncoder<'record> }

/// Converts a single field between its text form and 'a.
type FieldCodec<'a> = { Decode: FieldDecoder<'a>; Encode: FieldEncoder<'a> }

module Codec =
  let private partial (decode: RowDecoder<'a>) (encode: RowEncoder<'record>) : PartialCodec<'record, 'a> =
    { Decode = decode; Encode = encode }

  module Field =
    let create (decode: FieldDecoder<'a>) (encode: FieldEncoder<'a>) : FieldCodec<'a> =
      { Decode = decode; Encode = encode }

    /// Converts both directions; `decode` and `encode` should be inverses.
    let map (decode: 'a -> 'b) (encode: 'b -> 'a) (codec: FieldCodec<'a>) : FieldCodec<'b> =
      create (Decode.Field.map decode codec.Decode) (Encode.Field.contramap encode codec.Encode)

    /// Like `map`, but decoding may fail with a message.
    let andThen (decode: 'a -> Result<'b, string>) (encode: 'b -> 'a) (codec: FieldCodec<'a>) : FieldCodec<'b> =
      create (Decode.Field.andThen decode codec.Decode) (Encode.Field.contramap encode codec.Encode)

  module Row =
    let create (decode: RowDecoder<'record>) (encode: RowEncoder<'record>) : RowCodec<'record> =
      { Decode = decode; Encode = encode }

    /// Converts both directions; `decode` and `encode` should be inverses.
    let map (decode: 'a -> 'b) (encode: 'b -> 'a) (codec: RowCodec<'a>) : RowCodec<'b> =
      create (Decode.Row.map decode codec.Decode) (Encode.Row.contramap encode codec.Encode)

    let internal ofPartial (codec: PartialCodec<'record, 'record>) : RowCodec<'record> =
      create codec.Decode codec.Encode

    /// Decodes to `value` and writes no columns.
    let internal succeed (value: 'a) : PartialCodec<'record, 'a> = partial (Decode.Row.succeed value) []

    /// Decoding only; the columns come from the getters.
    let internal mapPartial (f: 'a -> 'b) (codec: PartialCodec<'record, 'a>) : PartialCodec<'record, 'b> =
      partial (Decode.Row.map f codec.Decode) codec.Encode

    /// Decodes both and keeps the errors from both; encodes the first's columns then the second's.
    let internal map2
      (f: 'a -> 'b -> 'c)
      (ca: PartialCodec<'record, 'a>)
      (cb: PartialCodec<'record, 'b>)
      : PartialCodec<'record, 'c> =
      partial (Decode.Row.map2 f ca.Decode cb.Decode) (ca.Encode @ cb.Encode)

  // EVERYTHING BELOW SHADOWS int, float, string, ETC. AS FUNCTIONS.
  let string: FieldCodec<string> = Field.create Decode.string Encode.string
  let int: FieldCodec<int> = Field.create Decode.int Encode.int
  let int64: FieldCodec<int64> = Field.create Decode.int64 Encode.int64
  let float: FieldCodec<float> = Field.create Decode.float Encode.float
  let decimal: FieldCodec<decimal> = Field.create Decode.decimal Encode.decimal
  let bool: FieldCodec<bool> = Field.create Decode.bool Encode.bool
  let guid: FieldCodec<System.Guid> = Field.create Decode.guid Encode.guid

  /// An offset in the text decodes as UTC, dropping the offset. For data that has offsets, use `dateTimeOffset`.
  let dateTime (format: string) : FieldCodec<System.DateTime> =
    Field.create (Decode.dateTime format) (Encode.dateTime format)

  let dateTimeOffset (format: string) : FieldCodec<System.DateTimeOffset> =
    Field.create (Decode.dateTimeOffset format) (Encode.dateTimeOffset format)

  /// A Local value decodes back as the same instant in UTC.
  let dateTimeIso: FieldCodec<System.DateTime> =
    Field.create Decode.dateTimeIso Encode.dateTimeIso

  let dateTimeOffsetIso: FieldCodec<System.DateTimeOffset> =
    Field.create Decode.dateTimeOffsetIso Encode.dateTimeOffsetIso

#if NET6_0_OR_GREATER
  let dateOnly (format: string) : FieldCodec<System.DateOnly> =
    Field.create (Decode.dateOnly format) (Encode.dateOnly format)

  let timeOnly (format: string) : FieldCodec<System.TimeOnly> =
    Field.create (Decode.timeOnly format) (Encode.timeOnly format)

  let dateOnlyIso: FieldCodec<System.DateOnly> =
    Field.create Decode.dateOnlyIso Encode.dateOnlyIso

  let timeOnlyIso: FieldCodec<System.TimeOnly> =
    Field.create Decode.timeOnlyIso Encode.timeOnlyIso
#endif

  /// `missing` decodes to None and None encodes to `missing`.
  let optionWith (missing: string) (codec: FieldCodec<'a>) : FieldCodec<'a option> =
    Field.create (Decode.optionWith missing codec.Decode) (Encode.optionWith missing codec.Encode)

  /// `missing` decodes to ValueNone and ValueNone encodes to `missing`.
  let voptionWith (missing: string) (codec: FieldCodec<'a>) : FieldCodec<'a voption> =
    Field.create (Decode.voptionWith missing codec.Decode) (Encode.voptionWith missing codec.Encode)

  /// A blank field decodes to None; None encodes to an empty field.
  let option (codec: FieldCodec<'a>) : FieldCodec<'a option> = optionWith "" codec

  /// A blank field decodes to ValueNone; ValueNone encodes to an empty field.
  let voption (codec: FieldCodec<'a>) : FieldCodec<'a voption> = voptionWith "" codec

  /// ROUND-TRIPS ONLY IF NO TEXT IS ACCEPTED BY BOTH CODECS: WITH TWO string CODECS, Error "x" DECODES BACK AS Ok "x".
  let result (okCodec: FieldCodec<'a>) (errCodec: FieldCodec<'e>) : FieldCodec<Result<'a, 'e>> =
    Field.create (Decode.result okCodec.Decode errCodec.Decode) (Encode.result okCodec.Encode errCodec.Encode)

  /// ROUND-TRIPS ONLY IF NO TEXT IS ACCEPTED BY BOTH CODECS: WITH float THEN int, Choice2Of2 3 DECODES BACK AS Choice1Of2 3.0.
  let choiceOf2 (left: FieldCodec<'a>) (right: FieldCodec<'b>) : FieldCodec<Choice<'a, 'b>> =
    Field.create (Decode.choiceOf2 left.Decode right.Decode) (Encode.choiceOf2 left.Encode right.Encode)

  /// The named column, which must exist exactly once when decoding.
  let field (header: string) (codec: FieldCodec<'field>) (getter: 'record -> 'field) : PartialCodec<'record, 'field> =
    partial (Decode.field header codec.Decode) (Encode.field header codec.Encode getter)

  /// The `occurrence`th (0-based) column named `header`, for tables where a header repeats.
  /// COLUMNS ARE WRITTEN IN CODEC ORDER, SO LIST A HEADER'S OCCURRENCES IN ORDER OR THEY WON'T ROUND-TRIP.
  let fieldOccurrence
    (header: string)
    (occurrence: int)
    (codec: FieldCodec<'field>)
    (getter: 'record -> 'field)
    : PartialCodec<'record, 'field> =
    partial (Decode.fieldOccurrence header occurrence codec.Decode) (Encode.field header codec.Encode getter)

  /// Decodes None when the column is missing, the row is short, or the field matches `missing`; None encodes to `missing`.
  let optionalFieldWith
    (missing: string)
    (header: string)
    (codec: FieldCodec<'field>)
    (getter: 'record -> 'field option)
    : PartialCodec<'record, 'field option> =
    partial
      (Decode.optionalFieldWith missing header codec.Decode)
      (Encode.field header (Encode.optionWith missing codec.Encode) getter)

  /// Decodes None when the column is missing, the row is short, or the field is blank; None encodes to an empty field.
  let optionalField
    (header: string)
    (codec: FieldCodec<'field>)
    (getter: 'record -> 'field option)
    : PartialCodec<'record, 'field option> =
    optionalFieldWith "" header codec getter

  /// The listed columns, each of which must exist exactly once when decoding. A header listed twice is written once,
  /// and a key missing from the dictionary encodes to an empty field. For headers that repeat, use `multiDictionary`.
  let dictionary
    (headers: string list)
    (codec: FieldCodec<'a>)
    (getter: 'record -> Dictionary<string, 'a>)
    : PartialCodec<'record, Dictionary<string, 'a>> =
    let headers = List.distinct headers

    partial
      (Decode.dictionaryOf headers codec.Decode)
      (Encode.dictionary headers codec.Encode |> Encode.Row.contramap getter)

  /// Each header maps to every column it names. List a header once per column to write; decoding reads every
  /// column with a listed header, however many the table has.
  let multiDictionary
    (headers: string list)
    (codec: FieldCodec<'a>)
    (getter: 'record -> Dictionary<string, 'a list>)
    : PartialCodec<'record, Dictionary<string, 'a list>> =
    partial
      (Decode.multiDictionaryOf headers codec.Decode)
      (Encode.multiDictionary headers codec.Encode |> Encode.Row.contramap getter)

/// NO Bind: ENCODING NEEDS THE FULL COLUMN LIST BEFORE ANY ROW, SO LATER COLUMNS CAN'T DEPEND ON DECODED VALUES.
type RowCodecBuilder() =
  member _.Return(value: 'a) : PartialCodec<'record, 'a> = Codec.Row.succeed value
  member _.ReturnFrom(codec: PartialCodec<'record, 'a>) : PartialCodec<'record, 'a> = codec

  member _.BindReturn(codec: PartialCodec<'record, 'a>, f: 'a -> 'b) : PartialCodec<'record, 'b> =
    Codec.Row.mapPartial f codec

  member _.MergeSources(ca: PartialCodec<'record, 'a>, cb: PartialCodec<'record, 'b>) : PartialCodec<'record, 'a * 'b> =
    Codec.Row.map2 (fun a b -> a, b) ca cb

  member _.Run(codec: PartialCodec<'record, 'record>) : RowCodec<'record> = Codec.Row.ofPartial codec

[<AutoOpen>]
module RowCodecBuilderInstance =
  /// Combine columns with `let!` and `and!`, then `return` the whole record.
  let codec = RowCodecBuilder()
