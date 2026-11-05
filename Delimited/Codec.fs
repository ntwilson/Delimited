namespace Delimited

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

  let dateTime (format: string) : FieldCodec<System.DateTime> =
    Field.create (Decode.dateTime format) (Encode.dateTime format)

  let dateTimeOffset (format: string) : FieldCodec<System.DateTimeOffset> =
    Field.create (Decode.dateTimeOffset format) (Encode.dateTimeOffset format)

#if NET6_0_OR_GREATER
  let dateOnly (format: string) : FieldCodec<System.DateOnly> =
    Field.create (Decode.dateOnly format) (Encode.dateOnly format)

  let timeOnly (format: string) : FieldCodec<System.TimeOnly> =
    Field.create (Decode.timeOnly format) (Encode.timeOnly format)
#endif

  /// A blank field decodes to None; None encodes to an empty field.
  let option (codec: FieldCodec<'a>) : FieldCodec<'a option> =
    Field.create (Decode.option codec.Decode) (Encode.option codec.Encode)

  /// A blank field decodes to ValueNone; ValueNone encodes to an empty field.
  let voption (codec: FieldCodec<'a>) : FieldCodec<'a voption> =
    Field.create (Decode.voption codec.Decode) (Encode.voption codec.Encode)

  /// AN Error VALUE THAT THE Ok CODEC ALSO ACCEPTS DECODES BACK AS Ok.
  let result (okCodec: FieldCodec<'a>) (errCodec: FieldCodec<'e>) : FieldCodec<Result<'a, 'e>> =
    Field.create (Decode.result okCodec.Decode errCodec.Decode) (Encode.result okCodec.Encode errCodec.Encode)

  /// A Choice2Of2 VALUE THAT THE LEFT CODEC ALSO ACCEPTS DECODES BACK AS Choice1Of2.
  let choiceOf2 (left: FieldCodec<'a>) (right: FieldCodec<'b>) : FieldCodec<Choice<'a, 'b>> =
    Field.create (Decode.choiceOf2 left.Decode right.Decode) (Encode.choiceOf2 left.Encode right.Encode)

  /// The named column, which must exist when decoding.
  let field (header: string) (codec: FieldCodec<'field>) (getter: 'record -> 'field) : PartialCodec<'record, 'field> =
    partial (Decode.field header codec.Decode) (Encode.field header codec.Encode getter)

  /// Decodes None when the column is missing, the row is short, or the field is blank; None encodes to an empty field.
  let optionalField
    (header: string)
    (codec: FieldCodec<'field>)
    (getter: 'record -> 'field option)
    : PartialCodec<'record, 'field option> =
    partial (Decode.optionalField header codec.Decode) (Encode.field header (Encode.option codec.Encode) getter)

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
