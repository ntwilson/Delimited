namespace Delimited

open System
open System.Globalization

/// Converts a single field between its text form and 'T.
type FieldCodec<'T> = { Decode: string -> Result<'T, string>; Encode: 'T -> string }

module Field =
  let private inv = CultureInfo.InvariantCulture

  let custom (decode: string -> Result<'T, string>) (encode: 'T -> string) : FieldCodec<'T> =
    { Decode = decode; Encode = encode }

  /// Builds a codec from a .NET-style TryParse; `expected` describes the type in error messages.
  let ofTryParse (expected: string) (tryParse: string -> bool * 'T) (encode: 'T -> string) : FieldCodec<'T> =
    custom
      (fun s ->
        match tryParse s with
        | true, v -> Ok v
        | false, _ -> Error $"Expected %s{expected} but got \"%s{s}\"")
      encode

  /// Converts both directions; `decode` and `encode` should be inverses.
  let map (decode: 'A -> 'B) (encode: 'B -> 'A) (codec: FieldCodec<'A>) : FieldCodec<'B> =
    custom (codec.Decode >> Result.map decode) (encode >> codec.Encode)

  /// Like `map`, but decoding may fail with a message.
  let andThen (decode: 'A -> Result<'B, string>) (encode: 'B -> 'A) (codec: FieldCodec<'A>) : FieldCodec<'B> =
    custom (codec.Decode >> Result.bind decode) (encode >> codec.Encode)

  let string: FieldCodec<string> = custom Ok id

  let int: FieldCodec<int> =
    ofTryParse "an integer" (fun s -> Int32.TryParse(s, NumberStyles.Integer, inv)) (fun i -> i.ToString inv)

  let int64: FieldCodec<int64> =
    ofTryParse "an integer" (fun s -> Int64.TryParse(s, NumberStyles.Integer, inv)) (fun i -> i.ToString inv)

  let float: FieldCodec<float> =
    ofTryParse
      "a number"
      (fun s -> Double.TryParse(s, NumberStyles.Float ||| NumberStyles.AllowThousands, inv))
      (fun x -> x.ToString("R", inv))

  let decimal: FieldCodec<decimal> =
    ofTryParse "a decimal" (fun s -> Decimal.TryParse(s, NumberStyles.Number, inv)) (fun x -> x.ToString inv)

  let bool: FieldCodec<bool> =
    ofTryParse "true or false" (fun s -> Boolean.TryParse s) (fun b -> if b then "true" else "false")

  let guid: FieldCodec<Guid> =
    ofTryParse "a GUID" (fun s -> Guid.TryParse s) (fun g -> g.ToString())

  let dateTime (format: string) : FieldCodec<DateTime> =
    ofTryParse
      $"a date/time formatted as %s{format}"
      (fun s -> DateTime.TryParseExact(s, format, inv, DateTimeStyles.None))
      (fun d -> d.ToString(format, inv))

  let dateTimeOffset (format: string) : FieldCodec<DateTimeOffset> =
    ofTryParse
      $"a date/time offset formatted as %s{format}"
      (fun s -> DateTimeOffset.TryParseExact(s, format, inv, DateTimeStyles.None))
      (fun d -> d.ToString(format, inv))

#if NET6_0_OR_GREATER
  let dateOnly (format: string) : FieldCodec<DateOnly> =
    ofTryParse
      $"a date formatted as %s{format}"
      (fun s -> DateOnly.TryParseExact(s, format, inv, DateTimeStyles.None))
      (fun d -> d.ToString(format, inv))

  let timeOnly (format: string) : FieldCodec<TimeOnly> =
    ofTryParse
      $"a time formatted as %s{format}"
      (fun s -> TimeOnly.TryParseExact(s, format, inv, DateTimeStyles.None))
      (fun t -> t.ToString(format, inv))
#endif

  /// A blank field decodes to None; None encodes to an empty field.
  let option (codec: FieldCodec<'T>) : FieldCodec<'T option> =
    custom
      (fun s ->
        if String.IsNullOrWhiteSpace s then
          Ok None
        else
          codec.Decode s |> Result.map Some)
      (function
       | Some v -> codec.Encode v
       | None -> "")

  /// A blank field decodes to ValueNone; ValueNone encodes to an empty field.
  let voption (codec: FieldCodec<'T>) : FieldCodec<'T voption> =
    custom
      (fun s ->
        if String.IsNullOrWhiteSpace s then
          Ok ValueNone
        else
          codec.Decode s |> Result.map ValueSome)
      (function
       | ValueSome v -> codec.Encode v
       | ValueNone -> "")

  let result (okCodec: FieldCodec<'a>) (errCodec: FieldCodec<'e>) : FieldCodec<Result<'a, 'e>> =
    custom
      (fun s ->
        match okCodec.Decode s, errCodec.Decode s with
        | Ok a, _ -> Ok(Ok a)
        | Error _, Ok e -> Ok(Error e)
        | Error e, Error _ -> Error e)
      (function
       | Ok a -> okCodec.Encode a
       | Error e -> errCodec.Encode e)

  let choiceOf2 (left: FieldCodec<'a>) (right: FieldCodec<'b>) : FieldCodec<Choice<'a, 'b>> =
    result left right
    |> map
      (function
      | Ok a -> Choice1Of2 a
      | Error b -> Choice2Of2 b)
      (function
       | Choice1Of2 a -> Ok a
       | Choice2Of2 b -> Error b)
