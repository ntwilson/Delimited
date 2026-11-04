module Delimited.Specs.FieldSpec

open System

open NUnit.Framework
open Swensen.Unquote

open Delimited

let roundTrip (codec: FieldCodec<'T>) (value: 'T) = codec.Decode(codec.Encode value)

[<Test>]
let ``primitive codecs round-trip`` () =
  test <@ roundTrip Field.int -42 = Ok -42 @>
  test <@ roundTrip Field.int64 9_000_000_000L = Ok 9_000_000_000L @>
  test <@ roundTrip Field.float 0.1 = Ok 0.1 @>
  test <@ roundTrip Field.decimal 12.345m = Ok 12.345m @>
  test <@ roundTrip Field.bool true = Ok true @>
  test <@ roundTrip Field.string "a,b" = Ok "a,b" @>
  let g = Guid.NewGuid()
  test <@ roundTrip Field.guid g = Ok g @>
  test <@ roundTrip (Field.dateOnly "yyyy-MM-dd") (DateOnly(2026, 11, 3)) = Ok(DateOnly(2026, 11, 3)) @>

[<Test>]
let ``bad input reports what was expected`` () =
  test <@ Field.int.Decode "abc" = Error "Expected an integer but got \"abc\"" @>

[<Test>]
let ``option treats blank as None`` () =
  test <@ (Field.option Field.int).Decode "  " = Ok None @>
  test <@ (Field.option Field.int).Decode "3" = Ok(Some 3) @>
  test <@ (Field.option Field.int).Encode None = "" @>

[<Test>]
let ``andThen validates in both directions`` () =
  let positive =
    Field.int
    |> Field.andThen (fun i -> if i > 0 then Ok(uint i) else Error "must be positive") int

  test <@ positive.Decode "5" = Ok 5u @>
  test <@ positive.Decode "-5" = Error "must be positive" @>
  test <@ positive.Encode 7u = "7" @>

[<Test>]
let ``result decodes with whichever codec accepts the field`` () =
  let codec = Field.result Field.int Field.bool
  test <@ codec.Decode "5" = Ok(Ok 5) @>
  test <@ codec.Decode "true" = Ok(Error true) @>

[<Test>]
let ``result reports the Ok codec's error when neither accepts the field`` () =
  let codec = Field.result Field.int Field.bool
  test <@ codec.Decode "abc" = Error "Expected an integer but got \"abc\"" @>

[<Test>]
let ``result prefers Ok when both codecs accept the field`` () =
  let codec = Field.result Field.int Field.string
  test <@ codec.Decode "5" = Ok(Ok 5) @>
  // "5" ENCODES FROM Error BUT DECODES BACK AS Ok.
  test <@ roundTrip codec (Error "5") = Ok(Ok 5) @>

[<Test>]
let ``result encodes each case with its own codec`` () =
  let codec = Field.result Field.float Field.bool
  test <@ codec.Encode(Ok 1.5) = "1.5" @>
  test <@ codec.Encode(Error false) = "false" @>
  test <@ roundTrip codec (Ok 1.5) = Ok(Ok 1.5) @>
  test <@ roundTrip codec (Error false) = Ok(Error false) @>

[<Test>]
let ``choiceOf2 decodes with whichever codec accepts the field`` () =
  let codec = Field.choiceOf2 Field.int Field.bool
  test <@ codec.Decode "5" = Ok(Choice1Of2 5) @>
  test <@ codec.Decode "false" = Ok(Choice2Of2 false) @>
  test <@ codec.Decode "abc" = Error "Expected an integer but got \"abc\"" @>

[<Test>]
let ``choiceOf2 prefers the left codec when both accept the field`` () =
  let codec = Field.choiceOf2 Field.int Field.string
  test <@ codec.Decode "5" = Ok(Choice1Of2 5) @>
  test <@ roundTrip codec (Choice2Of2 "5") = Ok(Choice1Of2 5) @>

[<Test>]
let ``choiceOf2 round-trips unambiguous values`` () =
  let codec = Field.choiceOf2 Field.int Field.bool
  test <@ codec.Encode(Choice1Of2 7) = "7" @>
  test <@ codec.Encode(Choice2Of2 true) = "true" @>
  test <@ roundTrip codec (Choice1Of2 7) = Ok(Choice1Of2 7) @>
  test <@ roundTrip codec (Choice2Of2 true) = Ok(Choice2Of2 true) @>

[<Test>]
let ``choiceOf2 works as a column codec`` () =
  let options = CsvOptions.csv
  let encoder = Encode.field "v" (Field.choiceOf2 Field.int Field.string) id
  let values = [ Choice1Of2 1; Choice2Of2 "n/a" ]
  let text = Csv.writeString options encoder values

  let actual =
    Csv.readSeq options (Decode.field "v" (Field.choiceOf2 Field.int Field.string)) (Source.Text text)

  test <@ List.ofSeq actual = [ Ok(Choice1Of2 1); Ok(Choice2Of2 "n/a") ] @>
