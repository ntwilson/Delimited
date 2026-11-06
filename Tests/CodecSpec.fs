module Delimited.Specs.CodecSpec

open System

open NUnit.Framework
open Swensen.Unquote

open Delimited

let roundTrip (codec: FieldCodec<'a>) (value: 'a) = codec.Decode(codec.Encode value)

[<Test>]
let ``primitive codecs round-trip`` () =
  test <@ roundTrip Codec.int -42 = Ok -42 @>
  test <@ roundTrip Codec.int64 9_000_000_000L = Ok 9_000_000_000L @>
  test <@ roundTrip Codec.float 0.1 = Ok 0.1 @>
  test <@ roundTrip Codec.decimal 12.345m = Ok 12.345m @>
  test <@ roundTrip Codec.bool true = Ok true @>
  test <@ roundTrip Codec.string "a,b" = Ok "a,b" @>
  let g = Guid.NewGuid()
  test <@ roundTrip Codec.guid g = Ok g @>
  test <@ roundTrip (Codec.dateOnly "yyyy-MM-dd") (DateOnly(2026, 11, 3)) = Ok(DateOnly(2026, 11, 3)) @>

[<Test>]
let ``option round-trips None through a blank field`` () =
  test <@ roundTrip (Codec.option Codec.int) None = Ok None @>
  test <@ roundTrip (Codec.option Codec.int) (Some 3) = Ok(Some 3) @>

[<Test>]
let ``andThen validates in both directions`` () =
  let positive =
    Codec.int
    |> Codec.Field.andThen (fun i -> if i > 0 then Ok(uint i) else Error "must be positive") int

  test <@ positive.Decode "5" = Ok 5u @>
  test <@ positive.Decode "-5" = Error "must be positive" @>
  test <@ positive.Encode 7u = "7" @>

[<Test>]
let ``result round-trips unambiguous values`` () =
  let codec = Codec.result Codec.float Codec.bool
  test <@ roundTrip codec (Ok 1.5) = Ok(Ok 1.5) @>
  test <@ roundTrip codec (Error false) = Ok(Error false) @>

[<Test>]
let ``result decodes an ambiguous Error back as Ok`` () =
  test <@ roundTrip (Codec.result Codec.int Codec.string) (Error "5") = Ok(Ok 5) @>

[<Test>]
let ``choiceOf2 round-trips unambiguous values`` () =
  let codec = Codec.choiceOf2 Codec.int Codec.bool
  test <@ roundTrip codec (Choice1Of2 7) = Ok(Choice1Of2 7) @>
  test <@ roundTrip codec (Choice2Of2 true) = Ok(Choice2Of2 true) @>

[<Test>]
let ``choiceOf2 decodes an ambiguous Choice2Of2 back as Choice1Of2`` () =
  test <@ roundTrip (Codec.choiceOf2 Codec.int Codec.string) (Choice2Of2 "5") = Ok(Choice1Of2 5) @>

[<Test>]
let ``a codec drives both a column encoder and decoder`` () =
  let options = TableOptions.csv
  let codec = Codec.choiceOf2 Codec.int Codec.string
  let values = [ Choice1Of2 1; Choice2Of2 "n/a" ]
  let text = Table.writeString options (Encode.field "v" codec.Encode id) values
  let actual = Table.read options (Decode.field "v" codec.Decode) (Source.Text text)
  test <@ List.ofSeq actual = [ Ok(Choice1Of2 1); Ok(Choice2Of2 "n/a") ] @>

type Reading = { Name: string; Temp: float; Count: int option }

let reading: RowCodec<Reading> =
  codec {
    let! name = Codec.field "name" Codec.string _.Name
    and! temp = Codec.field "temp" Codec.float _.Temp
    and! count = Codec.optionalField "count" Codec.int _.Count
    return { Name = name; Temp = temp; Count = count }
  }

[<Test>]
let ``row codec writes columns in declaration order`` () =
  let actual =
    Table.encodeRows TableOptions.csv reading.Encode [ { Name = "a"; Temp = 1.5; Count = None } ]
    |> Seq.map List.ofSeq
    |> List.ofSeq

  test <@ actual = [ [ "name"; "temp"; "count" ]; [ "a"; "1.5"; "" ] ] @>

[<Test>]
let ``row codec round-trips through text`` () =
  let values =
    [
      { Name = "a"; Temp = 1.5; Count = Some 2 }
      { Name = "b, c"; Temp = -0.1; Count = None }
    ]

  let text = Table.writeString TableOptions.csv reading.Encode values

  let actual =
    Table.read TableOptions.csv reading.Decode (Source.Text text) |> List.ofSeq

  test <@ actual = List.map Ok values @>

[<Test>]
let ``row codec reports every bad field`` () =
  let actual =
    Table.read TableOptions.csv reading.Decode (Source.Text "name,temp,count\na,hot,many\n")
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  let expected: Result<Reading, DecodeProblem list> list =
    [
      Error
        [
          InvalidValue({ Index = 1; Header = Some "temp" }, "hot", "Expected a number but got \"hot\"")
          InvalidValue({ Index = 2; Header = Some "count" }, "many", "Expected an integer but got \"many\"")
        ]
    ]

  test <@ actual = expected @>

[<Test>]
let ``row codec decodes a missing optional column as None`` () =
  let actual =
    Table.read TableOptions.csv reading.Decode (Source.Text "name,temp\na,1\n")
    |> List.ofSeq

  test <@ actual = [ Ok { Name = "a"; Temp = 1.0; Count = None } ] @>

type Tagged = Tagged of Reading

[<Test>]
let ``Codec.Row.map converts both directions`` () =
  let tagged = reading |> Codec.Row.map Tagged (fun (Tagged r) -> r)
  let value = Tagged { Name = "a"; Temp = 1.5; Count = Some 2 }
  let text = Table.writeString TableOptions.csv tagged.Encode [ value ]
  test <@ text.StartsWith "name,temp,count" @>
  test <@ Table.read TableOptions.csv tagged.Decode (Source.Text text) |> List.ofSeq = [ Ok value ] @>
