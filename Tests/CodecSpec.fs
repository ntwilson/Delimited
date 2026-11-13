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
  test <@ roundTrip (Codec.voption Codec.int) (ValueSome 3) = Ok(ValueSome 3) @>
  test <@ roundTrip (Codec.voption Codec.int) ValueNone = Ok ValueNone @>

[<Test>]
let ``formatted date/time codecs round-trip`` () =
  let local = DateTime(2026, 3, 14, 10, 30, 0)
  let offset = DateTimeOffset(2026, 3, 14, 10, 30, 0, TimeSpan.FromHours -5.0)
  test <@ (Codec.dateTime "yyyy-MM-dd HH:mm").Encode local = "2026-03-14 10:30" @>
  test <@ roundTrip (Codec.dateTime "yyyy-MM-dd HH:mm") local = Ok local @>
  test <@ (Codec.dateTimeOffset "yyyy-MM-dd HH:mm zzz").Encode offset = "2026-03-14 10:30 -05:00" @>

  let minusFive = TimeSpan.FromHours -5.0
  let decoded = roundTrip (Codec.dateTimeOffset "yyyy-MM-dd HH:mm zzz") offset
  test <@ decoded |> Result.map (fun d -> d, d.Offset) = Ok(offset, minusFive) @>

[<Test>]
let ``ISO codecs write ISO 8601 and round-trip`` () =
  let utc = DateTime(2026, 3, 14, 10, 0, 0, 500, DateTimeKind.Utc)
  let offset = DateTimeOffset(2026, 3, 14, 10, 0, 0, TimeSpan.FromHours 2.0)
  test <@ Codec.dateTimeIso.Encode utc = "2026-03-14T10:00:00.5Z" @>
  test <@ Codec.dateTimeIso.Encode(DateTime(2026, 3, 14, 10, 0, 0)) = "2026-03-14T10:00:00" @>
  test <@ Codec.dateTimeOffsetIso.Encode offset = "2026-03-14T10:00:00+02:00" @>

  test <@ roundTrip Codec.dateTimeIso utc |> Result.map (fun d -> d, d.Kind) = Ok(utc, DateTimeKind.Utc) @>
  let twoHours = TimeSpan.FromHours 2.0
  test <@ roundTrip Codec.dateTimeOffsetIso offset |> Result.map _.Offset = Ok twoHours @>

#if !LIBRARY_NETSTANDARD
[<Test>]
let ``DateOnly and TimeOnly codecs round-trip`` () =
  test <@ roundTrip (Codec.dateOnly "yyyy-MM-dd") (DateOnly(2026, 11, 3)) = Ok(DateOnly(2026, 11, 3)) @>
  test <@ roundTrip (Codec.timeOnly "HH:mm:ss") (TimeOnly(13, 45, 30)) = Ok(TimeOnly(13, 45, 30)) @>
  test <@ Codec.dateOnlyIso.Encode(DateOnly(2026, 3, 14)) = "2026-03-14" @>
  test <@ Codec.timeOnlyIso.Encode(TimeOnly(13, 45)) = "13:45:00" @>
  test <@ roundTrip Codec.dateOnlyIso (DateOnly(2026, 3, 14)) = Ok(DateOnly(2026, 3, 14)) @>
  test <@ roundTrip Codec.timeOnlyIso (TimeOnly(13, 45, 30, 250)) = Ok(TimeOnly(13, 45, 30, 250)) @>
#endif

[<Test>]
let ``dateTimeIso round-trips a Local value as the same instant in UTC`` () =
  let local = DateTime(2026, 3, 14, 10, 0, 0, DateTimeKind.Local)
  let utc = local.ToUniversalTime()
  test <@ roundTrip Codec.dateTimeIso local = Ok utc @>

[<Test>]
let ``option round-trips None through a blank field`` () =
  test <@ roundTrip (Codec.option Codec.int) None = Ok None @>
  test <@ roundTrip (Codec.option Codec.int) (Some 3) = Ok(Some 3) @>

[<Test>]
let ``optionWith round-trips None through the missing string`` () =
  test <@ (Codec.optionWith "NA" Codec.string).Encode None = "NA" @>
  test <@ roundTrip (Codec.optionWith "NA" Codec.string) None = Ok None @>
  test <@ roundTrip (Codec.optionWith "NA" Codec.string) (Some "") = Ok(Some "") @>
  test <@ roundTrip (Codec.voptionWith "NA" Codec.int) ValueNone = Ok ValueNone @>

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
  let codec = Codec.choiceOf2 Codec.int Codec.string
  let values = [ Choice1Of2 1; Choice2Of2 "n/a" ]

  let text = Table.writeString (Encode.field "v" codec.Encode id) values

  let actual = Table.read (Decode.field "v" codec.Decode) (Source.Text text)

  test <@ List.ofSeq actual = [ Ok(Choice1Of2 1); Ok(Choice2Of2 "n/a") ] @>

type Reading = { Name: string; Temp: float; Count: int option }

let reading: RowCodec<Reading> =
  codec {
    let! name = Codec.field "name" Codec.string _.Name
    and! temp = Codec.field "temp" Codec.float _.Temp
    and! count = Codec.optionalField "count" Codec.int _.Count
    return { Name = name; Temp = temp; Count = count }
  }

type Site = { Id: int; Readings: Collections.Generic.Dictionary<string, float option> }

let site =
  codec {
    let! id = Codec.field "id" Codec.int _.Id

    and! readings =
      Codec.dictionary [ "jan"; "feb" ] (Codec.option Codec.float) _.Readings

    return { Id = id; Readings = readings }
  }

[<Test>]
let ``dictionary codec round-trips alongside other fields`` () =
  let lines = [ [ "id"; "jan"; "feb" ]; [ "1"; "2.5"; "" ] ]

  let decoded =
    Table.decodeRows site.Decode (Seq.map Seq.ofList lines)
    |> Seq.choose Result.toOption
    |> List.ofSeq

  let encoded =
    Table.encodeRows site.Encode decoded |> Seq.map List.ofSeq |> List.ofSeq

  let readings =
    decoded |> List.map (fun s -> s.Id, s.Readings["jan"], s.Readings["feb"])

  test <@ readings = [ 1, Some 2.5, None ] @>
  test <@ encoded = lines @>

[<Test>]
let ``dictionary codec encodes a missing key as an empty field`` () =
  let readings =
    Collections.Generic.Dictionary [ Collections.Generic.KeyValuePair("jan", Some 1.0) ]

  let encoded =
    Table.encodeRows site.Encode [ { Id = 1; Readings = readings } ]
    |> Seq.map List.ofSeq
    |> List.ofSeq

  test <@ encoded = [ [ "id"; "jan"; "feb" ]; [ "1"; "1"; "" ] ] @>

type Pair = { First: int; Second: int }

[<Test>]
let ``fieldOccurrence codecs round-trip a repeated header`` () =
  let pair =
    codec {
      let! first = Codec.fieldOccurrence "x" 0 Codec.int _.First
      and! second = Codec.fieldOccurrence "x" 1 Codec.int _.Second
      return { First = first; Second = second }
    }

  let lines = [ [ "x"; "x" ]; [ "1"; "2" ] ]

  let decoded = Table.decodeRows pair.Decode (Seq.map Seq.ofList lines) |> List.ofSeq

  let encoded =
    Table.encodeRows pair.Encode [ { First = 1; Second = 2 } ]
    |> Seq.map List.ofSeq
    |> List.ofSeq

  test <@ decoded = [ Ok { First = 1; Second = 2 } ] @>
  test <@ encoded = lines @>

type Monthly = { Id: int; Readings: Collections.Generic.Dictionary<string, int list> }

[<Test>]
let ``multiDictionary codec round-trips repeated headers`` () =
  let monthly =
    codec {
      let! id = Codec.field "id" Codec.int _.Id
      and! readings = Codec.multiDictionary [ "jan"; "feb"; "jan" ] Codec.int _.Readings
      return { Id = id; Readings = readings }
    }

  let lines = [ [ "id"; "jan"; "feb"; "jan" ]; [ "1"; "10"; "20"; "11" ] ]

  let decoded =
    Table.decodeRows monthly.Decode (Seq.map Seq.ofList lines)
    |> Seq.choose Result.toOption
    |> List.ofSeq

  let encoded =
    Table.encodeRows monthly.Encode decoded |> Seq.map List.ofSeq |> List.ofSeq

  test <@ decoded |> List.map (fun m -> m.Readings["jan"], m.Readings["feb"]) = [ [ 10; 11 ], [ 20 ] ] @>
  test <@ encoded = lines @>

[<Test>]
let ``row codec writes columns in declaration order`` () =
  let actual =
    Table.encodeRows reading.Encode [ { Name = "a"; Temp = 1.5; Count = None } ]
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

  let text = Table.writeString reading.Encode values

  let actual = Table.read reading.Decode (Source.Text text) |> List.ofSeq

  test <@ actual = List.map Ok values @>

[<Test>]
let ``row codec reports every bad field`` () =
  let actual =
    Table.read reading.Decode (Source.Text "name,temp,count\na,hot,many\n")
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  let expected: Result<Reading, DecodeProblem list> list =
    [
      Error
        [
          InvalidValue({ Index = 1; Header = Some "temp" }, "hot", "Expected a number but got \"hot\"")
          InvalidValue({ Index = 2; Header = Some "count" }, "many", "Expected a 32-bit integer but got \"many\"")
        ]
    ]

  test <@ actual = expected @>

[<Test>]
let ``row codec decodes a missing optional column as None`` () =
  let actual =
    Table.read reading.Decode (Source.Text "name,temp\na,1\n") |> List.ofSeq

  test <@ actual = [ Ok { Name = "a"; Temp = 1.0; Count = None } ] @>

type Tagged = Tagged of Reading

[<Test>]
let ``Codec.Row.map converts both directions`` () =
  let tagged = reading |> Codec.Row.map Tagged (fun (Tagged r) -> r)
  let value = Tagged { Name = "a"; Temp = 1.5; Count = Some 2 }
  let text = Table.writeString tagged.Encode [ value ]
  test <@ text.StartsWith "name,temp,count" @>
  test <@ Table.read tagged.Decode (Source.Text text) |> List.ofSeq = [ Ok value ] @>
