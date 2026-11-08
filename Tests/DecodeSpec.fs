module Delimited.Specs.DecodeSpec

open NUnit.Framework
open Swensen.Unquote

open Delimited

type Reading = { Name: string; Temp: float; Count: int }

let reading =
  decoder {
    let! name = Decode.field "name" Decode.string
    and! temp = Decode.field "temp" Decode.float
    and! count = Decode.field "count" Decode.int

    return { Name = name; Temp = temp; Count = count }
  }

let rows (lines: string list list) = lines |> Seq.map Seq.ofList

[<Test>]
let ``and! decodes a record`` () =
  let actual =
    Table.decodeRows TableOptions.csv reading (rows [ [ "name"; "temp"; "count" ]; [ "a"; "1.5"; "2" ] ])

  test <@ List.ofSeq actual = [ Ok { Name = "a"; Temp = 1.5; Count = 2 } ] @>

[<Test>]
let ``and! reports every bad field with its row number`` () =
  let actual =
    Table.decodeRows
      TableOptions.csv
      reading
      (rows [ [ "name"; "temp"; "count" ]; [ "a"; "1"; "2" ]; [ "b"; "hot"; "many" ] ])
    |> List.ofSeq

  let expected: Result<Reading, DecodeError list> list =
    [
      Ok { Name = "a"; Temp = 1.0; Count = 2 }
      Error
        [
          {
            Row = 2
            Problem = InvalidValue({ Index = 1; Header = Some "temp" }, "hot", "Expected a number but got \"hot\"")
          }
          {
            Row = 2
            Problem =
              InvalidValue({ Index = 2; Header = Some "count" }, "many", "Expected a 32-bit integer but got \"many\"")
          }
        ]
    ]

  test <@ actual = expected @>

[<Test>]
let ``let! short-circuits for dependent fields`` () =
  let d =
    decoder {
      let! kind = Decode.field "kind" Decode.string

      match kind with
      | "int" -> return! Decode.field "value" Decode.int |> Decode.Row.map string
      | _ -> return! Decode.field "value" Decode.string
    }

  let actual =
    Table.decodeRows TableOptions.csv d (rows [ [ "kind"; "value" ]; [ "int"; "007" ]; [ "text"; "007" ] ])

  test <@ List.ofSeq actual = [ Ok "7"; Ok "007" ] @>

[<Test>]
let ``missing column and short rows`` () =
  let header = [ "a"; "b" ]

  let missing =
    Table.decodeRows TableOptions.csv (Decode.field "c" Decode.int) (rows [ header; [ "1"; "2" ] ])

  let expected: Result<int, DecodeError list> list =
    [ Error [ { Row = 1; Problem = ColumnNotFound "c" } ] ]

  test <@ List.ofSeq missing = expected @>

  let short =
    Table.decodeRows TableOptions.csv (Decode.field "b" Decode.int) (rows [ header; [ "1" ] ])

  let expectedShort: Result<int, DecodeError list> list =
    [
      Error [ { Row = 1; Problem = MissingField({ Index = 1; Header = Some "b" }, 1) } ]
    ]

  test <@ List.ofSeq short = expectedShort @>

  test
    <@
      Table.decodeRows
        TableOptions.csv
        (Decode.optionalField "b" Decode.int)
        (rows [ header; [ "1" ]; [ "1"; "" ]; [ "1"; "2" ] ])
      |> List.ofSeq
        =
        [ Ok None; Ok None; Ok(Some 2) ]
    @>

[<Test>]
let ``index works without headers`` () =
  let d = Decode.Row.map2 (+) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)

  let actual =
    Table.decodeRows (TableOptions.withoutHeaders TableOptions.csv) d (rows [ [ "1"; "2" ]; [ "3"; "4" ] ])

  test <@ List.ofSeq actual = [ Ok 3; Ok 7 ] @>

/// Fails if enumerated more than once, like lines streamed from a reader.
let oneShot (lines: string list list) =
  let enumerated = ref false

  { new System.Collections.Generic.IEnumerable<string seq> with
      member _.GetEnumerator() : System.Collections.Generic.IEnumerator<string seq> =
        if enumerated.Value then
          failwith "enumerated twice"

        enumerated.Value <- true
        (rows lines).GetEnumerator()
    interface System.Collections.IEnumerable with
      member this.GetEnumerator() : System.Collections.IEnumerator =
        (this :?> System.Collections.Generic.IEnumerable<string seq>).GetEnumerator()
  }

[<Test>]
let ``decodeRows reads a one-shot source once`` () =
  let withHeaders =
    Table.decodeRows
      TableOptions.csv
      (Decode.field "b" Decode.int)
      (oneShot [ [ "a"; "b" ]; [ "1"; "2" ]; [ "3"; "4" ] ])

  let withoutHeaders =
    Table.decodeRows
      (TableOptions.withoutHeaders TableOptions.csv)
      (Decode.index 1 Decode.int)
      (oneShot [ [ "1"; "2" ]; [ "3"; "4" ] ])

  test <@ List.ofSeq withHeaders = [ Ok 2; Ok 4 ] @>
  test <@ List.ofSeq withoutHeaders = [ Ok 2; Ok 4 ] @>

[<Test>]
let ``decodeRows doesn't touch the source until enumerated`` () =
  let touched = ref false

  let source =
    seq {
      touched.Value <- true
      yield! rows [ [ "a" ]; [ "1" ] ]
    }

  let _ = Table.decodeRows TableOptions.csv (Decode.field "a" Decode.int) source
  test <@ not touched.Value @>

[<Test>]
let ``index errors name the header only when the file has one`` () =
  let withHeaders =
    Table.decodeRows TableOptions.csv (Decode.index 1 Decode.int) (rows [ [ "a"; "b" ]; [ "1"; "x" ] ])

  let withoutHeaders =
    Table.decodeRows (TableOptions.withoutHeaders TableOptions.csv) (Decode.index 1 Decode.int) (rows [ [ "1"; "x" ] ])

  let invalid header : Result<int, DecodeError list> list =
    [
      Error
        [
          {
            Row = 1
            Problem = InvalidValue({ Index = 1; Header = header }, "x", "Expected a 32-bit integer but got \"x\"")
          }
        ]
    ]

  test <@ List.ofSeq withHeaders = invalid (Some "b") @>
  test <@ List.ofSeq withoutHeaders = invalid None @>

[<Test>]
let ``header-keyed decoders fail on a table without headers`` () =
  let decode (d: RowDecoder<'a>) =
    Table.decodeRows (TableOptions.withoutHeaders TableOptions.csv) (Decode.Row.map ignore d) (rows [ [ "1" ] ])
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  let field = decode (Decode.field "a" Decode.int)
  let optionalField = decode (Decode.optionalField "a" Decode.int)
  let dictionary = decode (Decode.dictionary Decode.int)

  test <@ field = [ Error [ NoHeaderRow ] ] @>
  test <@ optionalField = [ Error [ NoHeaderRow ] ] @>
  test <@ dictionary = [ Error [ NoHeaderRow ] ] @>

[<Test>]
let ``numbers accept thousands separators`` () =
  test <@ Decode.int "1,000" = Ok 1000 @>
  test <@ Decode.int64 "3,000,000,000" = Ok 3_000_000_000L @>
  test <@ Decode.float "1,000.5" = Ok 1000.5 @>
  test <@ Decode.decimal "1,000.5" = Ok 1000.5m @>

[<Test>]
let ``Row.fail fails every row with its message`` () =
  let actual =
    Table.decodeRows TableOptions.csv (Decode.Row.fail "nope": RowDecoder<int>) (rows [ [ "a" ]; [ "1" ]; [ "2" ] ])
    |> List.ofSeq

  test
    <@
      actual =
        [
          Error [ { Row = 1; Problem = RowFailed "nope" } ]
          Error [ { Row = 2; Problem = RowFailed "nope" } ]
        ]
    @>

[<Test>]
let ``a repeated header reads its first column`` () =
  let lines = rows [ [ "a"; "a" ]; [ "1"; "2" ] ]

  let field =
    Table.decodeRows TableOptions.csv (Decode.field "a" Decode.int) lines
    |> List.ofSeq

  let dictionary =
    Table.decodeRows TableOptions.csv (Decode.dictionary Decode.int |> Decode.Row.map (fun d -> d["a"], d.Count)) lines
    |> List.ofSeq

  test <@ field = [ Ok 1 ] @>
  test <@ dictionary = [ Ok(1, 1) ] @>

[<Test>]
let ``describe formats each problem`` () =
  let column = { Index = 1; Header = Some "temp" }
  test <@ DecodeError.describe { Row = 3; Problem = ColumnNotFound "temp" } = "Row 3: column \"temp\" not found" @>

  test
    <@
      DecodeError.describe { Row = 3; Problem = MissingField(column, 1) } =
        "Row 3: column 1 (\"temp\") is missing; the row has only 1 fields"
    @>

  test
    <@
      DecodeError.describe { Row = 3; Problem = InvalidValue({ column with Header = None }, "hot", "bad") } =
        "Row 3: column 1 has value \"hot\": bad"
    @>

  test <@ DecodeError.describe { Row = 3; Problem = RowFailed "nope" } = "Row 3: nope" @>

[<Test>]
let ``either takes the first success`` () =
  let d =
    Decode.Row.either (Decode.field "x" Decode.int |> Decode.Row.map float) (Decode.field "x" Decode.float)

  let actual =
    Table.decodeRows TableOptions.csv d (rows [ [ "x" ]; [ "2" ]; [ "2.5" ] ])

  test <@ List.ofSeq actual = [ Ok 2.0; Ok 2.5 ] @>

[<Test>]
let ``either reduced over a list reports every decoder's error in order`` () =
  let d =
    [ "a"; "b"; "c" ]
    |> List.map (fun h -> Decode.field h Decode.int)
    |> List.reduce Decode.Row.either

  let actual =
    Table.decodeRows TableOptions.csv d (rows [ [ "a"; "b"; "c" ]; [ "x"; "y"; "3" ]; [ "x"; "y"; "z" ] ])
    |> List.ofSeq

  test <@ actual.[0] = Ok 3 @>

  test
    <@
      match actual.[1] with
      | Error errors ->
        errors
        |> List.map (fun e ->
          match e.Problem with
          | InvalidValue(c, _, _) -> c.Header
          | _ -> None)
          =
          [ Some "a"; Some "b"; Some "c" ]
      | Ok _ -> false
    @>

[<Test>]
let ``dictionary maps headers to values and requires every field`` () =
  let actual =
    Table.decodeRows TableOptions.csv (Decode.dictionary Decode.string) (rows [ [ "a"; "b" ]; [ "1"; "2" ]; [ "3" ] ])
    |> Seq.map (Result.map (Seq.map (fun kv -> kv.Key, kv.Value) >> List.ofSeq))
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  test
    <@
      actual =
        [
          Ok [ "a", "1"; "b", "2" ]
          Error [ MissingField({ Index = 1; Header = Some "b" }, 1) ]
        ]
    @>

[<Test>]
let ``dictionaryOf reads only the listed columns and requires each`` () =
  let decode headers =
    Table.decodeRows
      TableOptions.csv
      (Decode.dictionaryOf headers Decode.int)
      (rows [ [ "a"; "b"; "c" ]; [ "1"; "2"; "3" ] ])
    |> Seq.map (Result.map (Seq.map (fun (KeyValue(k, v)) -> k, v) >> List.ofSeq >> List.sort))
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  let listed = decode [ "c"; "a" ]
  let missing = decode [ "a"; "x"; "y" ]

  test <@ listed = [ Ok [ "a", 1; "c", 3 ] ] @>
  test <@ missing = [ Error [ ColumnNotFound "x"; ColumnNotFound "y" ] ] @>

[<Test>]
let ``dictionary reports every bad field with its column`` () =
  let actual =
    Table.decodeRows TableOptions.csv (Decode.dictionary Decode.int) (rows [ [ "a"; "b"; "c" ]; [ "x"; "2"; "y" ] ])
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  let invalid index header value =
    InvalidValue({ Index = index; Header = Some header }, value, $"Expected a 32-bit integer but got \"%s{value}\"")

  test <@ actual = [ Error [ invalid 0 "a" "x"; invalid 2 "c" "y" ] ] @>

[<Test>]
let ``bad field input reports what was expected`` () =
  test <@ Decode.int "abc" = Error "Expected a 32-bit integer but got \"abc\"" @>
  test <@ Decode.int "3000000000" = Error "Expected a 32-bit integer but got \"3000000000\"" @>
  test <@ Decode.int64 "abc" = Error "Expected a 64-bit integer but got \"abc\"" @>

[<Test>]
let ``option treats blank as None`` () =
  test <@ Decode.option Decode.int "  " = Ok None @>
  test <@ Decode.option Decode.int "3" = Ok(Some 3) @>
  test <@ Decode.voption Decode.int "  " = Ok ValueNone @>
  test <@ Decode.voption Decode.int "3" = Ok(ValueSome 3) @>
  test <@ Decode.voption Decode.int "x" = Error "Expected a 32-bit integer but got \"x\"" @>

[<Test>]
let ``optionWith treats only the missing string as None`` () =
  test <@ Decode.optionWith "NA" Decode.string "NA" = Ok None @>
  test <@ Decode.optionWith "NA" Decode.string " NA " = Ok None @>
  test <@ Decode.optionWith "NA" Decode.string "" = Ok(Some "") @>
  test <@ Decode.optionWith "NA" Decode.string "na" = Ok(Some "na") @>
  test <@ Decode.voptionWith "NA" Decode.int "NA" = Ok ValueNone @>
  test <@ Decode.voptionWith "NA" Decode.int "" = Error "Expected a 32-bit integer but got \"\"" @>

[<Test>]
let ``optionalFieldWith reads the missing string and an absent column as None`` () =
  let actual =
    Table.decodeRows
      TableOptions.csv
      (Decode.Row.map2
        (fun a b -> a, b)
        (Decode.optionalFieldWith "-" "a" Decode.int)
        (Decode.optionalFieldWith "-" "b" Decode.int))
      (rows [ [ "a" ]; [ "-" ]; [ "3" ] ])
    |> List.ofSeq

  test <@ actual = [ Ok(None, None); Ok(Some 3, None) ] @>

[<Test>]
let ``Field.andThen validates a field`` () =
  let positive =
    Decode.int
    |> Decode.Field.andThen (fun i -> if i > 0 then Ok(uint i) else Error "must be positive")

  test <@ positive "5" = Ok 5u @>
  test <@ positive "-5" = Error "must be positive" @>

[<Test>]
let ``result decodes with whichever decoder accepts the field`` () =
  let d = Decode.result Decode.int Decode.bool
  test <@ d "5" = Ok(Ok 5) @>
  test <@ d "true" = Ok(Error true) @>

[<Test>]
let ``result reports the Ok decoder's error when neither accepts the field`` () =
  test <@ Decode.result Decode.int Decode.bool "abc" = Error "Expected a 32-bit integer but got \"abc\"" @>

[<Test>]
let ``result prefers Ok when both decoders accept the field`` () =
  test <@ Decode.result Decode.int Decode.string "5" = Ok(Ok 5) @>

[<Test>]
let ``choiceOf2 decodes with whichever decoder accepts the field`` () =
  let d = Decode.choiceOf2 Decode.int Decode.bool
  test <@ d "5" = Ok(Choice1Of2 5) @>
  test <@ d "false" = Ok(Choice2Of2 false) @>
  test <@ d "abc" = Error "Expected a 32-bit integer but got \"abc\", or Expected true or false but got \"abc\"" @>

[<Test>]
let ``Field.either takes the first success`` () =
  let d = Decode.Field.either (Decode.int |> Decode.Field.map float) Decode.float

  test <@ d "2" = Ok 2.0 @>
  test <@ d "2.5" = Ok 2.5 @>

[<Test>]
let ``Field.either reduced over a list joins every decoder's message in order`` () =
  let d =
    [ "yyyy-MM-dd"; "MM/dd/yyyy" ]
    |> List.map Decode.dateTime
    |> List.reduce Decode.Field.either

  test <@ d "03/14/2026" = Ok(System.DateTime(2026, 3, 14)) @>

  test
    <@
      d "x" =
        Error
          "Expected a date/time formatted as yyyy-MM-dd but got \"x\", or Expected a date/time formatted as MM/dd/yyyy but got \"x\""
    @>

[<Test>]
let ``dateTimeAuto accepts common formats`` () =
  test <@ Decode.dateTimeAuto "2026-03-14" = Ok(System.DateTime(2026, 3, 14)) @>
  test <@ Decode.dateTimeAuto "03/14/2026 13:45" = Ok(System.DateTime(2026, 3, 14, 13, 45, 0)) @>
  test <@ Decode.dateTimeAuto "nope" = Error "Expected a date/time but got \"nope\"" @>

[<Test>]
let ``dateTimeAuto keeps a UTC timestamp in UTC`` () =
  test
    <@ Decode.dateTimeAuto "2026-03-14T10:00:00Z" = Ok(System.DateTime(2026, 3, 14, 10, 0, 0, System.DateTimeKind.Utc)) @>

  test <@ Decode.dateTimeAuto "2026-03-14T10:00:00Z" |> Result.map _.Kind = Ok System.DateTimeKind.Utc @>

[<Test>]
let ``dateTimeOffsetAuto keeps the offset`` () =
  test
    <@
      Decode.dateTimeOffsetAuto "2026-03-14T10:00:00+02:00" =
        Ok(System.DateTimeOffset(2026, 3, 14, 10, 0, 0, System.TimeSpan.FromHours 2.0))
    @>

  test
    <@ Decode.dateTimeOffsetAuto "2026-03-14T10:00:00+02:00" |> Result.map _.Offset = Ok(System.TimeSpan.FromHours 2.0) @>

#if !LIBRARY_NETSTANDARD
[<Test>]
let ``dateOnlyAuto accepts common formats`` () =
  test <@ Decode.dateOnlyAuto "2026-03-14" = Ok(System.DateOnly(2026, 3, 14)) @>
  test <@ Decode.dateOnlyAuto "3/14/2026" = Ok(System.DateOnly(2026, 3, 14)) @>
  test <@ Decode.dateOnlyAuto "nope" = Error "Expected a date but got \"nope\"" @>

[<Test>]
let ``timeOnlyAuto accepts common formats`` () =
  test <@ Decode.timeOnlyAuto "13:45" = Ok(System.TimeOnly(13, 45)) @>
  test <@ Decode.timeOnlyAuto "1:45 PM" = Ok(System.TimeOnly(13, 45)) @>
  test <@ Decode.timeOnlyAuto "nope" = Error "Expected a time but got \"nope\"" @>
#endif

[<Test>]
let ``dateTimeIso accepts T, t, or a space and basic or extended format`` () =
  let inputs =
    [
      "2026-03-14T10:00"
      "2026-03-14t10:00"
      "2026-03-14 10:00"
      "2026-03-14T10"
      "20260314T1000"
      "20260314 100000"
    ]

  test <@ inputs |> List.map Decode.dateTimeIso = List.replicate 6 (Ok(System.DateTime(2026, 3, 14, 10, 0, 0))) @>

[<Test>]
let ``dateTimeIso accepts a fraction with a period or a comma`` () =
  let expected = Ok(System.DateTime(2026, 3, 14, 10, 0, 0, 250))
  test <@ Decode.dateTimeIso "2026-03-14T10:00:00.25" = expected @>
  test <@ Decode.dateTimeIso "2026-03-14T10:00:00,25" = expected @>
  test <@ Decode.dateTimeIso "20260314T100000,25" = expected @>

[<Test>]
let ``dateTimeIso converts every offset form to UTC`` () =
  let kindOf s =
    Decode.dateTimeIso s |> Result.map (fun d -> d, d.Kind)

  // 10:00 AT +02:00 IS 08:00 UTC
  let eight = Ok(System.DateTime(2026, 3, 14, 8, 0, 0), System.DateTimeKind.Utc)

  test
    <@
      [
        "2026-03-14T10:00+02:00"
        "2026-03-14T10:00+0200"
        "2026-03-14T10:00+02"
        "20260314T1000+0200"
      ]
      |> List.map kindOf
        =
        List.replicate 4 eight
    @>

  test <@ kindOf "2026-03-14T10:00Z" = Ok(System.DateTime(2026, 3, 14, 10, 0, 0), System.DateTimeKind.Utc) @>

  test <@ kindOf "2026-03-14T10:00" = Ok(System.DateTime(2026, 3, 14, 10, 0, 0), System.DateTimeKind.Unspecified) @>

[<Test>]
let ``dateTimeIso rejects date-only text, 24:00, mixed formats, and forms .NET can't express`` () =
  let inputs =
    [
      "2026-03-14"
      "2026-03-14T24:00"
      "2026-03-14T1000"
      "20260314T10:00"
      "2026-03-14T10:00z"
      "2026-W11-6T10:00"
      "2026-073T10:00"
      "2026-03-14T10.5"
      "03/14/2026 10:00"
    ]

  test <@ inputs |> List.filter (Decode.dateTimeIso >> Result.isOk) = [] @>

  test <@ Decode.dateTimeIso "2026-03-14" = Error "Expected an ISO 8601 date/time but got \"2026-03-14\"" @>

[<Test>]
let ``dateTimeOffsetIso keeps the offset and requires one`` () =
  let offsetOf s =
    Decode.dateTimeOffsetIso s |> Result.map (fun d -> d, d.Offset)

  let minusFive = System.TimeSpan.FromHours -5.0

  test
    <@
      offsetOf "2026-03-14 10:00:00.5-05:00" =
        Ok(System.DateTimeOffset(2026, 3, 14, 10, 0, 0, 500, minusFive), minusFive)
    @>

  test <@ offsetOf "20260314T1000-05" |> Result.map snd = Ok minusFive @>
  test <@ offsetOf "2026-03-14T10Z" |> Result.map snd = Ok System.TimeSpan.Zero @>
  test <@ Result.isError (Decode.dateTimeOffsetIso "2026-03-14T10:00:00") @>

#if !LIBRARY_NETSTANDARD
[<Test>]
let ``dateOnlyIso accepts basic and extended calendar dates only`` () =
  test <@ Decode.dateOnlyIso "2026-03-14" = Ok(System.DateOnly(2026, 3, 14)) @>
  test <@ Decode.dateOnlyIso "20260314" = Ok(System.DateOnly(2026, 3, 14)) @>

  test
    <@
      [ "2026-03"; "2026-W11-6"; "2026-073"; "3/14/2026"; "2026-03-14T10:00" ]
      |> List.filter (Decode.dateOnlyIso >> Result.isOk)
        =
        []
    @>

  test <@ Decode.dateOnlyIso "3/14/2026" = Error "Expected an ISO 8601 date but got \"3/14/2026\"" @>

[<Test>]
let ``timeOnlyIso accepts every precision, either fraction separator, and a leading T`` () =
  let cases =
    [
      "13", System.TimeOnly(13, 0)
      "13:45", System.TimeOnly(13, 45)
      "1345", System.TimeOnly(13, 45)
      "13:45:30", System.TimeOnly(13, 45, 30)
      "134530", System.TimeOnly(13, 45, 30)
      "T13:45", System.TimeOnly(13, 45)
      "13:45:30.25", System.TimeOnly(13, 45, 30, 250)
      "13:45:30,25", System.TimeOnly(13, 45, 30, 250)
    ]

  test <@ cases |> List.map (fst >> Decode.timeOnlyIso) = (cases |> List.map (snd >> Ok)) @>

[<Test>]
let ``timeOnlyIso rejects 24:00, offsets, mixed formats, and other formats`` () =
  test
    <@
      [ "24:00"; "13:45Z"; "13:45+02:00"; "13:4530"; "1:45 PM" ]
      |> List.filter (Decode.timeOnlyIso >> Result.isOk)
        =
        []
    @>

  test <@ Decode.timeOnlyIso "1:45 PM" = Error "Expected an ISO 8601 time but got \"1:45 PM\"" @>
#endif

[<Test>]
let ``choiceOf2 prefers the left decoder when both accept the field`` () =
  test <@ Decode.choiceOf2 Decode.int Decode.string "5" = Ok(Choice1Of2 5) @>

let caseInsensitive = TableOptions.withCaseInsensitiveHeaders TableOptions.csv

[<Test>]
let ``header case matters by default`` () =
  let actual =
    Table.decodeRows TableOptions.csv (Decode.field "name" Decode.string) (rows [ [ "Name" ]; [ "a" ] ])

  let expected: Result<string, DecodeError list> list =
    [ Error [ { Row = 1; Problem = ColumnNotFound "name" } ] ]

  test <@ List.ofSeq actual = expected @>

[<Test>]
let ``withCaseInsensitiveHeaders matches fields regardless of case`` () =
  let actual =
    Table.decodeRows caseInsensitive reading (rows [ [ "NAME"; "Temp"; "count" ]; [ "a"; "1.5"; "2" ] ])

  test <@ List.ofSeq actual = [ Ok { Name = "a"; Temp = 1.5; Count = 2 } ] @>

[<Test>]
let ``withCaseInsensitiveHeaders applies to optional fields`` () =
  let actual =
    Table.decodeRows caseInsensitive (Decode.optionalField "b" Decode.int) (rows [ [ "a"; "B" ]; [ "1"; "2" ] ])

  test <@ List.ofSeq actual = [ Ok(Some 2) ] @>

[<Test>]
let ``withCaseInsensitiveHeaders applies when reading text`` () =
  let actual =
    Table.read caseInsensitive (Decode.field "name" Decode.string) (Source.Text "Name\na\n")
    |> List.ofSeq

  test <@ actual = [ Ok "a" ] @>

[<Test>]
let ``headers differing only in case keep the first column when matching case-insensitively`` () =
  let actual =
    Table.decodeRows caseInsensitive (Decode.field "id" Decode.int) (rows [ [ "Id"; "ID" ]; [ "1"; "2" ] ])

  test <@ List.ofSeq actual = [ Ok 1 ] @>

[<Test>]
let ``case-insensitive dictionaries read each column once and look up keys in any case`` () =
  let lookups (d: System.Collections.Generic.Dictionary<string, int>) = d.Count, d["A"], d["b"]

  let listed =
    Table.decodeRows
      caseInsensitive
      (Decode.dictionaryOf [ "a"; "A"; "B" ] Decode.int |> Decode.Row.map lookups)
      (rows [ [ "a"; "B" ]; [ "1"; "2" ] ])

  let every =
    Table.decodeRows
      caseInsensitive
      (Decode.dictionary Decode.int |> Decode.Row.map lookups)
      (rows [ [ "a"; "A"; "B" ]; [ "1"; "9"; "2" ] ])

  test <@ List.ofSeq listed = [ Ok(2, 1, 2) ] @>
  test <@ List.ofSeq every = [ Ok(2, 1, 2) ] @>
