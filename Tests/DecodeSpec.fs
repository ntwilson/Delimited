module Delimited.Specs.DecodeSpec

open NUnit.Framework
open Swensen.Unquote

open Delimited

type Reading = { Name: string; Temp: float; Count: int }

let reading =
  decoder {
    let! name = Decode.field "name" Field.string
    and! temp = Decode.field "temp" Field.float
    and! count = Decode.field "count" Field.int

    return { Name = name; Temp = temp; Count = count }
  }

let rows (lines: string list list) = lines |> Seq.map Seq.ofList

[<Test>]
let ``and! decodes a record`` () =
  let actual =
    Csv.decodeRows CsvOptions.csv reading (rows [ [ "name"; "temp"; "count" ]; [ "a"; "1.5"; "2" ] ])

  test <@ List.ofSeq actual = [ Ok { Name = "a"; Temp = 1.5; Count = 2 } ] @>

[<Test>]
let ``and! reports every bad field with its row number`` () =
  let actual =
    Csv.decodeRows
      CsvOptions.csv
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
            Problem = InvalidValue({ Index = 2; Header = Some "count" }, "many", "Expected an integer but got \"many\"")
          }
        ]
    ]

  test <@ actual = expected @>

[<Test>]
let ``let! short-circuits for dependent fields`` () =
  let d =
    decoder {
      let! kind = Decode.field "kind" Field.string

      match kind with
      | "int" -> return! Decode.field "value" Field.int |> Decode.map string
      | _ -> return! Decode.field "value" Field.string
    }

  let actual =
    Csv.decodeRows CsvOptions.csv d (rows [ [ "kind"; "value" ]; [ "int"; "007" ]; [ "text"; "007" ] ])

  test <@ List.ofSeq actual = [ Ok "7"; Ok "007" ] @>

[<Test>]
let ``missing column and short rows`` () =
  let header = [ "a"; "b" ]

  let missing =
    Csv.decodeRows CsvOptions.csv (Decode.field "c" Field.int) (rows [ header; [ "1"; "2" ] ])

  let expected: Result<int, DecodeError list> list =
    [ Error [ { Row = 1; Problem = ColumnNotFound "c" } ] ]

  test <@ List.ofSeq missing = expected @>

  let short =
    Csv.decodeRows CsvOptions.csv (Decode.field "b" Field.int) (rows [ header; [ "1" ] ])

  let expectedShort: Result<int, DecodeError list> list =
    [
      Error [ { Row = 1; Problem = MissingField({ Index = 1; Header = Some "b" }, 1) } ]
    ]

  test <@ List.ofSeq short = expectedShort @>

  test
    <@
      Csv.decodeRows
        CsvOptions.csv
        (Decode.optionalField "b" Field.int)
        (rows [ header; [ "1" ]; [ "1"; "" ]; [ "1"; "2" ] ])
      |> List.ofSeq
        =
        [ Ok None; Ok None; Ok(Some 2) ]
    @>

[<Test>]
let ``index works without headers`` () =
  let d = Decode.map2 (+) (Decode.index 0 Field.int) (Decode.index 1 Field.int)

  let actual =
    Csv.decodeRows (CsvOptions.withoutHeaders CsvOptions.csv) d (rows [ [ "1"; "2" ]; [ "3"; "4" ] ])

  test <@ List.ofSeq actual = [ Ok 3; Ok 7 ] @>

[<Test>]
let ``index errors name the header only when the file has one`` () =
  let withHeaders =
    Csv.decodeRows CsvOptions.csv (Decode.index 1 Field.int) (rows [ [ "a"; "b" ]; [ "1"; "x" ] ])

  let withoutHeaders =
    Csv.decodeRows (CsvOptions.withoutHeaders CsvOptions.csv) (Decode.index 1 Field.int) (rows [ [ "1"; "x" ] ])

  let invalid header : Result<int, DecodeError list> list =
    [
      Error
        [
          {
            Row = 1
            Problem = InvalidValue({ Index = 1; Header = header }, "x", "Expected an integer but got \"x\"")
          }
        ]
    ]

  test <@ List.ofSeq withHeaders = invalid (Some "b") @>
  test <@ List.ofSeq withoutHeaders = invalid None @>

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
let ``oneOf takes the first success`` () =
  let d =
    Decode.oneOf [ Decode.field "x" Field.int |> Decode.map float; Decode.field "x" Field.float ]

  let actual = Csv.decodeRows CsvOptions.csv d (rows [ [ "x" ]; [ "2" ]; [ "2.5" ] ])
  test <@ List.ofSeq actual = [ Ok 2.0; Ok 2.5 ] @>

[<Test>]
let ``dictionary maps headers to values`` () =
  let actual =
    Csv.decodeRows CsvOptions.csv Decode.dictionary (rows [ [ "a"; "b" ]; [ "1" ] ])
    |> Seq.map (Result.map (Seq.map (fun kv -> kv.Key, kv.Value) >> List.ofSeq))
    |> List.ofSeq

  test <@ actual = [ Ok [ "a", "1"; "b", "" ] ] @>
