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
            Problem = InvalidValue({ Index = 2; Header = Some "count" }, "many", "Expected an integer but got \"many\"")
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
  let d =
    Decode.Row.map2 (+) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)

  let actual =
    Table.decodeRows (TableOptions.withoutHeaders TableOptions.csv) d (rows [ [ "1"; "2" ]; [ "3"; "4" ] ])

  test <@ List.ofSeq actual = [ Ok 3; Ok 7 ] @>

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
    Decode.Row.oneOf
      [
        Decode.field "x" Decode.int |> Decode.Row.map float
        Decode.field "x" Decode.float
      ]

  let actual = Table.decodeRows TableOptions.csv d (rows [ [ "x" ]; [ "2" ]; [ "2.5" ] ])
  test <@ List.ofSeq actual = [ Ok 2.0; Ok 2.5 ] @>

[<Test>]
let ``dictionary maps headers to values`` () =
  let actual =
    Table.decodeRows TableOptions.csv (Decode.dictionary Decode.string) (rows [ [ "a"; "b" ]; [ "1" ] ])
    |> Seq.map (Result.map (Seq.map (fun kv -> kv.Key, kv.Value) >> List.ofSeq))
    |> List.ofSeq

  test <@ actual = [ Ok [ "a", "1"; "b", "" ] ] @>

[<Test>]
let ``dictionary reports every bad field with its column`` () =
  let actual =
    Table.decodeRows TableOptions.csv (Decode.dictionary Decode.int) (rows [ [ "a"; "b"; "c" ]; [ "x"; "2"; "y" ] ])
    |> Seq.map (Result.mapError (List.map _.Problem))
    |> List.ofSeq

  let invalid index header value =
    InvalidValue({ Index = index; Header = Some header }, value, $"Expected an integer but got \"%s{value}\"")

  test <@ actual = [ Error [ invalid 0 "a" "x"; invalid 2 "c" "y" ] ] @>

[<Test>]
let ``bad field input reports what was expected`` () =
  test <@ Decode.int "abc" = Error "Expected an integer but got \"abc\"" @>

[<Test>]
let ``option treats blank as None`` () =
  test <@ Decode.option Decode.int "  " = Ok None @>
  test <@ Decode.option Decode.int "3" = Ok(Some 3) @>

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
  test <@ Decode.result Decode.int Decode.bool "abc" = Error "Expected an integer but got \"abc\"" @>

[<Test>]
let ``result prefers Ok when both decoders accept the field`` () =
  test <@ Decode.result Decode.int Decode.string "5" = Ok(Ok 5) @>

[<Test>]
let ``choiceOf2 decodes with whichever decoder accepts the field`` () =
  let d = Decode.choiceOf2 Decode.int Decode.bool
  test <@ d "5" = Ok(Choice1Of2 5) @>
  test <@ d "false" = Ok(Choice2Of2 false) @>
  test <@ d "abc" = Error "Expected an integer but got \"abc\"" @>

[<Test>]
let ``choiceOf2 prefers the left decoder when both accept the field`` () =
  test <@ Decode.choiceOf2 Decode.int Decode.string "5" = Ok(Choice1Of2 5) @>
