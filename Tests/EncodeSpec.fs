module Delimited.Specs.EncodeSpec

open System.Collections.Generic
open System.Dynamic

open NUnit.Framework
open Swensen.Unquote

open Delimited

type Reading = { Name: string; Temp: float option }

let reading: Encoder<Reading> =
  Encode.columns
    [
      Encode.field "name" Field.string _.Name
      Encode.field "temp" (Field.option Field.float) _.Temp
    ]

let encode options encoder xs =
  Csv.encodeRows options encoder xs |> Seq.map List.ofSeq |> List.ofSeq

[<Test>]
let ``encodes records with a header row`` () =
  let actual =
    encode CsvOptions.csv reading [ { Name = "a"; Temp = Some 1.5 }; { Name = "b"; Temp = None } ]

  test <@ actual = [ [ "name"; "temp" ]; [ "a"; "1.5" ]; [ "b"; "" ] ] @>

[<Test>]
let ``dictionary writes empty for a missing key`` () =
  let d = Dictionary<string, string>(dict [ "a", "1" ])
  let actual = encode CsvOptions.csv (Encode.dictionary [ "a"; "b" ]) [ d ]
  test <@ actual = [ [ "a"; "b" ]; [ "1"; "" ] ] @>

[<Test>]
let ``objDictionary encodes ExpandoObject`` () =
  let x = ExpandoObject() :> IDictionary<string, obj>
  x["a"] <- box 1.5
  x["b"] <- null

  let actual =
    encode (CsvOptions.withoutHeaders CsvOptions.csv) (Encode.objDictionary [ "a"; "b"; "c" ]) [ x ]

  test <@ actual = [ [ "1.5"; ""; "" ] ] @>

[<Test>]
let ``fields pads short rows`` () =
  let actual = encode CsvOptions.csv (Encode.fields [ "a"; "b" ]) [ [| "1" |] ]
  test <@ actual = [ [ "a"; "b" ]; [ "1"; "" ] ] @>
