module Delimited.Specs.EncodeSpec

open System.Collections.Generic

open NUnit.Framework
open Swensen.Unquote

open Delimited

type Reading = { Name: string; Temp: float option }

let reading: RowEncoder<Reading> =
  Encode.columns
    [
      Encode.field "name" Encode.string _.Name
      Encode.field "temp" (Encode.option Encode.float) _.Temp
    ]

let encode options encoder xs =
  Table.encodeRows options encoder xs |> Seq.map List.ofSeq |> List.ofSeq

[<Test>]
let ``encodes records with a header row`` () =
  let actual =
    encode TableOptions.csv reading [ { Name = "a"; Temp = Some 1.5 }; { Name = "b"; Temp = None } ]

  test <@ actual = [ [ "name"; "temp" ]; [ "a"; "1.5" ]; [ "b"; "" ] ] @>

[<Test>]
let ``dictionary writes empty for a missing key`` () =
  let d = Dictionary<string, string>(dict [ "a", "1" ])

  let actual =
    encode TableOptions.csv (Encode.dictionary [ "a"; "b" ] Encode.string) [ d ]

  test <@ actual = [ [ "a"; "b" ]; [ "1"; "" ] ] @>

[<Test>]
let ``fields pads short rows`` () =
  let actual = encode TableOptions.csv (Encode.fields [ "a"; "b" ]) [ [| "1" |] ]
  test <@ actual = [ [ "a"; "b" ]; [ "1"; "" ] ] @>

[<Test>]
let ``option encodes None as an empty field`` () =
  test <@ Encode.option Encode.int None = "" @>
  test <@ Encode.option Encode.int (Some 3) = "3" @>

[<Test>]
let ``result encodes each case with its own encoder`` () =
  let e = Encode.result Encode.float Encode.bool
  test <@ e (Ok 1.5) = "1.5" @>
  test <@ e (Error false) = "false" @>

[<Test>]
let ``choiceOf2 encodes each case with its own encoder`` () =
  let e = Encode.choiceOf2 Encode.int Encode.bool
  test <@ e (Choice1Of2 7) = "7" @>
  test <@ e (Choice2Of2 true) = "true" @>

[<Test>]
let ``Field.contramap adapts an encoder`` () =
  test <@ Encode.Field.contramap (fun (s: string) -> s.Length) Encode.int "abc" = "3" @>
