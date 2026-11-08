module Delimited.Specs.CsvSpec

open System.Reflection

open NUnit.Framework
open Swensen.Unquote
// OPENED BEFORE Delimited ON PURPOSE: PROVES `Csv.` STILL RESOLVES TO OUR MODULE, NOT THE Sylvan.Data.Csv NAMESPACE.
open Sylvan.Data

open Delimited

let private publicFunctions moduleName =
  typeof<Source>.Assembly.GetType(moduleName, true).GetMethods(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.map (fun m ->
    m.Name,
    m.GetParameters()
    |> Array.map (fun p -> p.Name, string p.ParameterType)
    |> List.ofArray)
  |> Set.ofArray

[<Test>]
let ``Csv mirrors every Table function that takes options`` () =
  let expected =
    publicFunctions "Delimited.Table"
    |> Set.filter (fun (_, parameters) -> parameters |> List.tryHead |> Option.map fst = Some "options")
    |> Set.map (fun (name, parameters) -> name, List.tail parameters)

  test <@ publicFunctions "Delimited.Csv" = expected @>

type Note = { Id: int; Text: string }

let noteCodec: RowCodec<Note> =
  codec {
    let! id = Codec.field "id" Codec.int _.Id
    and! text = Codec.field "text" Codec.string _.Text
    return { Id = id; Text = text }
  }

[<Test>]
let ``Csv writes and reads comma-separated text with headers`` () =
  let notes = [ { Id = 1; Text = "a, b" }; { Id = 2; Text = "c" } ]
  let text = Csv.writeString noteCodec.Encode notes

  test <@ text.StartsWith "id,text" @>
  test <@ Csv.readAll noteCodec.Decode (Source.Text text) = (notes, []) @>
