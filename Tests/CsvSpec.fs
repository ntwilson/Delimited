module Delimited.Specs.CsvSpec

open System.IO
open System.Text
open System.Threading

open FSharp.Control
open NUnit.Framework
open Swensen.Unquote

open Delimited

type Note = { Id: int; Text: string }

let noteEncoder: Encoder<Note> =
  Encode.columns [ Encode.field "id" Field.int _.Id; Encode.field "text" Field.string _.Text ]

let noteDecoder =
  decoder {
    let! id = Decode.field "id" Field.int
    and! text = Decode.field "text" Field.string
    return { Id = id; Text = text }
  }

let tricky =
  [
    { Id = 1; Text = "plain" }
    { Id = 2; Text = "has, comma | pipe\ttab" }
    { Id = 3; Text = "has \"quotes\"" }
    { Id = 4; Text = "multi\nline" }
    { Id = 5; Text = "" }
  ]

let readAll options source =
  (Csv.readOrFail options noteDecoder source |> TaskSeq.toListAsync).Result

[<Test>]
let ``writeString then read round-trips quoting`` () =
  let text = Csv.writeString CsvOptions.csv noteEncoder tricky
  test <@ readAll CsvOptions.csv (Source.Text text) = tricky @>

[<TestCase('|')>]
[<TestCase('\t')>]
[<TestCase(';')>]
let ``custom delimiter round-trips`` (delimiter: char) =
  let options = CsvOptions.withDelimiter delimiter CsvOptions.csv
  let text = Csv.writeString options noteEncoder tricky
  test <@ text.StartsWith $"id%c{delimiter}text" @>
  test <@ readAll options (Source.Text text) = tricky @>

[<Test>]
let ``auto-detects the delimiter when reading`` () =
  test
    <@
      readAll CsvOptions.autoDetect (Source.Text "id|text\n1|a\n2|b\n") =
        [ { Id = 1; Text = "a" }; { Id = 2; Text = "b" } ]
    @>

[<Test>]
let ``reads a caller's stream as IAsyncEnumerable and leaves it open`` () =
  use stream = new MemoryStream(Encoding.UTF8.GetBytes "id,text\n1,a\n2,b\n")

  let e =
    (Csv.readOrFail CsvOptions.csv noteDecoder (Source.Stream stream)).GetAsyncEnumerator(CancellationToken.None)

  let rec drain acc =
    if e.MoveNextAsync().AsTask().Result then
      drain (e.Current :: acc)
    else
      List.rev acc

  let actual = drain []
  e.DisposeAsync().AsTask().Wait()
  test <@ actual = [ { Id = 1; Text = "a" }; { Id = 2; Text = "b" } ] @>
  test <@ stream.CanRead @>

[<Test>]
let ``readSeq returns decode errors per row`` () =
  let actual =
    Csv.readSeq CsvOptions.csv noteDecoder (Source.Text "id,text\n1,a\nx,b\n")
    |> List.ofSeq

  test <@ actual.Length = 2 @>
  test <@ actual[0] = Ok { Id = 1; Text = "a" } @>
  test <@ actual[1] |> Result.mapError (List.map _.Row) = Error [ 2L ] @>

[<Test>]
let ``readOrFail throws on a bad row`` () =
  let ex =
    Assert.Throws<System.AggregateException>(fun () -> readAll CsvOptions.csv (Source.Text "id,text\nx,a\n") |> ignore)

  test <@ (ex.InnerException :? DecodeException) @>

[<Test>]
let ``writeAsync streams a taskSeq to a file`` () =
  let path = Path.GetTempFileName()

  try
    let source = taskSeq { for n in tricky -> n }
    (Csv.writeAsync CsvOptions.csv noteEncoder (Sink.File path) source CancellationToken.None).Wait()
    test <@ readAll CsvOptions.csv (Source.File path) = tricky @>
  finally
    File.Delete path

[<Test>]
let ``writing to a caller's writer leaves it open`` () =
  use w = new StringWriter()
  Csv.write CsvOptions.csv noteEncoder (Sink.Writer w) [ { Id = 1; Text = "a" } ]
  w.Write "more"
  test <@ w.ToString().EndsWith "more" @>

[<Test>]
let ``dynamic rows round-trip through files`` () =
  let headers = [ "x"; "y" ]

  let text =
    Csv.writeString CsvOptions.csv (Encode.fields headers) [ [| "1"; "2" |]; [| "3" |] ]

  let actual =
    (Csv.read CsvOptions.csv Decode.fields (Source.Text text) |> TaskSeq.toListAsync).Result

  test <@ actual = [ Ok [| "1"; "2" |]; Ok [| "3"; "" |] ] @>

[<Test>]
let ``withReader exposes Sylvan`` () =
  let count =
    (Csv.withReader CsvOptions.csv (Source.Text "a,b,c\n1,2,3\n") (fun r ->
      System.Threading.Tasks.Task.FromResult r.FieldCount))
      .Result

  test <@ count = 3 @>
