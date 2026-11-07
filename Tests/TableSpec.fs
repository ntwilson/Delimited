module Delimited.Specs.TableSpec

open System.IO
open System.Text
open System.Threading

open FSharp.Control
open NUnit.Framework
open Swensen.Unquote

open Delimited

[<Test>]
let ``tests run against the intended library build`` () =
  let framework =
    typeof<Source>.Assembly.GetCustomAttributes(typeof<System.Runtime.Versioning.TargetFrameworkAttribute>, false)
    |> Array.exactlyOne
    :?> System.Runtime.Versioning.TargetFrameworkAttribute

#if LIBRARY_NETSTANDARD
  test <@ framework.FrameworkName = ".NETStandard,Version=v2.1" @>
#else
  test <@ framework.FrameworkName = ".NETCoreApp,Version=v6.0" @>
#endif

type Note = { Id: int; Text: string }

let noteCodec: RowCodec<Note> =
  codec {
    let! id = Codec.field "id" Codec.int _.Id
    and! text = Codec.field "text" Codec.string _.Text
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

let readOrFailList options source =
  (Table.readOrFailAsync options noteCodec.Decode source |> TaskSeq.toListAsync).Result

[<Test>]
let ``writeString then read round-trips quoting`` () =
  let text = Table.writeString TableOptions.csv noteCodec.Encode tricky
  test <@ readOrFailList TableOptions.csv (Source.Text text) = tricky @>

[<TestCase('|')>]
[<TestCase('\t')>]
[<TestCase(';')>]
let ``custom delimiter round-trips`` (delimiter: char) =
  let options = TableOptions.withDelimiter delimiter TableOptions.csv
  let text = Table.writeString options noteCodec.Encode tricky
  test <@ text.StartsWith $"id%c{delimiter}text" @>
  test <@ readOrFailList options (Source.Text text) = tricky @>

[<Test>]
let ``auto-detects the delimiter when reading`` () =
  test
    <@
      readOrFailList TableOptions.autoDetect (Source.Text "id|text\n1|a\n2|b\n") =
        [ { Id = 1; Text = "a" }; { Id = 2; Text = "b" } ]
    @>

[<Test>]
let ``reads a caller's stream as IAsyncEnumerable and leaves it open`` () =
  use stream = new MemoryStream(Encoding.UTF8.GetBytes "id,text\n1,a\n2,b\n")

  let e =
    (Table.readOrFailAsync TableOptions.csv noteCodec.Decode (Source.Stream stream))
      .GetAsyncEnumerator(CancellationToken.None)

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
let ``read returns decode errors per row`` () =
  let actual =
    Table.read TableOptions.csv noteCodec.Decode (Source.Text "id,text\n1,a\nx,b\n")
    |> List.ofSeq

  test <@ actual.Length = 2 @>
  test <@ actual[0] = Ok { Id = 1; Text = "a" } @>
  test <@ actual[1] |> Result.mapError (List.map _.Row) = Error [ 2L ] @>

[<Test>]
let ``readOrFailAsync throws on a bad row`` () =
  let ex =
    Assert.Throws<System.AggregateException>(fun () ->
      readOrFailList TableOptions.csv (Source.Text "id,text\nx,a\n") |> ignore)

  test <@ (ex.InnerException :? DecodeException) @>

let mixedRows = Source.Text "id,text\n1,a\nx,b\n3,c\ny,d\n"

[<Test>]
let ``readAll returns every row when all decode`` () =
  test
    <@
      Table.readAll TableOptions.csv noteCodec.Decode (Source.Text "id,text\n1,a\n2,b\n") =
        ([ { Id = 1; Text = "a" }; { Id = 2; Text = "b" } ], [])
    @>

[<Test>]
let ``readAll returns the passing rows and the errors from every failing row in order`` () =
  let values, errors = Table.readAll TableOptions.csv noteCodec.Decode mixedRows
  test <@ values = [ { Id = 1; Text = "a" }; { Id = 3; Text = "c" } ] @>
  test <@ errors |> List.map _.Row = [ 2L; 4L ] @>

let silentFailure: RowDecoder<Note> = fun _ -> Error []

[<Test>]
let ``decodeRows reports a row whose decoder fails with no errors`` () =
  let rowsOf options rows =
    Table.decodeRows options silentFailure rows
    |> Seq.map (Result.mapError (List.map _.Row))
    |> List.ofSeq

  let fields = [ [ "1"; "a" ]; [ "2"; "b" ] ] |> List.map Seq.ofList
  let withHeaders = Seq.ofList (Seq.ofList [ "id"; "text" ] :: fields)
  let noHeaders = { TableOptions.csv with HasHeaders = false }

  test <@ rowsOf TableOptions.csv withHeaders = [ Error [ 1L ]; Error [ 2L ] ] @>
  test <@ rowsOf noHeaders (Seq.ofList fields) = [ Error [ 1L ]; Error [ 2L ] ] @>

[<Test>]
let ``readAll reports a row whose decoder fails with no errors`` () =
  let values, errors = Table.readAll TableOptions.csv silentFailure mixedRows
  test <@ values = [] @>
  test <@ errors |> List.map _.Row = [ 1L; 2L; 3L; 4L ] @>

[<Test>]
let ``readAllAsync reports a row whose decoder fails with no errors`` () =
  let values, errors =
    (Table.readAllAsync TableOptions.csv silentFailure mixedRows).Result

  test <@ values = [] @>
  test <@ errors |> List.map _.Row = [ 1L; 2L; 3L; 4L ] @>

[<Test>]
let ``readOrFail gives a message when the decoder fails with no errors`` () =
  let ex =
    Assert.Throws<DecodeException>(fun () ->
      Table.readOrFail TableOptions.csv silentFailure mixedRows |> Seq.iter ignore)

  test <@ ex.Message <> "" @>

[<Test>]
let ``readAll of an empty table is empty`` () =
  test <@ Table.readAll TableOptions.csv noteCodec.Decode (Source.Text "id,text\n") = ([], []) @>

[<Test>]
let ``readAllAsync returns every row when all decode`` () =
  let actual =
    (Table.readAllAsync TableOptions.csv noteCodec.Decode (Source.Text "id,text\n1,a\n2,b\n")).Result

  test <@ actual = ([ { Id = 1; Text = "a" }; { Id = 2; Text = "b" } ], []) @>

[<Test>]
let ``readAllAsync returns the passing rows and the errors from every failing row in order`` () =
  let values, errors =
    (Table.readAllAsync TableOptions.csv noteCodec.Decode mixedRows).Result

  test <@ values = [ { Id = 1; Text = "a" }; { Id = 3; Text = "c" } ] @>
  test <@ errors |> List.map _.Row = [ 2L; 4L ] @>

[<Test>]
let ``writeAsync streams a taskSeq to a file`` () =
  let path = Path.GetTempFileName()

  try
    let source = taskSeq { for n in tricky -> n }
    (Table.writeAsync TableOptions.csv noteCodec.Encode (Sink.File path) source CancellationToken.None).Wait()
    test <@ readOrFailList TableOptions.csv (Source.File path) = tricky @>
  finally
    File.Delete path

[<Test>]
let ``writing to a caller's stream leaves it open`` () =
  use stream = new MemoryStream()

  Table.write TableOptions.csv noteCodec.Encode (Sink.Stream stream) [ { Id = 1; Text = "a" } ]
  |> ignore

  test <@ stream.CanWrite @>
  stream.Position <- 0L
  test <@ readOrFailList TableOptions.csv (Source.Stream stream) = [ { Id = 1; Text = "a" } ] @>

[<Test>]
let ``reads a caller's reader and leaves it open`` () =
  use reader = new StringReader("id,text\n1,a\n")

  let actual =
    Table.readOrFail TableOptions.csv noteCodec.Decode (Source.Reader reader)
    |> List.ofSeq

  test <@ actual = [ { Id = 1; Text = "a" } ] @>
  // A DISPOSED StringReader THROWS HERE INSTEAD OF RETURNING -1.
  test <@ reader.Peek() = -1 @>

[<Test>]
let ``writing to a caller's writer leaves it open`` () =
  use w = new StringWriter()

  Table.write TableOptions.csv noteCodec.Encode (Sink.Writer w) [ { Id = 1; Text = "a" } ]
  |> ignore

  w.Write "more"
  test <@ w.ToString().EndsWith "more" @>

[<Test>]
let ``dynamic rows round-trip through files`` () =
  let headers = [ "x"; "y" ]

  let text =
    Table.writeString TableOptions.csv (Encode.fields headers) [ [| "1"; "2" |]; [| "3" |] ]

  let actual =
    (Table.readAsync TableOptions.csv Decode.fields (Source.Text text)
     |> TaskSeq.toListAsync)
      .Result

  test <@ actual = [ Ok [| "1"; "2" |]; Ok [| "3"; "" |] ] @>

let noteTable () =
  let table = new System.Data.DataTable()
  table.Columns.Add("id", typeof<int>) |> ignore
  table.Columns.Add("text", typeof<string>) |> ignore
  tricky |> List.iter (fun n -> table.Rows.Add(n.Id, n.Text) |> ignore)
  table

[<Test>]
let ``writeDataReader round-trips a DbDataReader`` () =
  use table = noteTable ()
  use w = new StringWriter()

  Table.writeDataReader TableOptions.csv (Sink.Writer w) (table.CreateDataReader())
  |> ignore

  test <@ readOrFailList TableOptions.csv (Source.Text(string w)) = tricky @>

[<Test>]
let ``writeDataReaderAsync round-trips a DbDataReader`` () =
  use table = noteTable ()
  use w = new StringWriter()

  (Table.writeDataReaderAsync TableOptions.csv (Sink.Writer w) (table.CreateDataReader()) CancellationToken.None).Wait()

  test <@ readOrFailList TableOptions.csv (Source.Text(string w)) = tricky @>

[<Test>]
let ``withReaderAsync exposes Sylvan`` () =
  let count =
    (Table.withReaderAsync TableOptions.csv (Source.Text "a,b,c\n1,2,3\n") (fun r ->
      System.Threading.Tasks.Task.FromResult r.FieldCount))
      .Result

  test <@ count = 3 @>
