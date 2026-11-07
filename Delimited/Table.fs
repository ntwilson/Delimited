namespace Delimited

open System
open System.Collections.Generic
open System.Data.Common
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks

open FSharp.Control
open Sylvan.Data
open Sylvan.Data.Csv

/// Where to read from. A Stream or TextReader you pass in stays open; you own it.
[<RequireQualifiedAccess>]
type Source =
  | File of path: string
  | Stream of Stream
  | Reader of TextReader
  | Text of string

/// Where to write to. A Stream or TextWriter you pass in stays open; you own it.
[<RequireQualifiedAccess>]
type Sink =
  | File of path: string
  | Stream of Stream
  | Writer of TextWriter

/// Disposing flushes but never closes the inner writer.
type internal KeepOpenWriter(inner: TextWriter) =
  inherit TextWriter(inner.FormatProvider)
  override _.Encoding = inner.Encoding

  override _.NewLine
    with get () = inner.NewLine
    and set v = inner.NewLine <- v

  override _.Write(c: char) = inner.Write c
  override _.Write(buffer: char[], index: int, count: int) = inner.Write(buffer, index, count)
  override _.Write(s: string) = inner.Write s
  override _.WriteAsync(buffer: char[], index: int, count: int) = inner.WriteAsync(buffer, index, count)
  override _.WriteAsync(buffer: ReadOnlyMemory<char>, ct: CancellationToken) = inner.WriteAsync(buffer, ct)
  override _.Flush() = inner.Flush()
  override _.FlushAsync() = inner.FlushAsync()
  override _.Dispose(_: bool) = inner.Flush()

module Table =
  let private noop =
    { new IDisposable with
        member _.Dispose() = ()
    }

  /// The reader, plus what to dispose when done (a no-op for caller-owned readers).
  let private openReader (source: Source) : struct (TextReader * IDisposable) =
    match source with
    | Source.File path ->
      let r = new StreamReader(path)
      struct (r, r)
    | Source.Stream s ->
      let r = new StreamReader(s, Encoding.UTF8, true, 4096, true)
      struct (r, r)
    | Source.Reader r -> struct (r, noop)
    | Source.Text text ->
      let r = new StringReader(text)
      struct (r, r)

  let private openWriter (sink: Sink) : struct (TextWriter * IDisposable) =
    match sink with
    | Sink.File path ->
      let w = new StreamWriter(path)
      struct (w, w)
    | Sink.Stream s ->
      let w = new StreamWriter(s, UTF8Encoding false, 4096, true)
      struct (w, w)
    | Sink.Writer w -> struct (w, noop)

  let private headersOf (options: TableOptions) (csv: CsvDataReader) =
    if options.HasHeaders then
      Some(Array.init csv.FieldCount csv.GetName)
    else
      None

  let private rowOf (number: int64) headers ordinals (csv: CsvDataReader) : Row =
    {
      Number = number
      Headers = headers
      Ordinals = ordinals
      FieldCount = csv.RowFieldCount
      GetString = csv.GetString
    }

  // RowDecoder IS A PLAIN FUNCTION, SO NOTHING STOPS ONE RETURNING Error []; WITHOUT THIS THE ROW WOULD VANISH.
  let private decodeRow (decoder: RowDecoder<'a>) (row: Row) : Result<'a, DecodeError list> =
    match decoder row with
    | Error [] ->
      Error
        [
          {
            Row = row.Number
            Problem = RowFailed "The decoder failed without reporting an error"
          }
        ]
    | result -> result

  let private orFail (result: Result<'a, DecodeError list>) : 'a =
    match result with
    | Ok v -> v
    | Error errors -> raise (DecodeException errors)

  /// Streams decoded rows. The source is opened when enumeration starts and closed when it ends.
  let readAsync
    (options: TableOptions)
    (decoder: RowDecoder<'a>)
    (source: Source)
    : TaskSeq<Result<'a, DecodeError list>> =
    taskSeq {
      let struct (textReader, owner) = openReader source
      use _ = owner

      let! csv =
        CsvDataReader.CreateAsync(textReader, TableOptions.toReaderOptions options)

      use _ = csv
      let headers = headersOf options csv
      let ordinals = Row.ordinalsOf headers
      let mutable number = 0L
      let mutable more = true

      while more do
        let! hasRow = csv.ReadAsync()

        if hasRow then
          number <- number + 1L
          yield decodeRow decoder (rowOf number headers ordinals csv)
        else
          more <- false
    }

  /// Like `readAsync`, but throws a DecodeException at the first row that fails.
  let readOrFailAsync (options: TableOptions) (decoder: RowDecoder<'a>) (source: Source) : TaskSeq<'a> =
    readAsync options decoder source |> TaskSeq.map orFail

  /// The synchronous version of `readAsync`.
  let read (options: TableOptions) (decoder: RowDecoder<'a>) (source: Source) : Result<'a, DecodeError list> seq =
    seq {
      let struct (textReader, owner) = openReader source
      use _ = owner
      use csv = CsvDataReader.Create(textReader, TableOptions.toReaderOptions options)
      let headers = headersOf options csv
      let ordinals = Row.ordinalsOf headers

      yield!
        Seq.unfold (fun i -> Some(i, i + 1L)) 1L
        |> Seq.takeWhile (fun _ -> csv.Read())
        |> Seq.map (fun number -> decodeRow decoder (rowOf number headers ordinals csv))
    }

  /// Like `read`, but throws a DecodeException at the first row that fails.
  let readOrFail (options: TableOptions) (decoder: RowDecoder<'a>) (source: Source) : 'a seq =
    read options decoder source |> Seq.map orFail

  /// Reads every row eagerly, giving the values of the rows that decode and the errors from those that don't.
  let readAllAsync
    (options: TableOptions)
    (decoder: RowDecoder<'a>)
    (source: Source)
    : Task<'a list * DecodeError list> =
    task {
      let! state =
        readAsync options decoder source |> TaskSeq.fold Results.collect ([], [])

      return Results.finish state
    }

  /// The synchronous version of `readAllAsync`.
  let readAll (options: TableOptions) (decoder: RowDecoder<'a>) (source: Source) : 'a list * DecodeError list =
    read options decoder source |> Results.partition

  /// Full access to Sylvan's reader, for anything the codecs don't cover.
  let withReaderAsync (options: TableOptions) (source: Source) (f: CsvDataReader -> Task<'a>) : Task<'a> =
    task {
      let struct (textReader, owner) = openReader source
      use _ = owner

      use! csv =
        CsvDataReader.CreateAsync(textReader, TableOptions.toReaderOptions options)

      return! f csv
    }

  let private builder (encoder: RowEncoder<'a>) : ObjectDataReader.Builder<'a> =
    (ObjectDataReader.Builder<'a>(), encoder)
    ||> List.fold (fun b (header, get) -> b.AddColumn<string>(header, Func<'a, string> get))


  // SYLVAN CLOSES THE TEXTWRITER WHEN THE CSVDATAWRITER IS DISPOSED; WE OWN THAT LIFETIME INSTEAD.
  let private csvWriter options textWriter =
    CsvDataWriter.Create(new KeepOpenWriter(textWriter), TableOptions.toWriterOptions options)

  /// Writes any DbDataReader, e.g. query results, without an encoder.
  let writeDataReader (options: TableOptions) (sink: Sink) (reader: DbDataReader) =
    let struct (textWriter, owner) = openWriter sink
    use _ = owner
    use csv = csvWriter options textWriter
    csv.Write reader

  /// The asynchronous version of `writeDataReader`.
  let writeDataReaderAsync
    (options: TableOptions)
    (sink: Sink)
    (reader: DbDataReader)
    (cancellationToken: CancellationToken)
    =
    task {
      let struct (textWriter, owner) = openWriter sink
      use _ = owner
      use csv = csvWriter options textWriter
      return! csv.WriteAsync(reader, cancellationToken)
    }

  let write (options: TableOptions) (encoder: RowEncoder<'a>) (sink: Sink) (rows: 'a seq) =
    writeDataReader options sink ((builder encoder).Build rows)

  let writeAsync
    (options: TableOptions)
    (encoder: RowEncoder<'a>)
    (sink: Sink)
    (rows: TaskSeq<'a>)
    (cancellationToken: CancellationToken)
    =
    writeDataReaderAsync options sink ((builder encoder).Build(rows, cancellationToken)) cancellationToken

  let writeString (options: TableOptions) (encoder: RowEncoder<'a>) (rows: 'a seq) : string =
    use w = new StringWriter()
    write options encoder (Sink.Writer w) rows |> ignore
    string w

  let private remaining (e: IEnumerator<'a>) =
    seq {
      while e.MoveNext() do
        yield e.Current
    }

  /// Decodes rows that are already split into fields. With headers, the first row is the header row.
  let decodeRows
    (options: TableOptions)
    (decoder: RowDecoder<'a>)
    (rows: string seq seq)
    : Result<'a, DecodeError list> seq =
    // ONE ENUMERATOR FOR HEADER AND BODY, SO ONE-SHOT SOURCES ARE READ ONCE.
    seq {
      use e = rows.GetEnumerator()

      if not options.HasHeaders || e.MoveNext() then
        let headers =
          if options.HasHeaders then
            Some(Seq.toArray e.Current)
          else
            None

        let ordinals = Row.ordinalsOf headers

        yield!
          remaining e
          |> Seq.mapi (fun i values ->
            decodeRow decoder (Row.ofArray (int64 (i + 1)) headers ordinals (Seq.toArray values)))
    }

  /// Encodes to rows of fields, with a header row first when `options.HasHeaders`.
  let encodeRows (options: TableOptions) (encoder: RowEncoder<'a>) (rows: 'a seq) : string seq seq =
    let headers =
      if options.HasHeaders then
        Seq.singleton (encoder |> List.map fst :> string seq)
      else
        Seq.empty

    let encodeRow x =
      encoder |> List.map (fun (_, get) -> get x) :> string seq

    Seq.append headers (Seq.map encodeRow rows)
