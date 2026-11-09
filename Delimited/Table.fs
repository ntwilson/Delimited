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
  let readAsyncWithOptions
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

  /// `readAsyncWithOptions` with `TableOptions.defaults`.
  let readAsync (decoder: RowDecoder<'a>) (source: Source) =
    readAsyncWithOptions TableOptions.defaults decoder source

  /// Like `readAsync`, but throws a DecodeException at the first row that fails.
  let readOrFailAsyncWithOptions (options: TableOptions) (decoder: RowDecoder<'a>) (source: Source) : TaskSeq<'a> =
    readAsyncWithOptions options decoder source |> TaskSeq.map orFail

  /// `readOrFailAsyncWithOptions` with `TableOptions.defaults`.
  let readOrFailAsync (decoder: RowDecoder<'a>) (source: Source) =
    readOrFailAsyncWithOptions TableOptions.defaults decoder source

  /// The synchronous version of `readAsync`.
  let readWithOptions
    (options: TableOptions)
    (decoder: RowDecoder<'a>)
    (source: Source)
    : Result<'a, DecodeError list> seq =
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

  /// `readWithOptions` with `TableOptions.defaults`.
  let read (decoder: RowDecoder<'a>) (source: Source) =
    readWithOptions TableOptions.defaults decoder source

  /// Like `read`, but throws a DecodeException at the first row that fails.
  let readOrFailWithOptions (options: TableOptions) (decoder: RowDecoder<'a>) (source: Source) : 'a seq =
    readWithOptions options decoder source |> Seq.map orFail

  /// `readOrFailWithOptions` with `TableOptions.defaults`.
  let readOrFail (decoder: RowDecoder<'a>) (source: Source) =
    readOrFailWithOptions TableOptions.defaults decoder source

  /// Reads every row eagerly, giving the values of the rows that decode and the errors from those that don't.
  let readAllAsyncWithOptions
    (options: TableOptions)
    (decoder: RowDecoder<'a>)
    (source: Source)
    : Task<'a list * DecodeError list> =
    task {
      let! state =
        readAsyncWithOptions options decoder source
        |> TaskSeq.fold Results.collect ([], [])

      return Results.finish state
    }

  /// `readAllAsyncWithOptions` with `TableOptions.defaults`.
  let readAllAsync (decoder: RowDecoder<'a>) (source: Source) =
    readAllAsyncWithOptions TableOptions.defaults decoder source

  /// The synchronous version of `readAllAsync`.
  let readAllWithOptions
    (options: TableOptions)
    (decoder: RowDecoder<'a>)
    (source: Source)
    : 'a list * DecodeError list =
    readWithOptions options decoder source |> Results.partition

  /// `readAllWithOptions` with `TableOptions.defaults`.
  let readAll (decoder: RowDecoder<'a>) (source: Source) =
    readAllWithOptions TableOptions.defaults decoder source

  /// Full access to Sylvan's reader, for anything the codecs don't cover.
  let withReaderAsyncWithOptions (options: TableOptions) (source: Source) (f: CsvDataReader -> Task<'a>) : Task<'a> =
    task {
      let struct (textReader, owner) = openReader source
      use _ = owner

      use! csv =
        CsvDataReader.CreateAsync(textReader, TableOptions.toReaderOptions options)

      return! f csv
    }

  /// `withReaderAsyncWithOptions` with `TableOptions.defaults`.
  let withReaderAsync (source: Source) (f: CsvDataReader -> Task<'a>) =
    withReaderAsyncWithOptions TableOptions.defaults source f

  let private builder (encoder: RowEncoder<'a>) : ObjectDataReader.Builder<'a> =
    (ObjectDataReader.Builder<'a>(), encoder)
    ||> List.fold (fun b (header, get) -> b.AddColumn<string>(header, Func<'a, string> get))


  // SYLVAN CLOSES THE TEXTWRITER WHEN THE CSVDATAWRITER IS DISPOSED; WE OWN THAT LIFETIME INSTEAD.
  let private csvWriter options textWriter =
    CsvDataWriter.Create(new KeepOpenWriter(textWriter), TableOptions.toWriterOptions options)

  /// Writes any DbDataReader, e.g. query results, without an encoder.
  let writeDataReaderWithOptions (options: TableOptions) (sink: Sink) (reader: DbDataReader) =
    let struct (textWriter, owner) = openWriter sink
    use _ = owner
    use csv = csvWriter options textWriter
    csv.Write reader

  /// `writeDataReaderWithOptions` with `TableOptions.defaults`.
  let writeDataReader (sink: Sink) (reader: DbDataReader) =
    writeDataReaderWithOptions TableOptions.defaults sink reader

  /// The asynchronous version of `writeDataReader`.
  let writeDataReaderAsyncWithOptions
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

  /// `writeDataReaderAsyncWithOptions` with `TableOptions.defaults`.
  let writeDataReaderAsync (sink: Sink) (reader: DbDataReader) (cancellationToken: CancellationToken) =
    writeDataReaderAsyncWithOptions TableOptions.defaults sink reader cancellationToken

  let writeWithOptions (options: TableOptions) (encoder: RowEncoder<'a>) (sink: Sink) (rows: 'a seq) =
    writeDataReaderWithOptions options sink ((builder encoder).Build rows)

  /// `writeWithOptions` with `TableOptions.defaults`.
  let write (encoder: RowEncoder<'a>) (sink: Sink) (rows: 'a seq) =
    writeWithOptions TableOptions.defaults encoder sink rows

  let writeAsyncWithOptions
    (options: TableOptions)
    (encoder: RowEncoder<'a>)
    (sink: Sink)
    (rows: TaskSeq<'a>)
    (cancellationToken: CancellationToken)
    =
    writeDataReaderAsyncWithOptions options sink ((builder encoder).Build(rows, cancellationToken)) cancellationToken

  /// `writeAsyncWithOptions` with `TableOptions.defaults`.
  let writeAsync (encoder: RowEncoder<'a>) (sink: Sink) (rows: TaskSeq<'a>) (cancellationToken: CancellationToken) =
    writeAsyncWithOptions TableOptions.defaults encoder sink rows cancellationToken

  let writeStringWithOptions (options: TableOptions) (encoder: RowEncoder<'a>) (rows: 'a seq) : string =
    use w = new StringWriter()
    writeWithOptions options encoder (Sink.Writer w) rows |> ignore
    string w

  /// `writeStringWithOptions` with `TableOptions.defaults`.
  let writeString (encoder: RowEncoder<'a>) (rows: 'a seq) =
    writeStringWithOptions TableOptions.defaults encoder rows

  let private remaining (e: IEnumerator<'a>) =
    seq {
      while e.MoveNext() do
        yield e.Current
    }

  /// Decodes rows that are already split into fields. With headers, the first row is the header row.
  let decodeRowsWithOptions
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

  /// `decodeRowsWithOptions` with `TableOptions.defaults`.
  let decodeRows (decoder: RowDecoder<'a>) (rows: string seq seq) =
    decodeRowsWithOptions TableOptions.defaults decoder rows

  /// Encodes to rows of fields, with a header row first when `options.HasHeaders`.
  let encodeRowsWithOptions (options: TableOptions) (encoder: RowEncoder<'a>) (rows: 'a seq) : string seq seq =
    let headers =
      if options.HasHeaders then
        Seq.singleton (encoder |> List.map fst :> string seq)
      else
        Seq.empty

    let encodeRow x =
      encoder |> List.map (fun (_, get) -> get x) :> string seq

    Seq.append headers (Seq.map encodeRow rows)

  /// `encodeRowsWithOptions` with `TableOptions.defaults`.
  let encodeRows (encoder: RowEncoder<'a>) (rows: 'a seq) =
    encodeRowsWithOptions TableOptions.defaults encoder rows
