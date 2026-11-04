namespace Delimited

open System
open System.Collections.Generic
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

module Csv =
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

  let private headersOf (options: CsvOptions) (csv: CsvDataReader) =
    if options.HasHeaders then
      Array.init csv.FieldCount csv.GetName
    else
      [||]

  let private rowOf (number: int64) headers ordinals (csv: CsvDataReader) : Row =
    {
      Number = number
      Headers = headers
      Ordinals = ordinals
      FieldCount = csv.RowFieldCount
      GetString = csv.GetString
    }

  let private orFail (result: Result<'T, DecodeError list>) : 'T =
    match result with
    | Ok v -> v
    | Error errors -> raise (DecodeException errors)

  /// Streams decoded rows. The source is opened when enumeration starts and closed when it ends.
  let read (options: CsvOptions) (decoder: Decoder<'T>) (source: Source) : TaskSeq<Result<'T, DecodeError list>> =
    taskSeq {
      let struct (textReader, owner) = openReader source
      use _ = owner
      let! csv = CsvDataReader.CreateAsync(textReader, CsvOptions.toReaderOptions options)
      use _ = csv
      let headers = headersOf options csv
      let ordinals = Row.ordinalsOf headers
      let mutable number = 0L
      let mutable more = true

      while more do
        let! hasRow = csv.ReadAsync()

        if hasRow then
          number <- number + 1L
          yield decoder (rowOf number headers ordinals csv)
        else
          more <- false
    }

  /// Like `read`, but throws a DecodeException at the first row that fails.
  let readOrFail (options: CsvOptions) (decoder: Decoder<'T>) (source: Source) : TaskSeq<'T> =
    read options decoder source |> TaskSeq.map orFail

  /// The synchronous version of `read`.
  let readSeq (options: CsvOptions) (decoder: Decoder<'T>) (source: Source) : Result<'T, DecodeError list> seq =
    seq {
      let struct (textReader, owner) = openReader source
      use _ = owner
      use csv = CsvDataReader.Create(textReader, CsvOptions.toReaderOptions options)
      let headers = headersOf options csv
      let ordinals = Row.ordinalsOf headers

      yield!
        Seq.initInfinite (fun i -> int64 (i + 1))
        |> Seq.takeWhile (fun _ -> csv.Read())
        |> Seq.map (fun number -> decoder (rowOf number headers ordinals csv))
    }

  /// Full access to Sylvan's reader, for anything the codecs don't cover.
  let withReader (options: CsvOptions) (source: Source) (f: CsvDataReader -> Task<'A>) : Task<'A> =
    task {
      let struct (textReader, owner) = openReader source
      use _ = owner
      use! csv = CsvDataReader.CreateAsync(textReader, CsvOptions.toReaderOptions options)
      return! f csv
    }

  let private builder (encoder: Encoder<'T>) : ObjectDataReader.Builder<'T> =
    encoder
    |> List.fold
      (fun (b: ObjectDataReader.Builder<'T>) (header, get) -> b.AddColumn<string>(header, Func<'T, string> get))
      (ObjectDataReader.Builder<'T>())

  // SYLVAN CLOSES THE TEXTWRITER WHEN THE CSVDATAWRITER IS DISPOSED; WE OWN THAT LIFETIME INSTEAD.
  let private csvWriter options textWriter =
    CsvDataWriter.Create(new KeepOpenWriter(textWriter), CsvOptions.toWriterOptions options)

  let write (options: CsvOptions) (encoder: Encoder<'T>) (sink: Sink) (rows: 'T seq) : unit =
    let struct (textWriter, owner) = openWriter sink
    use _ = owner
    use csv = csvWriter options textWriter
    csv.Write((builder encoder).Build rows) |> ignore

  let writeAsync
    (options: CsvOptions)
    (encoder: Encoder<'T>)
    (sink: Sink)
    (rows: TaskSeq<'T>)
    (cancellationToken: CancellationToken)
    : Task =
    task {
      let struct (textWriter, owner) = openWriter sink
      use _ = owner
      use csv = csvWriter options textWriter

      let! _ =
        csv.WriteAsync((builder encoder).Build(rows, cancellationToken), cancellationToken)

      ()
    }

  let writeString (options: CsvOptions) (encoder: Encoder<'T>) (rows: 'T seq) : string =
    use w = new StringWriter()
    write options encoder (Sink.Writer w) rows
    w.ToString()

  /// Decodes rows that are already split into fields. With headers, the first row is the header row.
  let decodeRows
    (options: CsvOptions)
    (decoder: Decoder<'T>)
    (rows: string seq seq)
    : Result<'T, DecodeError list> seq =
    let arrays = rows |> Seq.map Seq.toArray

    match options.HasHeaders, Seq.tryHead arrays with
    | true, None -> Seq.empty
    | true, Some headers ->
      let ordinals = Row.ordinalsOf headers

      arrays
      |> Seq.indexed
      |> Seq.filter (fun (i, _) -> i > 0)
      |> Seq.map (fun (i, values) -> decoder (Row.ofArray (int64 i) headers ordinals values))
    | false, _ ->
      let ordinals = Row.ordinalsOf [||]

      arrays
      |> Seq.mapi (fun i values -> decoder (Row.ofArray (int64 (i + 1)) [||] ordinals values))

  /// Encodes to rows of fields, with a header row first when `options.HasHeaders`.
  let encodeRows (options: CsvOptions) (encoder: Encoder<'T>) (rows: 'T seq) : string seq seq =
    let headers =
      if options.HasHeaders then
        Seq.singleton (encoder |> List.map fst :> string seq)
      else
        Seq.empty

    let encodeRow x =
      encoder |> List.map (fun (_, get) -> get x) :> string seq

    Seq.append headers (Seq.map encodeRow rows)
