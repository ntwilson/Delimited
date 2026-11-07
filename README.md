# Delimited

F# helper to read/write delimited text streams, mostly CSV files. Utilizes [Sylvan.Data.Csv](https://www.nuget.org/packages/Sylvan.Data.Csv) to quickly parse CSV data into strings, but uses codecs to encode and decode similar to [Thoth.Json](https://thoth-org.github.io/Thoth.Json/documentation/introduction/).

Targets `netstandard2.1` and `net6.0` (`dateOnly`/`timeOnly` need `net6.0`).

## Reading

```fsharp
open Delimited
open FSharp.Control

type Reading = { Name: string; Temp: float; Date: System.DateOnly }

let reading =
  decoder {
    let! name = Decode.field "name" Decode.string
    and! temp = Decode.field "temp" Decode.float
    and! date = Decode.field "date" (Decode.dateOnly "yyyy-MM-dd")
    return { Name = name; Temp = temp; Date = date }
  }

// TaskSeq<Result<Reading, DecodeError list>>, which is an IAsyncEnumerable.
// `and!` reports every bad field in a row, not just the first.
let rows = Table.readAsync TableOptions.csv reading (Source.File "readings.csv")

// Or throw a DecodeException on the first bad row:
let! all = Table.readOrFailAsync TableOptions.tsv reading (Source.Stream stream) |> TaskSeq.toListAsync

// Or eagerly read and split on successfully parsed rows and failures
let! (successes, failures) = Table.readAllAsync TableOptions.csv reading (Source.File "readings.csv")
```

Sources: `Source.File`, `Source.Stream`, `Source.Reader`, `Source.Text`. A stream or reader you pass in is left open.
`Table.read` is the synchronous version, and `Table.decodeRows` decodes an in-memory `string seq seq`.

Date and time decoders take an exact format. For ISO 8601 (`2026-03-14T10:00:00Z`), use `dateTimeIso`, `dateTimeOffsetIso`, `dateOnlyIso` or `timeOnlyIso`, which also exist in `Encode` and `Codec`. When decoding, these accept basic (`20260314T1000`) or extended format, a space instead of the `T`, minutes and seconds optional, and a fraction after `.` or `,`. They don't accept week or ordinal dates, fractional hours or minutes, or `24:00`, because .NET's exact-format parsing can't express them. `dateTimeIso` converts any offset to UTC, and `dateTimeOffsetIso` requires an offset or `Z`.

When the format isn't known in advance, use `Decode.dateTimeAuto`, `dateTimeOffsetAuto`, `dateOnlyAuto` or `timeOnlyAuto`. These accept anything the invariant culture recognizes, so an ambiguous date like `03/04/2026` is read month first.

## Writing

```fsharp
let encoder : RowEncoder<Reading> =
  Encode.columns
    [ Encode.field "name" Encode.string _.Name
      Encode.field "temp" Encode.float _.Temp
      Encode.field "date" (Encode.dateOnly "yyyy-MM-dd") _.Date ]

// Each write returns the number of rows written.
let count = Table.write TableOptions.csv encoder (Sink.File "out.csv") readings     // seq
let! count = Table.writeAsync TableOptions.csv encoder (Sink.Stream s) asyncReadings ct   // TaskSeq / IAsyncEnumerable
let! count = Table.writeAsync TableOptions.tsv encoder (Sink.Stream s) (readings |> TaskSeq.ofSeq) ct
let text = Table.writeString (TableOptions.withDelimiter '|' TableOptions.csv) encoder readings
```

`Table.encodeRows` encodes to an in-memory `string seq seq`, with the header row first when the options have headers.

## Custom fields

A `FieldDecoder<'a>` is just `string -> Result<'a, string>` and a `FieldEncoder<'a>` is `'a -> string`, so you can define your own decoders and encoders to be able to work with any custom field:

```fsharp
let yesNo : FieldDecoder<bool> = function "Y" -> Ok true | "N" -> Ok false | s -> Error $"bad flag %s{s}"
let positive = Decode.int |> Decode.Field.andThen (fun i -> if i > 0 then Ok i else Error "must be positive")
let celsius = Encode.float |> Encode.Field.contramap (fun (Celsius c) -> c)
```

Combinators that exist for both fields and rows live in `Decode.Field`/`Decode.Row` and `Encode.Field`/`Encode.Row`.

`Decode.Field.either` and `Decode.Row.either` try one decoder and fall back to a second. To try more than two, reduce a list with them:

```fsharp
let when' =
  [ "yyyy-MM-dd"; "MM/dd/yyyy"; "dd.MM.yyyy" ]
  |> List.map Decode.dateTime
  |> List.reduce Decode.Field.either
  |> Decode.field "when"
```

`FieldCodec<'a>` pairs a decoder with an encoder when you need both; pass `codec.Decode` to `Decode.field` and `codec.Encode` to `Encode.field`:

```fsharp
let celsius = Codec.float |> Codec.Field.map Celsius (fun (Celsius c) -> c)
let yesNoCodec = Codec.Field.create yesNo (fun b -> if b then "Y" else "N")
```

`Codec.result` and `Codec.choiceOf2` round-trip only when no text is accepted by both codecs. With `Codec.choiceOf2 Codec.float Codec.int`, `Choice2Of2 3` is written as `3` and read back as `Choice1Of2 3.0`.

## Reading and writing the same file

The `codec` builder declares each column once and gives you both a decoder and an encoder:

```fsharp
let reading : RowCodec<Reading> =
  codec {
    let! name = Codec.field "name" Codec.string _.Name
    and! temp = Codec.field "temp" Codec.float _.Temp
    and! date = Codec.field "date" (Codec.dateOnly "yyyy-MM-dd") _.Date
    return { Name = name; Temp = temp; Date = date }
  }

let rows = Table.readAsync TableOptions.csv reading.Decode (Source.File "readings.csv")
Table.write TableOptions.tsv reading.Encode (Sink.File "out.tsv") readings |> ignore
```

`Codec.Row.map` converts a finished codec in both directions, like `Codec.Field.map`.
`codec { }` supports `and!` but not dependent `let!`. When later columns depend on earlier values, write a separate `decoder { }` and `RowEncoder`.

## Dynamic data

Dynamic shapes are ordinary decoders and encoders, so they work with every source and sink:

- `Decode.fields` gives `string[]`; `Decode.dictionary Decode.string` gives `Dictionary<string, string>` of every column
- `Decode.dictionaryOf headers Decode.string` gives only the listed columns, each of which must exist
- `Encode.fields headers` writes `string[]`; `Encode.dictionary headers Encode.string` writes `IDictionary<string, string>`
- `Codec.dictionary headers Codec.float _.Readings` combines the two inside `codec { }`; use `Codec.option` when cells can be blank
- `Table.withReaderAsync options source (fun csvDataReader -> task { ... })` gives you Sylvan's raw `CsvDataReader`
- `Table.writeDataReader options sink dbDataReader` (and `writeDataReaderAsync`) writes any `DbDataReader`, such as query results, without an encoder

## Options

`TableOptions.csv`, `TableOptions.tsv`, `TableOptions.psv`, and `TableOptions.autoDetect` (detects the delimiter when reading) are the presets. Use `withDelimiter` and `withoutHeaders` to adjust them, or manually adjust the options such as

```fsharp
{ TableOptions.csv with HasHeaders = false; Delimiter = ValueSome ';' }
```

You can use the `ConfigureReader`/`ConfigureWriter` fields in `TableOptions` to reach any other Sylvan setting.

## Development

Run the tests in Release (`dotnet test -c Release`). In Debug builds, `taskSeq` code that awaits a task that really suspends (such as a file read) can silently yield nothing. This is a known compiler issue that is fixed in a newer SDK.

To test the NuGet package itself rather than the project, run `./test-package.ps1`. It packs into `artifacts/` and runs `PackageTests` against that package.
