# Delimited

F# helper to read/write delimited text streams, mostly CSV files. Utilizes [Sylvan.Data.Csv](https://www.nuget.org/packages/Sylvan.Data.Csv) to quickly parse CSV data into strings, but uses codecs to encode and decode similar to [Thoth.Json](https://thoth-org.github.io/Thoth.Json/documentation/introduction/).

Targets `netstandard2.1` and `net6.0` (`Field.dateOnly`/`Field.timeOnly` need `net6.0`).

## Reading

```fsharp
open Delimited
open FSharp.Control

type Reading = { Name: string; Temp: float; Date: System.DateOnly }

let reading =
  decoder {
    let! name = Decode.field "name" Field.string
    and! temp = Decode.field "temp" Field.float
    and! date = Decode.field "date" (Field.dateOnly "yyyy-MM-dd")
    return { Name = name; Temp = temp; Date = date }
  }

// TaskSeq<Result<Reading, DecodeError list>>, which is an IAsyncEnumerable.
// `and!` reports every bad field in a row, not just the first.
let rows = Csv.read CsvOptions.csv reading (Source.File "readings.csv")

// Or throw a DecodeException on the first bad row:
let! all = Csv.readOrFail CsvOptions.tsv reading (Source.Stream stream) |> TaskSeq.toListAsync
```

Sources: `Source.File`, `Source.Stream`, `Source.Reader`, `Source.Text`. A stream or reader you pass in is left open.
`Csv.readSeq` is the synchronous version, and `Csv.decodeRows` decodes an in-memory `string seq seq`.

## Writing

```fsharp
let encoder : Encoder<Reading> =
  Encode.columns
    [ Encode.field "name" Field.string _.Name
      Encode.field "temp" Field.float _.Temp
      Encode.field "date" (Field.dateOnly "yyyy-MM-dd") _.Date ]

Csv.write CsvOptions.csv encoder (Sink.File "out.csv") readings              // seq
do! Csv.writeAsync CsvOptions.csv encoder (Sink.Stream s) asyncReadings ct   // TaskSeq / IAsyncEnumerable
let text = Csv.writeString (CsvOptions.withDelimiter '|' CsvOptions.csv) encoder readings
```

## Custom codecs

`FieldCodec<'T>` works in both directions, so one codec covers decoding and encoding:

```fsharp
let celsius = Field.float |> Field.map Celsius (fun (Celsius c) -> c)
let positive = Field.int |> Field.andThen (fun i -> if i > 0 then Ok i else Error "must be positive") id
let yesNo = Field.custom (function "Y" -> Ok true | "N" -> Ok false | s -> Error $"bad flag %s{s}") (fun b -> if b then "Y" else "N")
```

## Dynamic data

Dynamic shapes are ordinary codecs, so they work with every source and sink:

- `Decode.fields` gives `string[]`; `Decode.dictionary` gives `IReadOnlyDictionary<string, string>`
- `Encode.fields headers` writes `string[]`; `Encode.dictionary headers` writes `IDictionary<string, string>`; `Encode.objDictionary headers` writes `IDictionary<string, obj>`, which includes `ExpandoObject`
- `Csv.withReader options source (fun csvDataReader -> task { ... })` gives you Sylvan's raw `CsvDataReader`

## Options

`CsvOptions.csv`, `CsvOptions.tsv` and `CsvOptions.autoDetect` (detects the delimiter when reading) are the presets. Use `withDelimiter` and `withoutHeaders` to adjust them, and `ConfigureReader`/`ConfigureWriter` to reach any other Sylvan setting.

## Development

Run the tests in Release (`dotnet test -c Release`). In Debug builds, `taskSeq` code that awaits a task that really suspends (such as a file read) can silently yield nothing. This is a known compiler issue that is fixed in a newer SDK.
