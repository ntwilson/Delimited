namespace Delimited

open System

open Sylvan.Data.Csv

type Delimiter =
  | AutoDetect
  | Specific of char

/// 'shape is Headed or Positional; only Headed options accept decoders that find columns by name.
type TableOptions<'shape> =
  internal
    {
      /// AutoDetect writes commas
      Delimiter: Delimiter
      HasHeaders: bool
      Quote: char
      /// How decoders match column names to headers.
      HeaderComparer: StringComparer
      /// Runs last, so it can override anything above.
      ConfigureReader: CsvDataReaderOptions -> unit
      ConfigureWriter: CsvDataWriterOptions -> unit
    }

module TableOptions =
  /// Comma-delimited, with a header row and `"` quotes.
  let defaults: TableOptions<Headed> =
    {
      Delimiter = Delimiter.Specific ','
      HasHeaders = true
      Quote = '"'
      HeaderComparer = StringComparer.Ordinal
      ConfigureReader = ignore
      ConfigureWriter = ignore
    }

  let withDelimiter (delimiter: char) (options: TableOptions<'shape>) : TableOptions<'shape> =
    { options with Delimiter = Delimiter.Specific delimiter }

  /// Writes commas.
  let withAutoDetectDelimiter (options: TableOptions<'shape>) : TableOptions<'shape> =
    { options with Delimiter = AutoDetect }

  let withQuote (quote: char) (options: TableOptions<'shape>) : TableOptions<'shape> = { options with Quote = quote }

  /// Only `Decode.index`, `Decode.fields` and other decoders that don't need headers can read with these options.
  let withoutHeaders (options: TableOptions<'shape>) : TableOptions<Positional> =
    {
      Delimiter = options.Delimiter
      HasHeaders = false
      Quote = options.Quote
      HeaderComparer = options.HeaderComparer
      ConfigureReader = options.ConfigureReader
      ConfigureWriter = options.ConfigureWriter
    }

  /// How decoders match column names to headers. Also keys the dictionaries from `Decode.dictionary`/`dictionaryOf`.
  let withHeaderComparer (comparer: StringComparer) (options: TableOptions<Headed>) : TableOptions<Headed> =
    { options with HeaderComparer = comparer }

  let withCaseInsensitiveHeaders (options: TableOptions<Headed>) : TableOptions<Headed> =
    withHeaderComparer StringComparer.OrdinalIgnoreCase options

  /// Runs last, so it can override any other option.
  /// TURNING OFF HasHeaders HERE BYPASSES THE Headed/Positional CHECK; USE withoutHeaders INSTEAD.
  let withReaderConfig
    (configure: CsvDataReaderOptions -> unit)
    (options: TableOptions<'shape>)
    : TableOptions<'shape> =
    { options with ConfigureReader = configure }

  /// Runs last, so it can override any other option.
  let withWriterConfig
    (configure: CsvDataWriterOptions -> unit)
    (options: TableOptions<'shape>)
    : TableOptions<'shape> =
    { options with ConfigureWriter = configure }

  let internal toReaderOptions (options: TableOptions<'shape>) : CsvDataReaderOptions =
    // WE MANAGE THE TEXTREADER'S LIFETIME OURSELVES, SO A CALLER'S READER ISN'T CLOSED UNDER THEM.
    let o =
      CsvDataReaderOptions(
        HasHeaders = options.HasHeaders,
        Quote = options.Quote,
        HeaderComparer = options.HeaderComparer,
        OwnsReader = false
      )

    match options.Delimiter with
    | Delimiter.Specific d -> o.Delimiter <- d
    | AutoDetect -> ()

    options.ConfigureReader o
    o

  let internal toWriterOptions (options: TableOptions<'shape>) : CsvDataWriterOptions =
    let o =
      CsvDataWriterOptions(
        WriteHeaders = options.HasHeaders,
        Quote = options.Quote,
        Delimiter =
          match options.Delimiter with
          | Specific c -> c
          | AutoDetect -> ','
      )

    options.ConfigureWriter o
    o
