namespace Delimited

open System

open Sylvan.Data.Csv

type Delimiter =
  | AutoDetect
  | Specific of char

type TableOptions =
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
  let defaults =
    {
      Delimiter = Delimiter.Specific ','
      HasHeaders = true
      Quote = '"'
      HeaderComparer = StringComparer.Ordinal
      ConfigureReader = ignore
      ConfigureWriter = ignore
    }

  let withDelimiter (delimiter: char) (options: TableOptions) =
    { options with Delimiter = Delimiter.Specific delimiter }

  let withAutoDetectDelimiter (options: TableOptions) = { options with Delimiter = AutoDetect }

  let withoutHeaders (options: TableOptions) = { options with HasHeaders = false }

  let withCaseInsensitiveHeaders (options: TableOptions) =
    { options with HeaderComparer = StringComparer.OrdinalIgnoreCase }

  let internal toReaderOptions (options: TableOptions) : CsvDataReaderOptions =
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

  let internal toWriterOptions (options: TableOptions) : CsvDataWriterOptions =
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
