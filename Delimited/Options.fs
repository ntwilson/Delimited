namespace Delimited

open Sylvan.Data.Csv

type TableOptions =
  {
    /// ValueNone auto-detects when reading and writes commas.
    Delimiter: char voption
    HasHeaders: bool
    Quote: char
    /// Runs last, so it can override anything above.
    ConfigureReader: CsvDataReaderOptions -> unit
    ConfigureWriter: CsvDataWriterOptions -> unit
  }

module TableOptions =
  let csv =
    {
      Delimiter = ValueSome ','
      HasHeaders = true
      Quote = '"'
      ConfigureReader = ignore
      ConfigureWriter = ignore
    }

  let tsv = { csv with Delimiter = ValueSome '\t' }

  let psv = { csv with Delimiter = ValueSome '|' }

  let autoDetect = { csv with Delimiter = ValueNone }

  let withDelimiter (delimiter: char) (options: TableOptions) =
    { options with Delimiter = ValueSome delimiter }

  let withoutHeaders (options: TableOptions) = { options with HasHeaders = false }

  let internal toReaderOptions (options: TableOptions) : CsvDataReaderOptions =
    // WE MANAGE THE TEXTREADER'S LIFETIME OURSELVES, SO A CALLER'S READER ISN'T CLOSED UNDER THEM.
    let o =
      CsvDataReaderOptions(HasHeaders = options.HasHeaders, Quote = options.Quote, OwnsReader = false)

    match options.Delimiter with
    | ValueSome d -> o.Delimiter <- d
    | ValueNone -> ()

    options.ConfigureReader o
    o

  let internal toWriterOptions (options: TableOptions) : CsvDataWriterOptions =
    let o =
      CsvDataWriterOptions(
        WriteHeaders = options.HasHeaders,
        Quote = options.Quote,
        Delimiter = ValueOption.defaultValue ',' options.Delimiter
      )

    options.ConfigureWriter o
    o
