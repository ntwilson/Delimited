namespace Delimited

/// `Table` with `TableOptions.csv` applied.
[<RequireQualifiedAccess>]
module Csv =
  let readAsync decoder source =
    Table.readAsync TableOptions.csv decoder source

  let readOrFailAsync decoder source =
    Table.readOrFailAsync TableOptions.csv decoder source

  let read decoder source =
    Table.read TableOptions.csv decoder source

  let readOrFail decoder source =
    Table.readOrFail TableOptions.csv decoder source

  let readAllAsync decoder source =
    Table.readAllAsync TableOptions.csv decoder source

  let readAll decoder source =
    Table.readAll TableOptions.csv decoder source

  let withReaderAsync source f =
    Table.withReaderAsync TableOptions.csv source f

  let writeDataReader sink reader =
    Table.writeDataReader TableOptions.csv sink reader

  let writeDataReaderAsync sink reader cancellationToken =
    Table.writeDataReaderAsync TableOptions.csv sink reader cancellationToken

  let write encoder sink rows =
    Table.write TableOptions.csv encoder sink rows

  let writeAsync encoder sink rows cancellationToken =
    Table.writeAsync TableOptions.csv encoder sink rows cancellationToken

  let writeString encoder rows =
    Table.writeString TableOptions.csv encoder rows

  let decodeRows decoder rows =
    Table.decodeRows TableOptions.csv decoder rows

  let encodeRows encoder rows =
    Table.encodeRows TableOptions.csv encoder rows
