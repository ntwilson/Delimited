namespace Delimited

open System
open System.Collections.Generic

/// One row being decoded. Only needed for decoding.
type Row =
  {
    /// 1-based data row number, not counting the header.
    Number: int64
    Headers: string[]
    Ordinals: IReadOnlyDictionary<string, int>
    FieldCount: int
    GetString: int -> string
  }

module internal Row =
  /// When headers repeat, the first one wins.
  let ordinalsOf (headers: string[]) : IReadOnlyDictionary<string, int> =
    headers
    |> Seq.indexed
    |> Seq.distinctBy snd
    |> Seq.map (fun (i, h) -> KeyValuePair(h, i))
    |> fun kvs -> Dictionary(kvs, StringComparer.Ordinal) :> IReadOnlyDictionary<_, _>

  let tryOrdinal (header: string) (row: Row) : int voption =
    match row.Ordinals.TryGetValue header with
    | true, i -> ValueSome i
    | false, _ -> ValueNone

  let tryGet (index: int) (row: Row) : string voption =
    if index >= 0 && index < row.FieldCount then
      ValueSome(row.GetString index)
    else
      ValueNone

  let ofArray
    (number: int64)
    (headers: string[])
    (ordinals: IReadOnlyDictionary<string, int>)
    (values: string[])
    : Row =
    {
      Number = number
      Headers = headers
      Ordinals = ordinals
      FieldCount = values.Length
      GetString = fun i -> values.[i]
    }
