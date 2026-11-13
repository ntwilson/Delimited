namespace Delimited

open System
open System.Collections.Generic
open System.Linq

/// One row being decoded. Only needed for decoding.
type Row =
  internal
    {
      /// 1-based data row number, not counting the header.
      Number: int64
      /// None when the table has no header row.
      Headers: string[] option
      HeaderComparer: StringComparer
      Ordinals: IReadOnlyDictionary<string, int[]>
      FieldCount: int
      GetString: int -> string
    }

module internal Row =
  /// `Seq.groupBy` with a key comparer. Groups, and the elements within each, keep source order.
  let private groupBy
    (comparer: IEqualityComparer<'key>)
    (projection: 'a -> 'key)
    (source: 'a seq)
    : ('key * 'a seq) seq =
    Enumerable.GroupBy(source, Func<'a, 'key> projection, comparer)
    |> Seq.map (fun g -> g.Key, g :> 'a seq)

  /// Every index of each header, in column order. Headers that `comparer` deems equal share one entry.
  let ordinalsOf (comparer: StringComparer) (headers: string[] option) : IReadOnlyDictionary<string, int[]> =
    // GROUP WITH THE COMPARER, NOT Seq.groupBy: "Id" AND "ID" WOULD BE SEPARATE GROUPS AND THE DICTIONARY WOULD THROW.
    headers
    |> Option.defaultValue [||]
    |> Seq.indexed
    |> groupBy comparer snd
    |> Seq.map (fun (header, group) -> KeyValuePair(header, group |> Seq.map fst |> Seq.toArray))
    |> fun kvs -> Dictionary(kvs, comparer) :> IReadOnlyDictionary<_, _>

  /// Empty when the header isn't in the table.
  let ordinals (header: string) (row: Row) : int[] =
    match row.Ordinals.TryGetValue header with
    | true, indexes -> indexes
    | false, _ -> [||]

  let tryGet (index: int) (row: Row) : string voption =
    if index >= 0 && index < row.FieldCount then
      ValueSome(row.GetString index)
    else
      ValueNone

  let ofArray
    (number: int64)
    (headers: string[] option)
    (comparer: StringComparer)
    (ordinals: IReadOnlyDictionary<string, int[]>)
    (values: string[])
    : Row =
    {
      Number = number
      Headers = headers
      HeaderComparer = comparer
      Ordinals = ordinals
      FieldCount = values.Length
      GetString = fun i -> values.[i]
    }
