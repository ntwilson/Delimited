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
      Ordinals: IReadOnlyDictionary<string, int>
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

  /// When headers repeat (as judged by `comparer`), the first one wins.
  let ordinalsOf (comparer: StringComparer) (headers: string[] option) : IReadOnlyDictionary<string, int> =
    // DEDUPE WITH THE COMPARER, NOT distinctBy: "Id" AND "ID" WOULD BOTH SURVIVE AND THE DICTIONARY WOULD THROW.
    // GROUPS ARE NEVER EMPTY, SO Seq.head IS THE FIRST OCCURRENCE.
    headers
    |> Option.defaultValue [||]
    |> Seq.indexed
    |> groupBy comparer snd
    |> Seq.map (fun (header, group) -> KeyValuePair(header, fst (Seq.head group)))
    |> fun kvs -> Dictionary(kvs, comparer) :> IReadOnlyDictionary<_, _>

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
    (headers: string[] option)
    (comparer: StringComparer)
    (ordinals: IReadOnlyDictionary<string, int>)
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
