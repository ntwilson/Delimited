module Delimited.PackageSpecs.PackageSpec

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Text
open System.Threading

open FSharp.Control
open NUnit.Framework
open Swensen.Unquote

open Delimited

type Reading = { Name: string; Temp: float }

let readingCodec: RowCodec<Reading> =
  codec {
    let! name = Codec.field "name" Codec.string _.Name
    and! temp = Codec.field "temp" Codec.float _.Temp
    return { Name = name; Temp = temp }
  }

let packageAssembly = typeof<TableOptions>.Assembly

let sourceLinkKind = Guid "CC110556-A091-4D38-9FEC-25AB9A351A6A"

[<Test>]
let ``referenced Delimited is the version just packed`` () =
  let expected =
    Assembly.GetExecutingAssembly().GetCustomAttributes<AssemblyMetadataAttribute>()
    |> Seq.find (fun a -> a.Key = "DelimitedVersion")
    |> _.Value

  let actual =
    packageAssembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>().InformationalVersion

  // INFORMATIONAL VERSION CARRIES A "+<commit>" SUFFIX.
  test <@ actual.Split('+').[0] = expected @>

[<Test>]
let ``round-trips a file through the packed library and its dependencies`` () =
  let path = Path.GetTempFileName()

  try
    let readings = [ { Name = "a, b"; Temp = 1.5 }; { Name = "c"; Temp = -2.0 } ]

    let written =
      Table.write TableOptions.csv readingCodec.Encode (Sink.File path) readings

    let successes, failures =
      (Table.readAllAsync TableOptions.csv readingCodec.Decode (Source.File path)).Result

    test <@ written = 2L && successes = readings && failures = [] @>
  finally
    File.Delete path


type Note = { Id: int; Text: string }

let noteCodec: RowCodec<Note> =
  codec {
    let! id = Codec.field "id" Codec.int _.Id
    and! text = Codec.field "text" Codec.string _.Text
    return { Id = id; Text = text }
  }

let tricky =
  [
    { Id = 1; Text = "plain" }
    { Id = 2; Text = "has, comma | pipe\ttab" }
    { Id = 3; Text = "has \"quotes\"" }
    { Id = 4; Text = "multi\nline" }
    { Id = 5; Text = "" }
  ]

let readOrFailList options source =
  (Table.readOrFailAsync options noteCodec.Decode source |> TaskSeq.toListAsync).Result

[<Test>]
let ``writeAsync streams a taskSeq to a file`` () =
  let path = Path.GetTempFileName()

  try
    let source = taskSeq { for n in tricky -> n }
    (Table.writeAsync TableOptions.csv noteCodec.Encode (Sink.File path) source CancellationToken.None).Wait()
    test <@ readOrFailList TableOptions.csv (Source.File path) = tricky @>
  finally
    File.Delete path

let readEmbeddedPdb () =
  use pe = new PEReader(File.OpenRead packageAssembly.Location)

  pe.ReadDebugDirectory()
  |> Seq.tryFind (fun e -> e.Type = DebugDirectoryEntryType.EmbeddedPortablePdb)
  |> Option.map pe.ReadEmbeddedPortablePdbDebugDirectoryData

[<Test>]
let ``DLL embeds a PDB with Source Link to GitHub`` () =
  let pdbProvider = readEmbeddedPdb ()
  test <@ pdbProvider.IsSome @>
  use pdbProvider = pdbProvider.Value
  let pdb = pdbProvider.GetMetadataReader()

  let sourceLink =
    pdb.CustomDebugInformation
    |> Seq.map pdb.GetCustomDebugInformation
    |> Seq.tryFind (fun cdi -> pdb.GetGuid cdi.Kind = sourceLinkKind)
    |> Option.map (fun cdi -> Encoding.UTF8.GetString(pdb.GetBlobBytes cdi.Value))

  test
    <@
      sourceLink
      |> Option.exists _.Contains("https://raw.githubusercontent.com/ntwilson/Delimited/")
    @>

// A REAL PATH WOULD LET EDITORS ON THE BUILD MACHINE OPEN LOCAL FILES, MASKING BROKEN SOURCE LINK.
[<Test>]
let ``PDB records only deterministic source paths`` () =
  let pdbProvider = readEmbeddedPdb ()
  test <@ pdbProvider.IsSome @>
  use pdbProvider = pdbProvider.Value
  let pdb = pdbProvider.GetMetadataReader()

  let localPaths =
    pdb.Documents
    |> Seq.map (fun h -> pdb.GetString (pdb.GetDocument h).Name)
    |> Seq.filter (fun path -> not (path.StartsWith "/_"))
    |> Seq.toList

  test <@ localPaths = [] @>
