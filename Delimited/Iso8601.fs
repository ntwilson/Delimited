namespace Delimited

/// The ISO 8601 forms .NET's exact-format parsing can express, plus RFC 3339's space or "t" before the time.
module internal Iso8601 =
  let private extended =
    "yyyy-MM-dd", [ "HH"; "HH:mm"; "HH:mm:ss.FFFFFFF"; "HH:mm:ss,FFFFFFF" ]

  let private basic = "yyyyMMdd", [ "HH"; "HHmm"; "HHmmss.FFFFFFF"; "HHmmss,FFFFFFF" ]

  // ISO 8601 FORBIDS MIXING BASIC AND EXTENDED, SO EACH DATE ONLY PAIRS WITH ITS OWN TIMES.
  let private dateTimes (zones: string list) =
    [|
      for date, times in [ extended; basic ] do
        for separator in [ "'T'"; "'t'"; "' '" ] do
          for time in times do
            for zone in zones do
              date + separator + time + zone
    |]

  // K MATCHES "Z", ±hh:mm, ±hhmm, OR NOTHING, BUT NOT ±hh.
  let dateTimeFormats = dateTimes [ "K"; "zz" ]

  // zzz REJECTS "Z", SO UTC NEEDS A LITERAL.
  let dateTimeOffsetFormats = dateTimes [ "zzz"; "zz"; "'Z'" ]

  let dateOnlyFormats = [| fst extended; fst basic |]

  let timeOnlyFormats =
    [|
      for prefix in [ ""; "'T'" ] do
        for time in List.distinct (snd extended @ snd basic) do
          prefix + time
    |]

  /// What the encoders write.
  let dateTimeFormat = "yyyy-MM-dd'T'HH:mm:ss.FFFFFFFK"
  let dateTimeOffsetFormat = "yyyy-MM-dd'T'HH:mm:ss.FFFFFFFzzz"
  let dateOnlyFormat = fst extended
  let timeOnlyFormat = "HH:mm:ss.FFFFFFF"
