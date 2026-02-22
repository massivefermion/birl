import birl/duration
import birl/zones
import gleam/bool
import gleam/function
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/duration as time_duration
import gleam/time/timestamp

import ranger

pub opaque type Time {
  Time(
    timestamp: timestamp.Timestamp,
    offset: time_duration.Duration,
    timezone: option.Option(String),
    monotonic_time: option.Option(Int),
  )
}

pub type Day {
  Day(year: Int, month: Int, date: Int)
}

pub type TimeOfDay {
  TimeOfDay(hour: Int, minute: Int, second: Int, nanosecond: Int)
}

/// Deprecated: Use the nanosecond field directly, multiply by 1_000_000 for milliseconds
@deprecated("Use nanosecond field directly, divide by 1_000_000 for milliseconds")
pub fn get_milli_second(tod: TimeOfDay) -> Int {
  tod.nanosecond / 1_000_000
}

/// starting point of unix timestamps
pub fn unix_epoch() -> Time {
  Time(
    timestamp.from_unix_seconds(0),
    time_duration.seconds(0),
    option.None,
    option.None,
  )
}

pub type Weekday {
  Mon
  Tue
  Wed
  Thu
  Fri
  Sat
  Sun
}

pub type Month {
  Jan
  Feb
  Mar
  Apr
  May
  Jun
  Jul
  Aug
  Sep
  Oct
  Nov
  Dec
}

/// use this to get the current time in the local timezone offset
pub fn now() -> Time {
  let ts = timestamp.system_time()
  let offset = calendar.local_offset()
  let monotonic_now = ffi_monotonic_now()
  let timezone = local_timezone()

  Time(
    ts,
    offset,
    option.map(timezone, fn(tz) {
      case list.any(zones.list, fn(item) { item.0 == tz }) {
        True -> option.Some(tz)
        False -> option.None
      }
    })
      |> option.flatten,
    option.Some(monotonic_now),
  )
}

/// use this to get the current time in utc
pub fn utc_now() -> Time {
  let ts = timestamp.system_time()
  let monotonic_now = ffi_monotonic_now()
  Time(
    ts,
    time_duration.seconds(0),
    option.Some("Etc/UTC"),
    option.Some(monotonic_now),
  )
}

/// use this to get the current time with a given offset.
///
/// some examples of acceptable offsets:
///
/// `"+330", "03:30", "-8:00","-7", "-0400", "03"`
pub fn now_with_offset(offset: String) -> Result(Time, Nil) {
  use offset_seconds <- result.try(parse_offset(offset))
  let ts = timestamp.system_time()
  let monotonic_now = ffi_monotonic_now()
  Time(
    ts,
    time_duration.seconds(offset_seconds),
    option.None,
    option.Some(monotonic_now),
  )
  |> Ok
}

pub fn now_with_timezone(timezone: String) -> Result(Time, Nil) {
  case list.key_find(zones.list, timezone) {
    Ok(offset_seconds) -> {
      let ts = timestamp.system_time()
      let monotonic_now = ffi_monotonic_now()
      Time(
        ts,
        time_duration.seconds(offset_seconds),
        option.Some(timezone),
        option.Some(monotonic_now),
      )
      |> Ok
    }

    Error(Nil) -> Error(Nil)
  }
}

pub fn monotonic_now() -> Int {
  ffi_monotonic_now()
}

/// returns if the time has passed
pub fn has_occured(value: Time) -> Bool {
  compare(now(), value) == order.Gt
}

/// returns a string which is the date part of an ISO8601 string along with the offset
pub fn to_date_string(value: Time) -> String {
  let #(#(year, month, day), _, offset) = to_parts(value)

  int.to_string(year)
  <> "-"
  <> {
    month
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "-"
  <> {
    day
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> offset
}

/// like `to_date_string` except it does not contain the offset
pub fn to_naive_date_string(value: Time) -> String {
  let #(#(year, month, day), _, _) = to_parts(value)

  int.to_string(year)
  <> "-"
  <> {
    month
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "-"
  <> {
    day
    |> int.to_string
    |> string.pad_start(2, "0")
  }
}

/// returns a string which is the time part of an ISO8601 string along with the offset
pub fn to_time_string(value: Time) -> String {
  let #(_, #(hour, minute, second, milli_second), offset) = to_parts(value)

  {
    hour
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    minute
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    second
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "."
  <> {
    milli_second
    |> int.to_string
    |> string.pad_start(3, "0")
  }
  <> offset
}

/// like `to_time_string` except it does not contain the offset
pub fn to_naive_time_string(value: Time) -> String {
  let #(_, #(hour, minute, second, milli_second), _) = to_parts(value)

  {
    hour
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    minute
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    second
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "."
  <> {
    milli_second
    |> int.to_string
    |> string.pad_start(3, "0")
  }
}

pub fn to_iso8601(value: Time) -> String {
  let #(#(year, month, day), #(hour, minute, second, milli_second), offset) =
    to_parts(value)

  int.to_string(year)
  <> "-"
  <> {
    month
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "-"
  <> {
    day
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "T"
  <> {
    hour
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    minute
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    second
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "."
  <> {
    milli_second
    |> int.to_string
    |> string.pad_start(3, "0")
  }
  <> offset
}

/// if you need to parse an `ISO8601` string, this is probably what you're looking for.
///
/// given the huge surface area that `ISO8601` covers, it does not make sense for `birl`
/// to support all of it in one function, so this function parses only strings for which both
/// day and time of day can be extracted or deduced. Some acceptable examples are given below:
///
///   - `2019t14-4` -> `2019-01-01T14:00:00.000-04:00`
///
///   - `2019-03-26t14:00.9z` -> `2019-03-26T14:00:00.900Z`
///
///   - `2019-03-26+330` -> `2019-03-26T00:00:00.000+03:30`
///
///   - `20190326t1400-4` -> `2019-03-26T14:00:00.000-04:00`
///
///   - `19051222T16:38-3` -> `1905-12-22T16:38:00.000-03:00`
///
///   - `2019-03-26 14:30:00.9Z` -> `2019-03-26T14:30:00.900Z`
///
///   - `2019-03-26T14:00:00.9Z` -> `2019-03-26T14:00:00.900Z`
///
///   - `1905-12-22 16:38:23-3` -> `1905-12-22T16:38:23.000-03:00`
///
///   - `2019-03-26T14:00:00,4999Z` -> `2019-03-26T14:00:00.499Z`
///
///   - `1905-12-22T163823+0330` -> `1905-12-22T16:38:23.000+03:30`
///
///   - `1905-12-22T16:38:23.000+03:30` -> `1905-12-22T16:38:23.000+03:30`
pub fn parse(value: String) -> Result(Time, Nil) {
  let assert Ok(offset_pattern) = regexp.from_string("(.*)([+|\\-].*)")
  let value = string.trim(value)

  use #(day_string, offsetted_time_string) <- result.try(
    case
      string.split(value, "T"),
      string.split(value, "t"),
      string.split(value, " ")
    {
      [day_string, time_string], _, _
      | _, [day_string, time_string], _
      | _, _, [day_string, time_string]
      -> Ok(#(day_string, time_string))
      [_], [_], [_] -> Ok(#(value, "00"))
      _, _, _ -> Error(Nil)
    },
  )

  let day_string = string.trim(day_string)
  let offsetted_time_string = string.trim(offsetted_time_string)

  use #(day_string, time_string, offset_string) <- result.try(
    case
      string.ends_with(offsetted_time_string, "Z")
      || string.ends_with(offsetted_time_string, "z")
    {
      True ->
        Ok(#(day_string, string.drop_end(offsetted_time_string, 1), "+00:00"))
      False ->
        case regexp.scan(offset_pattern, offsetted_time_string) {
          [
            regexp.Match(
              _,
              [option.Some(time_string), option.Some(offset_string)],
            ),
          ] -> Ok(#(day_string, time_string, offset_string))
          _ ->
            case regexp.scan(offset_pattern, day_string) {
              [
                regexp.Match(
                  _,
                  [option.Some(day_string), option.Some(offset_string)],
                ),
              ] -> Ok(#(day_string, "00", offset_string))
              _ -> Error(Nil)
            }
        }
    },
  )

  let time_string = string.replace(time_string, ":", "")
  use #(time_string, milli_seconds_result) <- result.try(
    case string.split(time_string, "."), string.split(time_string, ",") {
      [_], [_] -> {
        Ok(#(time_string, Ok(0)))
      }
      [time_string, milli_seconds_string], [_]
      | [_], [time_string, milli_seconds_string]
      -> {
        Ok(#(
          time_string,
          milli_seconds_string
            |> string.slice(0, 3)
            |> string.pad_end(3, "0")
            |> int.parse,
        ))
      }

      _, _ -> Error(Nil)
    },
  )

  case milli_seconds_result {
    Ok(milli_seconds) -> {
      use day <- result.try(parse_date_section(day_string))
      let assert [year, month, date] = day

      use time_of_day <- result.try(parse_time_section(time_string))
      let assert [hour, minute, second] = time_of_day

      from_parts(
        #(year, month, date),
        #(hour, minute, second, milli_seconds),
        offset_string,
      )
    }

    Error(Nil) -> Error(Nil)
  }
}

/// this function parses `ISO8601` strings in which no date is specified, which
/// means such inputs don't actually represent a particular moment in time. That's why
/// the result of this function is an instance of `TimeOfDay` along with the offset specificed
/// in the string. Some acceptable examples are given below:
///
///   - `t25z` -> `#(TimeOfDay(2, 5, 0, 0), "Z")`
///
///   - `14-4` -> `#(TimeOfDay(14, 0, 0, 0), "-04:00")`
///
///   - `T145+4` -> `#(TimeOfDay(14, 5, 0, 0), "+04:00")`
///
///   - `16:38-3` -> `#(TimeOfDay(16, 38, 0, 0), "-03:00")`
///
///   - `t14:65.9z` -> `#(TimeOfDay(14, 6, 5, 900), "-04:00")`
///
///   - `163823+0330` -> `#(TimeOfDay(16, 38, 23, 0), "+03:30")`
///
///   - `T16:38:23.050+03:30` -> `#(TimeOfDay(16, 38, 23, 50), "+03:30")`
pub fn parse_time_of_day(value: String) -> Result(#(TimeOfDay, String), Nil) {
  let assert Ok(offset_pattern) = regexp.from_string("(.*)([+|\\-].*)")

  let time_string = case
    string.starts_with(value, "T"),
    string.starts_with(value, "t")
  {
    True, _ | _, True -> string.drop_start(value, 1)
    _, _ -> value
  }

  use #(time_string, offset_string) <- result.try(
    case
      string.ends_with(time_string, "Z") || string.ends_with(time_string, "z")
    {
      True -> Ok(#(string.drop_end(value, 1), "+00:00"))
      False ->
        case regexp.scan(offset_pattern, value) {
          [
            regexp.Match(
              _,
              [option.Some(time_string), option.Some(offset_string)],
            ),
          ] -> Ok(#(time_string, offset_string))
          _ -> Error(Nil)
        }
    },
  )

  let time_string = string.replace(time_string, ":", "")

  use #(time_string, milli_seconds_result) <- result.try(
    case string.split(time_string, "."), string.split(time_string, ",") {
      [_], [_] -> {
        Ok(#(time_string, Ok(0)))
      }
      [time_string, milli_seconds_string], [_]
      | [_], [time_string, milli_seconds_string]
      -> {
        Ok(#(
          time_string,
          milli_seconds_string
            |> string.slice(0, 3)
            |> string.pad_end(3, "0")
            |> int.parse,
        ))
      }
      _, _ -> Error(Nil)
    },
  )

  case milli_seconds_result {
    Ok(milli_seconds) -> {
      use time_of_day <- result.try(parse_time_section(time_string))
      let assert [hour, minute, second] = time_of_day

      use offset_seconds <- result.try(parse_offset(offset_string))
      use offset_string <- result.try(
        generate_offset(time_duration.seconds(offset_seconds)),
      )

      // Convert milliseconds to nanoseconds for TimeOfDay
      Ok(#(
        TimeOfDay(hour, minute, second, milli_seconds * 1_000_000),
        offset_string,
      ))
    }
    Error(Nil) -> Error(Nil)
  }
}

/// accepts fromats similar to the ones listed for `parse_time_of_day` except that there shoundn't be any offset information
pub fn parse_naive_time_of_day(
  value: String,
) -> Result(#(TimeOfDay, String), Nil) {
  let time_string = case
    string.starts_with(value, "T"),
    string.starts_with(value, "t")
  {
    True, _ | _, True -> string.drop_start(value, 1)
    _, _ -> value
  }

  let time_string = string.replace(time_string, ":", "")

  use #(time_string, milli_seconds_result) <- result.try(
    case string.split(time_string, "."), string.split(time_string, ",") {
      [_], [_] -> {
        Ok(#(time_string, Ok(0)))
      }
      [time_string, milli_seconds_string], [_]
      | [_], [time_string, milli_seconds_string]
      -> {
        Ok(#(
          time_string,
          milli_seconds_string
            |> string.slice(0, 3)
            |> string.pad_end(3, "0")
            |> int.parse,
        ))
      }
      _, _ -> Error(Nil)
    },
  )

  case milli_seconds_result {
    Ok(milli_seconds) -> {
      use time_of_day <- result.try(parse_time_section(time_string))
      let assert [hour, minute, second] = time_of_day

      // Convert milliseconds to nanoseconds for TimeOfDay
      Ok(#(TimeOfDay(hour, minute, second, milli_seconds * 1_000_000), "Z"))
    }
    Error(Nil) -> Error(Nil)
  }
}

pub fn time_of_day_to_string(value: TimeOfDay) -> String {
  int.to_string(value.hour)
  <> ":"
  <> int.to_string(value.minute)
  |> string.pad_start(2, "0")
  <> ":"
  <> int.to_string(value.second)
  |> string.pad_start(2, "0")
  <> "."
  <> int.to_string(value.nanosecond / 1_000_000)
  |> string.pad_start(3, "0")
}

pub fn time_of_day_to_short_string(value: TimeOfDay) -> String {
  int.to_string(value.hour)
  <> ":"
  <> int.to_string(value.minute)
  |> string.pad_start(2, "0")
}

/// the naive format is the same as ISO8601 except that it does not contain the offset
pub fn to_naive(value: Time) -> String {
  let #(#(year, month, day), #(hour, minute, second, milli_second), _) =
    to_parts(value)

  int.to_string(year)
  <> "-"
  <> {
    month
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "-"
  <> {
    day
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "T"
  <> {
    hour
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    minute
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    second
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> "."
  <> {
    milli_second
    |> int.to_string
    |> string.pad_start(3, "0")
  }
}

/// accepts fromats similar to the ones listed for `parse` except that there shoundn't be any offset information
pub fn from_naive(value: String) -> Result(Time, Nil) {
  let value = string.trim(value)

  use #(day_string, time_string) <- result.try(
    case
      string.split(value, "T"),
      string.split(value, "t"),
      string.split(value, " ")
    {
      [day_string, time_string], _, _
      | _, [day_string, time_string], _
      | _, _, [day_string, time_string]
      -> Ok(#(day_string, time_string))
      [_], [_], [_] -> Ok(#(value, "00"))
      _, _, _ -> Error(Nil)
    },
  )

  let day_string = string.trim(day_string)
  let time_string = string.trim(time_string)

  let time_string = string.replace(time_string, ":", "")
  use #(time_string, milli_seconds_result) <- result.try(
    case string.split(time_string, "."), string.split(time_string, ",") {
      [_], [_] -> Ok(#(time_string, Ok(0)))

      [time_string, milli_seconds_string], [_]
      | [_], [time_string, milli_seconds_string]
      ->
        Ok(#(
          time_string,
          milli_seconds_string
            |> string.slice(0, 3)
            |> string.pad_end(3, "0")
            |> int.parse,
        ))

      _, _ -> Error(Nil)
    },
  )

  case milli_seconds_result {
    Ok(milli_seconds) -> {
      use day <- result.try(parse_date_section(day_string))
      let assert [year, month, date] = day

      use time_of_day <- result.try(parse_time_section(time_string))
      let assert [hour, minute, second] = time_of_day

      from_parts(
        #(year, month, date),
        #(hour, minute, second, milli_seconds),
        "Z",
      )
    }

    Error(Nil) -> Error(Nil)
  }
}

/// see [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)
pub fn to_http(value: Time) -> String {
  let assert Ok(value) = set_offset(value, "Z")
  let #(#(year, _, day), #(hour, minute, second, _), _) = to_parts(value)
  let short_weekday = short_string_weekday(value)
  let short_month = short_string_month(value)

  short_weekday
  <> ", "
  <> {
    day
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> " "
  <> short_month
  <> " "
  <> int.to_string(year)
  <> " "
  <> {
    hour
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    minute
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    second
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> " GMT"
}

/// like `to_http` but assumes the offset in the DateTime value instead of `GMT`
pub fn to_http_with_offset(value: Time) -> String {
  let #(#(year, _, day), #(hour, minute, second, _), offset) = to_parts(value)
  let short_weekday = short_string_weekday(value)
  let short_month = short_string_month(value)

  let offset = case offset {
    "Z" -> "GMT"
    _ -> offset
  }

  short_weekday
  <> ", "
  <> {
    day
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> " "
  <> short_month
  <> " "
  <> int.to_string(year)
  <> " "
  <> {
    hour
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    minute
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> ":"
  <> {
    second
    |> int.to_string
    |> string.pad_start(2, "0")
  }
  <> " "
  <> offset
}

/// see [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)
///
/// also supports other similar formats:
///
///   - `Tue, 01-Nov-2016 08:49:37 GMT`
///
///   - `Tue, 01 Nov 2016 08:49:37 +0630`
///
///   - `Tue, 01-November-2016 08:49:37 Z`
///
///   - `Tuesday, 01-Nov-2016 08:49:37 +330`
///
///   - `Tuesday, 01 November 2016 08:49:37 +06:30`
pub fn from_http(value: String) -> Result(Time, Nil) {
  let value = string.trim(value)
  use #(weekday, rest) <- result.try(string.split_once(value, ","))

  use <- bool.guard(
    !list.any(weekday_strings, fn(weekday_item) {
      let strings = weekday_item.1
      strings.0 == weekday || strings.1 == weekday
    }),
    Error(Nil),
  )

  let rest = string.trim(rest)
  let assert Ok(whitespace_pattern) = regexp.from_string("\\s+")
  case regexp.split(whitespace_pattern, rest) {
    [day_string, month_string, year_string, time_string, offset_string] -> {
      let time_string = string.replace(time_string, ":", "")
      case
        int.parse(day_string),
        list.index_map(month_strings, fn(month, index) {
          let strings = month.1
          #(index, strings.0, strings.1)
        })
        |> list.find(fn(month) {
          month.1 == month_string || month.2 == month_string
        }),
        int.parse(year_string),
        parse_time_section(time_string)
      {
        Ok(day), Ok(#(month_index, _, _)), Ok(year), Ok([hour, minute, second]) ->
          case
            from_parts(
              #(year, month_index + 1, day),
              #(hour, minute, second, 0),
              case offset_string {
                "GMT" -> "Z"
                _ -> offset_string
              },
            )
          {
            Ok(value) -> {
              let correct_weekday = string_weekday(value)
              let correct_short_weekday = short_string_weekday(value)

              case
                list.contains([correct_weekday, correct_short_weekday], weekday)
              {
                True -> Ok(value)
                False -> Error(Nil)
              }
            }
            Error(Nil) -> Error(Nil)
          }
        _, _, _, _ -> Error(Nil)
      }
    }

    [day_string, time_string, offset_string] ->
      case string.split(day_string, "-") {
        [day_string, month_string, year_string] -> {
          let time_string = string.replace(time_string, ":", "")
          case
            int.parse(day_string),
            list.index_map(month_strings, fn(month, index) {
              let strings = month.1
              #(index, strings.0, strings.1)
            })
            |> list.find(fn(month) {
              month.1 == month_string || month.2 == month_string
            }),
            int.parse(year_string),
            parse_time_section(time_string)
          {
            Ok(day),
              Ok(#(month_index, _, _)),
              Ok(year),
              Ok([hour, minute, second])
            ->
              case
                from_parts(
                  #(year, month_index + 1, day),
                  #(hour, minute, second, 0),
                  case offset_string {
                    "GMT" -> "Z"
                    _ -> offset_string
                  },
                )
              {
                Ok(value) -> {
                  let correct_weekday = string_weekday(value)
                  let correct_short_weekday = short_string_weekday(value)

                  case
                    list.contains(
                      [correct_weekday, correct_short_weekday],
                      weekday,
                    )
                  {
                    True -> Ok(value)
                    False -> Error(Nil)
                  }
                }
                Error(Nil) -> Error(Nil)
              }
            _, _, _, _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

/// unix timestamps are the number of seconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn to_unix(value: Time) -> Int {
  let Time(timestamp: ts, ..) = value
  let #(seconds, _) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  seconds
}

/// unix timestamps are the number of seconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn from_unix(value: Int) -> Time {
  Time(
    timestamp.from_unix_seconds(value),
    time_duration.seconds(0),
    option.None,
    option.None,
  )
}

/// unix milli timestamps are the number of milliseconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn to_unix_milli(value: Time) -> Int {
  let Time(timestamp: ts, ..) = value
  let #(seconds, nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  seconds * 1000 + nanoseconds / 1_000_000
}

/// unix milli timestamps are the number of milliseconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn from_unix_milli(value: Int) -> Time {
  let seconds = value / 1000
  let nanoseconds = { value % 1000 } * 1_000_000
  Time(
    timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds),
    time_duration.seconds(0),
    option.None,
    option.None,
  )
}

/// unix micro timestamps are the number of microseconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn to_unix_micro(value: Time) -> Int {
  let Time(timestamp: ts, ..) = value
  let #(seconds, nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  seconds * 1_000_000 + nanoseconds / 1000
}

/// unix micro timestamps are the number of microseconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn from_unix_micro(value: Int) -> Time {
  let seconds = value / 1_000_000
  let nanoseconds = { value % 1_000_000 } * 1000
  Time(
    timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds),
    time_duration.seconds(0),
    option.None,
    option.None,
  )
}

/// unix nano timestamps are the number of nanoseconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn to_unix_nano(value: Time) -> Int {
  let Time(timestamp: ts, ..) = value
  let #(seconds, nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  seconds * 1_000_000_000 + nanoseconds
}

/// unix nano timestamps are the number of nanoseconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn from_unix_nano(value: Int) -> Time {
  let seconds = value / 1_000_000_000
  let nanoseconds = value % 1_000_000_000
  Time(
    timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds),
    time_duration.seconds(0),
    option.None,
    option.None,
  )
}

pub fn compare(a: Time, b: Time) -> order.Order {
  let Time(timestamp: tsa, monotonic_time: mta, ..) = a
  let Time(timestamp: tsb, monotonic_time: mtb, ..) = b

  // Prefer monotonic time for comparison if both have it
  case mta, mtb {
    option.Some(ma), option.Some(mb) -> int.compare(ma, mb)
    _, _ -> timestamp.compare(tsa, tsb)
  }
}

pub fn difference(a: Time, b: Time) -> duration.Duration {
  let Time(timestamp: tsa, monotonic_time: mta, ..) = a
  let Time(timestamp: tsb, monotonic_time: mtb, ..) = b

  // Prefer monotonic time for difference if both have it
  case mta, mtb {
    option.Some(ma), option.Some(mb) -> {
      // Monotonic time is in microseconds, convert to nanoseconds for Duration
      let diff_micros = ma - mb
      duration.micro_seconds(diff_micros)
    }
    _, _ -> timestamp.difference(tsa, tsb)
  }
}

const string_to_units = [
  #("year", duration.Year),
  #("month", duration.Month),
  #("week", duration.Week),
  #("day", duration.Day),
  #("hour", duration.Hour),
  #("minute", duration.Minute),
  #("second", duration.Second),
]

/// you could say this is the opposite of `legible_difference`
///
/// ```gleam
/// > parse_relative(birl.now(), "8 minutes ago")
/// ```
pub fn parse_relative(origin: Time, legible_difference: String) {
  case string.split(legible_difference, " ") {
    ["in", amount_string, unit]
    | [amount_string, unit, "from now"]
    | [amount_string, unit, "later"]
    | [amount_string, unit, "ahead"]
    | [amount_string, unit, "in the future"]
    | [amount_string, unit, "hence"] -> {
      let unit = case string.ends_with(unit, "s") {
        False -> unit
        True -> string.drop_end(unit, 1)
      }

      use amount <- result.try(int.parse(amount_string))
      use unit <- result.try(list.key_find(string_to_units, unit))
      Ok(add(origin, duration.new([#(amount, unit)])))
    }

    [amount_string, unit, "ago"]
    | [amount_string, unit, "before"]
    | [amount_string, unit, "earlier"]
    | [amount_string, unit, "since"]
    | [amount_string, unit, "in the past"] -> {
      let unit = case string.ends_with(unit, "s") {
        False -> unit
        True -> string.drop_end(unit, 1)
      }

      use amount <- result.try(int.parse(amount_string))
      use unit <- result.try(list.key_find(string_to_units, unit))
      Ok(subtract(origin, duration.new([#(amount, unit)])))
    }

    _ -> Error(Nil)
  }
}

const units_to_string = [
  #(duration.Year, "year"),
  #(duration.Month, "month"),
  #(duration.Week, "week"),
  #(duration.Day, "day"),
  #(duration.Hour, "hour"),
  #(duration.Minute, "minute"),
  #(duration.Second, "second"),
]

pub fn legible_difference(a: Time, b: Time) -> String {
  case
    difference(a, b)
    |> duration.blur
  {
    #(_, duration.MicroSecond) | #(_, duration.MilliSecond) -> "just now"

    #(amount, unit) -> {
      let assert Ok(unit) = list.key_find(units_to_string, unit)
      let is_negative = amount < 0
      let amount = int.absolute_value(amount)

      let unit = case amount {
        1 -> unit
        _ -> unit <> "s"
      }

      case is_negative {
        True -> "in " <> int.to_string(amount) <> " " <> unit
        False ->
          amount
          |> int.absolute_value
          |> int.to_string
          <> " "
          <> unit
          <> " ago"
      }
    }
  }
}

pub fn add(value: Time, dur: duration.Duration) -> Time {
  let Time(timestamp: ts, offset: o, timezone: timezone, monotonic_time: mt) =
    value
  let new_ts = timestamp.add(ts, dur)

  // Also update monotonic time if present (it's in microseconds)
  let dur_micros = duration_to_microseconds(dur)
  let new_mt = option.map(mt, fn(m) { m + dur_micros })

  Time(timestamp: new_ts, offset: o, timezone: timezone, monotonic_time: new_mt)
}

pub fn subtract(value: Time, dur: duration.Duration) -> Time {
  let Time(timestamp: ts, offset: o, timezone: timezone, monotonic_time: mt) =
    value
  // gleam_time doesn't have subtract, so negate the duration and add
  // difference(a, b) = b - a, so difference(dur, 0) = 0 - dur = -dur
  let negated_dur = time_duration.difference(dur, time_duration.seconds(0))
  let new_ts = timestamp.add(ts, negated_dur)

  // Also update monotonic time if present (it's in microseconds)
  let dur_micros = duration_to_microseconds(dur)
  let new_mt = option.map(mt, fn(m) { m - dur_micros })

  Time(timestamp: new_ts, offset: o, timezone: timezone, monotonic_time: new_mt)
}

pub fn weekday(value: Time) -> Weekday {
  let Time(timestamp: ts, offset: offset, ..) = value
  // Get unix timestamp in microseconds and offset in microseconds for FFI
  let #(seconds, nanoseconds) = timestamp.to_unix_seconds_and_nanoseconds(ts)
  let micros = seconds * 1_000_000 + nanoseconds / 1000
  let #(offset_seconds, _) = time_duration.to_seconds_and_nanoseconds(offset)
  let offset_micros = offset_seconds * 1_000_000

  let assert Ok(wd) = weekday_from_int(ffi_weekday(micros, offset_micros))
  wd
}

pub fn string_weekday(value: Time) -> String {
  weekday(value)
  |> weekday_to_string
}

pub fn short_string_weekday(value: Time) -> String {
  weekday(value)
  |> weekday_to_short_string
}

pub fn weekday_to_string(value: Weekday) -> String {
  case value {
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"
  }
}

pub fn weekday_to_short_string(value: Weekday) -> String {
  case value {
    Mon -> "Mon"
    Tue -> "Tue"
    Wed -> "Wed"
    Thu -> "Thu"
    Fri -> "Fri"
    Sat -> "Sat"
    Sun -> "Sun"
  }
}

pub fn parse_weekday(value: String) -> Result(Weekday, Nil) {
  let lowercase = string.lowercase(value)
  let weekday =
    list.find(weekday_strings, fn(weekday_string) {
      let #(_, #(long, short)) = weekday_string
      lowercase == string.lowercase(short)
      || lowercase == string.lowercase(long)
    })
  weekday
  |> result.map(fn(weekday) { weekday.0 })
}

pub fn parse_month(value: String) -> Result(Month, Nil) {
  let lowercase = string.lowercase(value)
  let month =
    list.find(month_strings, fn(month_string) {
      let #(_, #(long, short)) = month_string
      lowercase == string.lowercase(short)
      || lowercase == string.lowercase(long)
    })
  month
  |> result.map(fn(month) { month.0 })
}

pub fn month(value: Time) -> Month {
  let #(#(_, month, _), _, _) = to_parts(value)
  let assert Ok(month) = month_from_int(month)
  month
}

pub fn string_month(value: Time) -> String {
  case month(value) {
    Jan -> "January"
    Feb -> "February"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"
  }
}

pub fn short_string_month(value: Time) -> String {
  case month(value) {
    Jan -> "Jan"
    Feb -> "Feb"
    Mar -> "Mar"
    Apr -> "Apr"
    May -> "May"
    Jun -> "Jun"
    Jul -> "Jul"
    Aug -> "Aug"
    Sep -> "Sep"
    Oct -> "Oct"
    Nov -> "Nov"
    Dec -> "Dec"
  }
}

/// can be used to create a time range starting from time `a` with step `s`
///
/// if `b` is `option.None` the range will be infinite
pub fn range(from a: Time, to b: option.Option(Time), step s: duration.Duration) {
  let assert Ok(range) = case b {
    option.Some(b) ->
      ranger.create(
        validate: fn(_) { True },
        negate_step: fn(dur) {
          time_duration.difference(dur, time_duration.seconds(0))
        },
        add: add,
        compare: compare,
      )(a, b, s)
    option.None ->
      ranger.create_infinite(
        validate: fn(_) { True },
        add: add,
        compare: compare,
      )(a, s)
  }
  range
}

/// WARNING: Does not respect daylight saving time!
pub fn set_timezone(value: Time, new_timezone: String) -> Result(Time, Nil) {
  case list.key_find(zones.list, new_timezone) {
    Ok(new_offset_seconds) -> {
      let Time(timestamp: ts, monotonic_time: mt, ..) = value
      Time(
        ts,
        time_duration.seconds(new_offset_seconds),
        option.Some(new_timezone),
        mt,
      )
      |> Ok
    }

    Error(Nil) -> Error(Nil)
  }
}

pub fn get_timezone(value: Time) -> option.Option(String) {
  let Time(timezone: timezone, ..) = value
  timezone
}

/// use this to change the offset of a given time value.
///
/// some examples of acceptable offsets:
///
/// `"+330", "03:30", "-8:00","-7", "-0400", "03", "Z"`
pub fn set_offset(value: Time, new_offset: String) -> Result(Time, Nil) {
  use new_offset_seconds <- result.try(parse_offset(new_offset))
  let Time(timestamp: ts, timezone: timezone, monotonic_time: mt, ..) = value
  Time(ts, time_duration.seconds(new_offset_seconds), timezone, mt)
  |> Ok
}

pub fn get_offset(value: Time) -> String {
  let Time(offset: offset, ..) = value
  let assert Ok(offset_str) = generate_offset(offset)
  offset_str
}

/// Get the offset as a gleam_time Duration
pub fn get_offset_duration(value: Time) -> time_duration.Duration {
  let Time(offset: offset, ..) = value
  offset
}

pub fn set_day(value: Time, day: Day) -> Time {
  let #(_, time, offset_str) = to_parts(value)
  let Day(year, month, date) = day
  let assert Ok(new_value) = from_parts(#(year, month, date), time, offset_str)

  Time(
    new_value.timestamp,
    new_value.offset,
    value.timezone,
    value.monotonic_time,
  )
}

pub fn get_day(value: Time) -> Day {
  let #(#(year, month, day), _, _) = to_parts(value)
  Day(year, month, day)
}

pub fn set_time_of_day(value: Time, time: TimeOfDay) -> Time {
  let #(date, _, offset_str) = to_parts(value)
  // TimeOfDay now uses nanoseconds, convert to milliseconds for from_parts
  let TimeOfDay(hour, minute, second, nanosecond) = time
  let milli_second = nanosecond / 1_000_000
  let assert Ok(new_value) =
    from_parts(date, #(hour, minute, second, milli_second), offset_str)

  Time(
    new_value.timestamp,
    new_value.offset,
    value.timezone,
    value.monotonic_time,
  )
}

pub fn get_time_of_day(value: Time) -> TimeOfDay {
  let #(_, #(hour, minute, second, milli_second), _) = to_parts(value)
  // Convert milliseconds to nanoseconds for TimeOfDay
  TimeOfDay(hour, minute, second, milli_second * 1_000_000)
}

@target(erlang)
/// calculates erlang datetime using the offset in the DateTime value
pub fn to_erlang_datetime(value: Time) -> #(#(Int, Int, Int), #(Int, Int, Int)) {
  let #(date, #(hour, minute, second, _), _) = to_parts(value)
  #(date, #(hour, minute, second))
}

@target(erlang)
/// calculates the universal erlang datetime regardless of the offset in the DateTime value
pub fn to_erlang_universal_datetime(
  value: Time,
) -> #(#(Int, Int, Int), #(Int, Int, Int)) {
  let assert Ok(value) = set_offset(value, "Z")
  let #(date, #(hour, minute, second, _), _) = to_parts(value)
  #(date, #(hour, minute, second))
}

@target(erlang)
/// calculates the DateTime value from the erlang datetime using the local offset of the system
pub fn from_erlang_local_datetime(
  erlang_datetime: #(#(Int, Int, Int), #(Int, Int, Int)),
) -> Time {
  let #(date, time) = erlang_datetime
  let offset = calendar.local_offset()

  let base =
    unix_epoch()
    |> set_day(Day(date.0, date.1, date.2))
    |> set_time_of_day(TimeOfDay(time.0, time.1, time.2, 0))

  let timezone = local_timezone()

  Time(
    base.timestamp,
    offset,
    option.map(timezone, fn(tz) {
      case list.any(zones.list, fn(item) { item.0 == tz }) {
        True -> option.Some(tz)
        False -> option.None
      }
    })
      |> option.flatten,
    option.None,
  )
}

@target(erlang)
/// calculates the DateTime value from the erlang datetime in UTC
pub fn from_erlang_universal_datetime(
  erlang_datetime: #(#(Int, Int, Int), #(Int, Int, Int)),
) -> Time {
  let #(date, time) = erlang_datetime
  let assert Ok(new_value) =
    unix_epoch()
    |> set_day(Day(date.0, date.1, date.2))
    |> set_time_of_day(TimeOfDay(time.0, time.1, time.2, 0))
    |> set_timezone("Etc/UTC")
  new_value
}

fn from_parts(
  date: #(Int, Int, Int),
  time: #(Int, Int, Int, Int),
  offset: String,
) -> Result(Time, Nil) {
  use offset_seconds <- result.try(parse_offset(offset))
  let #(year, month, day) = date
  let #(hour, minute, second, milli_second) = time

  // Convert to gleam_time types
  use gleam_month <- result.try(
    calendar.month_from_int(month)
    |> result.replace_error(Nil),
  )

  let calendar_date = calendar.Date(year, gleam_month, day)
  // Store nanoseconds (milliseconds * 1_000_000)
  let calendar_time =
    calendar.TimeOfDay(hour, minute, second, milli_second * 1_000_000)
  let offset_duration = time_duration.seconds(offset_seconds)

  let ts =
    timestamp.from_calendar(calendar_date, calendar_time, offset_duration)
  Time(ts, offset_duration, option.None, option.None)
  |> Ok
}

fn to_parts(value: Time) -> #(#(Int, Int, Int), #(Int, Int, Int, Int), String) {
  let Time(timestamp: ts, offset: offset, ..) = value

  let #(date, time) = timestamp.to_calendar(ts, offset)
  let month_int = calendar.month_to_int(date.month)
  // Convert nanoseconds to milliseconds
  let milli_second = time.nanoseconds / 1_000_000

  let assert Ok(offset_string) = generate_offset(offset)
  #(
    #(date.year, month_int, date.day),
    #(time.hours, time.minutes, time.seconds, milli_second),
    offset_string,
  )
}

/// Parse an offset string and return the offset in seconds
fn parse_offset(offset: String) -> Result(Int, Nil) {
  use <- bool.guard(list.contains(["Z", "z"], offset), Ok(0))
  let assert Ok(re) = regexp.from_string("([+-])")

  use #(sign, offset) <- result.try(case regexp.split(re, offset) {
    ["", "+", offset] -> Ok(#(1, offset))
    ["", "-", offset] -> Ok(#(-1, offset))
    [_] -> Ok(#(1, offset))
    _ -> Error(Nil)
  })

  case string.split(offset, ":") {
    [hour_str, minute_str] -> {
      use hour <- result.try(int.parse(hour_str))
      use minute <- result.try(int.parse(minute_str))
      Ok(sign * { hour * 60 + minute } * 60)
    }
    [offset] ->
      case string.length(offset) {
        1 -> {
          use hour <- result.try(int.parse(offset))
          Ok(sign * hour * 3600)
        }
        2 -> {
          use number <- result.try(int.parse(offset))
          case number < 14 {
            True -> Ok(sign * number * 3600)
            False -> Ok(sign * { number / 10 * 60 + number % 10 } * 60)
          }
        }
        3 -> {
          let assert Ok(hour_str) = string.first(offset)
          let minute_str = string.slice(offset, 1, 2)
          use hour <- result.try(int.parse(hour_str))
          use minute <- result.try(int.parse(minute_str))
          Ok(sign * { hour * 60 + minute } * 60)
        }
        4 -> {
          let hour_str = string.slice(offset, 0, 2)
          let minute_str = string.slice(offset, 2, 2)
          use hour <- result.try(int.parse(hour_str))
          use minute <- result.try(int.parse(minute_str))
          Ok(sign * { hour * 60 + minute } * 60)
        }
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn duration_to_microseconds(dur: time_duration.Duration) -> Int {
  let #(seconds, nanoseconds) = time_duration.to_seconds_and_nanoseconds(dur)
  seconds * 1_000_000 + nanoseconds / 1000
}

fn generate_offset(offset: time_duration.Duration) -> Result(String, Nil) {
  let #(total_seconds, _) = time_duration.to_seconds_and_nanoseconds(offset)
  use <- bool.guard(total_seconds == 0, Ok("Z"))

  let is_negative = total_seconds < 0
  let abs_seconds = int.absolute_value(total_seconds)
  let hours = abs_seconds / 3600
  let minutes = { abs_seconds % 3600 } / 60

  let sign = case is_negative {
    True -> "-"
    False -> "+"
  }

  let hour_str =
    hours
    |> int.to_string
    |> string.pad_start(2, "0")

  let minute_str =
    minutes
    |> int.to_string
    |> string.pad_start(2, "0")

  Ok(sign <> hour_str <> ":" <> minute_str)
}

fn parse_date_section(date: String) -> Result(List(Int), Nil) {
  use <- bool.guard(is_invalid_date(date), Error(Nil))

  case string.contains(date, "-") {
    True -> {
      let assert Ok(dash_pattern) =
        regexp.from_string(
          "(\\d{4})(?:-(1[0-2]|0?[0-9]))?(?:-(3[0-1]|[1-2][0-9]|0?[0-9]))?",
        )

      case regexp.scan(dash_pattern, date) {
        [regexp.Match(_, [option.Some(major)])] -> [
          int.parse(major),
          Ok(1),
          Ok(1),
        ]

        [regexp.Match(_, [option.Some(major), option.Some(middle)])] -> [
          int.parse(major),
          int.parse(middle),
          Ok(1),
        ]

        [
          regexp.Match(
            _,
            [option.Some(major), option.Some(middle), option.Some(minor)],
          ),
        ] -> [int.parse(major), int.parse(middle), int.parse(minor)]

        _ -> [Error(Nil)]
      }
    }

    False ->
      parse_section(
        date,
        "(\\d{4})(1[0-2]|0?[0-9])?(3[0-1]|[1-2][0-9]|0?[0-9])?",
        1,
      )
  }
  |> list.try_map(function.identity)
}

fn parse_time_section(time: String) -> Result(List(Int), Nil) {
  use <- bool.guard(is_invalid_time(time), Error(Nil))

  parse_section(
    time,
    "(2[0-3]|1[0-9]|0?[0-9])([1-5][0-9]|0?[0-9])?([1-5][0-9]|0?[0-9])?",
    0,
  )
  |> list.try_map(function.identity)
}

fn is_invalid_date(date: String) -> Bool {
  date
  |> string.to_utf_codepoints
  |> list.map(string.utf_codepoint_to_int)
  |> list.any(fn(code) {
    case code {
      _ if code == 45 -> False
      _ if code >= 48 && code <= 57 -> False
      _ -> True
    }
  })
}

fn is_invalid_time(time: String) -> Bool {
  time
  |> string.to_utf_codepoints
  |> list.map(string.utf_codepoint_to_int)
  |> list.any(fn(code) {
    case code {
      _ if code >= 48 && code <= 58 -> False
      _ -> True
    }
  })
}

fn parse_section(
  section: String,
  pattern_string: String,
  default: Int,
) -> List(Result(Int, Nil)) {
  let assert Ok(pattern) = regexp.from_string(pattern_string)
  case regexp.scan(pattern, section) {
    [regexp.Match(_, [option.Some(major)])] -> [
      int.parse(major),
      Ok(default),
      Ok(default),
    ]

    [regexp.Match(_, [option.Some(major), option.Some(middle)])] -> [
      int.parse(major),
      int.parse(middle),
      Ok(default),
    ]

    [
      regexp.Match(
        _,
        [option.Some(major), option.Some(middle), option.Some(minor)],
      ),
    ] -> [int.parse(major), int.parse(middle), int.parse(minor)]

    _ -> [Error(Nil)]
  }
}

@target(erlang)
fn weekday_from_int(weekday: Int) -> Result(Weekday, Nil) {
  case weekday {
    0 -> Ok(Mon)
    1 -> Ok(Tue)
    2 -> Ok(Wed)
    3 -> Ok(Thu)
    4 -> Ok(Fri)
    5 -> Ok(Sat)
    6 -> Ok(Sun)
    _ -> Error(Nil)
  }
}

@target(javascript)
fn weekday_from_int(weekday: Int) -> Result(Weekday, Nil) {
  case weekday {
    0 -> Ok(Sun)
    1 -> Ok(Mon)
    2 -> Ok(Tue)
    3 -> Ok(Wed)
    4 -> Ok(Thu)
    5 -> Ok(Fri)
    6 -> Ok(Sat)
    _ -> Error(Nil)
  }
}

fn month_from_int(month: Int) -> Result(Month, Nil) {
  case month {
    1 -> Ok(Jan)
    2 -> Ok(Feb)
    3 -> Ok(Mar)
    4 -> Ok(Apr)
    5 -> Ok(May)
    6 -> Ok(Jun)
    7 -> Ok(Jul)
    8 -> Ok(Aug)
    9 -> Ok(Sep)
    10 -> Ok(Oct)
    11 -> Ok(Nov)
    12 -> Ok(Dec)
    _ -> Error(Nil)
  }
}

const weekday_strings = [
  #(Mon, #("Monday", "Mon")),
  #(Tue, #("Tuesday", "Tue")),
  #(Wed, #("Wednesday", "Wed")),
  #(Thu, #("Thursday", "Thu")),
  #(Fri, #("Friday", "Fri")),
  #(Sat, #("Saturday", "Sat")),
  #(Sun, #("Sunday", "Sun")),
]

const month_strings = [
  #(Jan, #("January", "Jan")),
  #(Feb, #("February", "Feb")),
  #(Mar, #("March", "Mar")),
  #(Apr, #("April", "Apr")),
  #(May, #("May", "May")),
  #(Jun, #("June", "Jun")),
  #(Jul, #("July", "Jul")),
  #(Aug, #("August", "Aug")),
  #(Sep, #("September", "Sep")),
  #(Oct, #("October", "Oct")),
  #(Nov, #("November", "Nov")),
  #(Dec, #("December", "Dec")),
]

// ---------------------------------------------------------------------------
// FFI functions (retained for features gleam_time doesn't provide)
// ---------------------------------------------------------------------------

@external(erlang, "birl_ffi", "monotonic_now")
@external(javascript, "./birl_ffi.mjs", "monotonic_now")
fn ffi_monotonic_now() -> Int

@external(erlang, "birl_ffi", "weekday")
@external(javascript, "./birl_ffi.mjs", "weekday")
fn ffi_weekday(timestamp_micros: Int, offset_micros: Int) -> Int

@external(erlang, "birl_ffi", "local_timezone")
@external(javascript, "./birl_ffi.mjs", "local_timezone")
fn local_timezone() -> option.Option(String)

// ---------------------------------------------------------------------------
// gleam_time interoperability
// ---------------------------------------------------------------------------

/// Convert birl Time to gleam_time Timestamp.
///
/// Note: This conversion loses offset/timezone information since Timestamp
/// represents an absolute point in time (like UTC).
pub fn to_timestamp(value: Time) -> timestamp.Timestamp {
  let Time(timestamp: ts, ..) = value
  ts
}

/// Alias for to_timestamp - provides a consistent naming convention
pub fn to_gleam_timestamp(value: Time) -> timestamp.Timestamp {
  to_timestamp(value)
}

/// Convert gleam_time Timestamp to birl Time.
///
/// The resulting Time will be in UTC with no timezone information.
pub fn from_timestamp(ts: timestamp.Timestamp) -> Time {
  Time(ts, time_duration.seconds(0), option.Some("Etc/UTC"), option.None)
}

/// Alias for from_timestamp - provides a consistent naming convention
pub fn from_gleam_timestamp(ts: timestamp.Timestamp) -> Time {
  from_timestamp(ts)
}

/// Convert birl Day to gleam_time calendar.Date.
pub fn day_to_date(day: Day) -> calendar.Date {
  let assert Ok(month) = calendar.month_from_int(day.month)
  calendar.Date(day.year, month, day.date)
}

/// Convert gleam_time calendar.Date to birl Day.
pub fn date_to_day(date: calendar.Date) -> Day {
  Day(date.year, calendar.month_to_int(date.month), date.day)
}

/// Convert birl TimeOfDay to gleam_time calendar.TimeOfDay.
pub fn time_of_day_to_calendar(tod: TimeOfDay) -> calendar.TimeOfDay {
  calendar.TimeOfDay(tod.hour, tod.minute, tod.second, tod.nanosecond)
}

/// Convert gleam_time calendar.TimeOfDay to birl TimeOfDay.
pub fn calendar_to_time_of_day(tod: calendar.TimeOfDay) -> TimeOfDay {
  TimeOfDay(tod.hours, tod.minutes, tod.seconds, tod.nanoseconds)
}
