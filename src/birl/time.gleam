import gleam/int
import gleam/bool
import gleam/list
import gleam/order
import gleam/regex
import gleam/result
import gleam/string
import gleam/option
import gleam/function
import gleam/iterator
import birl/duration
import birl/zones
import ranger

pub opaque type DateTime {
  DateTime(
    wall_time: Int,
    offset: Int,
    timezone: option.Option(String),
    monotonic_time: option.Option(Int),
  )
}

pub type Date {
  Date(year: Int, month: Int, day: Int)
}

pub type Time {
  Time(hour: Int, minute: Int, second: Int, milli_second: Int)
}

/// starting point of unix timestamps
pub const unix_epoch = DateTime(0, 0, option.None, option.None)

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
pub fn now() -> DateTime {
  let now = ffi_now()
  let offset_in_minutes = ffi_local_offset()
  let monotonic_now = ffi_monotonic_now()
  let timezone = local_timezone()

  DateTime(
    now,
    offset_in_minutes * 60_000_000,
    option.map(
      timezone,
      fn(tz) {
        case
          zones.list
          |> list.any(fn(item) { item.0 == tz })
        {
          True -> option.Some(tz)
          False -> option.None
        }
      },
    )
    |> option.flatten,
    option.Some(monotonic_now),
  )
}

/// use this to get the current time in utc
pub fn utc_now() -> DateTime {
  let now = ffi_now()
  let monotonic_now = ffi_monotonic_now()
  DateTime(now, 0, option.Some("Etc/UTC"), option.Some(monotonic_now))
}

/// use this to get the current time with a given offset.
///
/// Some examples of acceptable offsets:
///
/// `"+330", "03:30", "-8:00","-7", "-0400", "03"`
pub fn now_with_offset(offset: String) -> Result(DateTime, Nil) {
  use offset <- result.then(parse_offset(offset))
  let now = ffi_now()
  let monotonic_now = ffi_monotonic_now()
  DateTime(now, offset, option.None, option.Some(monotonic_now))
  |> Ok
}

pub fn now_with_timezone(timezone: String) -> Result(DateTime, Nil) {
  case
    zones.list
    |> list.key_find(timezone)
  {
    Ok(offset) -> {
      let now = ffi_now()
      let monotonic_now = ffi_monotonic_now()
      DateTime(
        now,
        offset * 1_000_000,
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

pub fn to_iso8601(value: DateTime) -> String {
  let #(#(year, month, day), #(hour, minute, second, milli_second), offset) =
    to_parts(value)

  int.to_string(year) <> "-" <> {
    month
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> "-" <> {
    day
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> "T" <> {
    hour
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    minute
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    second
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> "." <> {
    milli_second
    |> int.to_string
    |> string.pad_left(3, "0")
  } <> offset
}

pub fn from_iso8601(value: String) -> Result(DateTime, Nil) {
  let assert Ok(offset_pattern) = regex.from_string("(.*)([+|\\-].*)")
  let value = string.trim(value)

  let #(date_string, offsetted_time_string) = case string.split(value, "T") {
    [date_string] -> #(date_string, "00")
    [date_string, offsetted_time_string] -> #(
      date_string,
      offsetted_time_string,
    )
  }

  let date_string = string.trim(date_string)
  let offsetted_time_string = string.trim(offsetted_time_string)

  let #(time_string, offset_string) = case
    string.ends_with(offsetted_time_string, "Z") || string.ends_with(
      offsetted_time_string,
      "z",
    )
  {
    True -> #(string.drop_right(offsetted_time_string, 1), "+00:00")
    False ->
      case regex.scan(offset_pattern, offsetted_time_string) {
        [regex.Match(_, [option.Some(time_string), option.Some(offset_string)])] -> #(
          time_string,
          offset_string,
        )
        [] -> {
          let local_offset_in_minutes = ffi_local_offset()
          let assert Ok(local_offset_string) =
            generate_offset(local_offset_in_minutes * 60_000_000)

          #(offsetted_time_string, local_offset_string)
        }
      }
  }

  let time_string = string.replace(time_string, ":", "")
  let #(time_string, milli_seconds_result) = case
    string.split(time_string, ".")
  {
    [time_string] -> #(time_string, Ok(0))
    [time_string, milli_seconds_string] -> #(
      time_string,
      int.parse(milli_seconds_string),
    )
  }

  case milli_seconds_result {
    Ok(milli_seconds) -> {
      use [year, month, day] <- result.then(parse_date(date_string))
      use [hour, minute, second] <- result.then(parse_time(time_string))

      case
        from_parts(
          #(year, month, day),
          #(hour, minute, second, milli_seconds),
          offset_string,
        )
      {
        Ok(DateTime(timestamp, offset, option.None, option.None)) ->
          Ok(DateTime(timestamp, offset, option.None, option.None))

        Error(Nil) -> Error(Nil)
      }
    }

    Error(Nil) -> Error(Nil)
  }
}

/// the naive format is the same as ISO8601 except that it does not contain the offset
pub fn to_naive(value: DateTime) -> String {
  let #(#(year, month, day), #(hour, minute, second, milli_second), _) =
    to_parts(value)

  int.to_string(year) <> "-" <> {
    month
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> "-" <> {
    day
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> "T" <> {
    hour
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    minute
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    second
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> "." <> {
    milli_second
    |> int.to_string
    |> string.pad_left(3, "0")
  }
}

/// the naive format is the same as ISO8601 except that it does not contain the offset
pub fn from_naive(value: String) -> Result(DateTime, Nil) {
  let value = string.trim(value)

  let #(date_string, time_string) = case string.split(value, "T") {
    [date_string] -> #(date_string, "00")
    [date_string, time_string] -> #(date_string, time_string)
  }

  let date_string = string.trim(date_string)
  let time_string = string.trim(time_string)

  let time_string = string.replace(time_string, ":", "")
  let #(time_string, milli_seconds_result) = case
    string.split(time_string, ".")
  {
    [time_string] -> #(time_string, Ok(0))
    [time_string, milli_seconds_string] -> #(
      time_string,
      int.parse(milli_seconds_string),
    )
  }

  case milli_seconds_result {
    Ok(milli_seconds) -> {
      use [year, month, day] <- result.then(parse_date(date_string))
      use [hour, minute, second] <- result.then(parse_time(time_string))

      case
        from_parts(
          #(year, month, day),
          #(hour, minute, second, milli_seconds),
          "Z",
        )
      {
        Ok(DateTime(timestamp, offset, option.None, option.None)) ->
          Ok(DateTime(timestamp, offset, option.None, option.None))

        Error(Nil) -> Error(Nil)
      }
    }

    Error(Nil) -> Error(Nil)
  }
}

/// see [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)
pub fn to_http(value: DateTime) -> String {
  let assert Ok(value) = set_offset(value, "Z")
  let #(#(year, _, day), #(hour, minute, second, _), _) = to_parts(value)
  let short_weekday = short_string_weekday(value)
  let short_month = short_string_month(value)

  short_weekday <> ", " <> {
    day
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> " " <> short_month <> " " <> int.to_string(year) <> " " <> {
    hour
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    minute
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    second
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> " GMT"
}

/// like `to_http` but assumes the offset in the DateTime value instead of `GMT`
pub fn to_http_with_offset(value: DateTime) -> String {
  let #(#(year, _, day), #(hour, minute, second, _), offset) = to_parts(value)
  let short_weekday = short_string_weekday(value)
  let short_month = short_string_month(value)

  let offset = case offset {
    "Z" -> "GMT"
    _ -> offset
  }

  short_weekday <> ", " <> {
    day
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> " " <> short_month <> " " <> int.to_string(year) <> " " <> {
    hour
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    minute
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> ":" <> {
    second
    |> int.to_string
    |> string.pad_left(2, "0")
  } <> " " <> offset
}

/// see [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)
///
/// also supports other similar formats:
///
/// `Tue, 01-Nov-2016 08:49:37 GMT`
///
/// `Tue, 01 Nov 2016 08:49:37 +0630`
///
/// `Tue, 01-November-2016 08:49:37 Z`
///
/// `Tuesday, 01-Nov-2016 08:49:37 +330`
///
/// `Tuesday, 01 November 2016 08:49:37 +06:30`
pub fn from_http(value: String) -> Result(DateTime, Nil) {
  let value = string.trim(value)
  let [weekday, rest] = string.split(value, ",")

  use <- bool.guard(
    !list.any(
      weekday_strings,
      fn(weekday_item) {
        let strings = weekday_item.1
        strings.0 == weekday || strings.1 == weekday
      },
    ),
    Error(Nil),
  )

  let rest = string.trim(rest)
  let assert Ok(whitespace_pattern) = regex.from_string("\\s+")
  case regex.split(whitespace_pattern, rest) {
    [day_string, month_string, year_string, time_string, offset_string] -> {
      let time_string = string.replace(time_string, ":", "")
      case
        #(
          int.parse(day_string),
          month_strings
          |> list.index_map(fn(index, month) {
            let strings = month.1
            #(index, strings.0, strings.1)
          })
          |> list.find(fn(month) {
            month.1 == month_string || month.2 == month_string
          }),
          int.parse(year_string),
          parse_time(time_string),
        )
      {
        #(
          Ok(day),
          Ok(#(month_index, _, _)),
          Ok(year),
          Ok([hour, minute, second]),
        ) ->
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
        _ -> Error(Nil)
      }
    }

    [date_string, time_string, offset_string] ->
      case string.split(date_string, "-") {
        [day_string, month_string, year_string] -> {
          let time_string = string.replace(time_string, ":", "")
          case
            #(
              int.parse(day_string),
              month_strings
              |> list.index_map(fn(index, month) {
                let strings = month.1
                #(index, strings.0, strings.1)
              })
              |> list.find(fn(month) {
                month.1 == month_string || month.2 == month_string
              }),
              int.parse(year_string),
              parse_time(time_string),
            )
          {
            #(
              Ok(day),
              Ok(#(month_index, _, _)),
              Ok(year),
              Ok([hour, minute, second]),
            ) ->
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
            _ -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }

    _ -> Error(Nil)
  }
}

/// unix timestamps are the number of seconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn to_unix(value: DateTime) -> Int {
  case value {
    DateTime(t, _, _, _) -> t / 1_000_000
  }
}

/// unix timestamps are the number of seconds that have elapsed since 00:00:00 UTC on January 1st, 1970
pub fn from_unix(value: Int) -> DateTime {
  DateTime(value * 1_000_000, 0, option.None, option.None)
}

pub fn compare(a: DateTime, b: DateTime) -> order.Order {
  let DateTime(wall_time: wta, offset: _, timezone: _, monotonic_time: mta) = a
  let DateTime(wall_time: wtb, offset: _, timezone: _, monotonic_time: mtb) = b

  let #(ta, tb) = case #(mta, mtb) {
    #(option.Some(ta), option.Some(tb)) -> #(ta, tb)
    _ -> #(wta, wtb)
  }

  case ta == tb {
    True -> order.Eq
    False ->
      case ta < tb {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

pub fn difference(a: DateTime, b: DateTime) -> duration.Duration {
  let DateTime(wall_time: wta, offset: _, timezone: _, monotonic_time: mta) = a
  let DateTime(wall_time: wtb, offset: _, timezone: _, monotonic_time: mtb) = b

  let #(ta, tb) = case #(mta, mtb) {
    #(option.Some(ta), option.Some(tb)) -> #(ta, tb)
    _ -> #(wta, wtb)
  }

  duration.Duration(ta - tb)
}

const units = [
  #(duration.Year, "year"),
  #(duration.Month, "month"),
  #(duration.Week, "week"),
  #(duration.Day, "day"),
  #(duration.Hour, "hour"),
  #(duration.Minute, "minute"),
  #(duration.Second, "second"),
]

pub fn legible_difference(a: DateTime, b: DateTime) -> String {
  case
    difference(a, b)
    |> duration.blur
  {
    #(_, duration.MicroSecond) | #(_, duration.MilliSecond) -> "just now"

    #(amount, unit) -> {
      let assert Ok(unit) = list.key_find(units, unit)
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
          |> int.to_string <> " " <> unit <> " ago"
      }
    }
  }
}

pub fn add(value: DateTime, duration: duration.Duration) -> DateTime {
  let DateTime(wall_time: wt, offset: o, timezone: timezone, monotonic_time: mt) =
    value
  let duration.Duration(duration) = duration
  case mt {
    option.Some(mt) ->
      DateTime(
        wall_time: wt + duration,
        offset: o,
        timezone: timezone,
        monotonic_time: option.Some(mt + duration),
      )
    option.None ->
      DateTime(
        wall_time: wt + duration,
        offset: o,
        timezone: timezone,
        monotonic_time: option.None,
      )
  }
}

pub fn subtract(value: DateTime, duration: duration.Duration) -> DateTime {
  let DateTime(wall_time: wt, offset: o, timezone: timezone, monotonic_time: mt) =
    value
  let duration.Duration(duration) = duration
  case mt {
    option.Some(mt) ->
      DateTime(
        wall_time: wt - duration,
        offset: o,
        timezone: timezone,
        monotonic_time: option.Some(mt - duration),
      )
    option.None ->
      DateTime(
        wall_time: wt - duration,
        offset: o,
        timezone: timezone,
        monotonic_time: option.None,
      )
  }
}

pub fn weekday(value: DateTime) -> Weekday {
  case value {
    DateTime(wall_time: t, offset: o, timezone: _, monotonic_time: _) -> {
      let assert Ok(weekday) = list.at(weekdays, ffi_weekday(t, o))
      weekday
    }
  }
}

pub fn string_weekday(value: DateTime) -> String {
  let weekday = weekday(value)
  let assert Ok(#(weekday, _)) = list.key_find(weekday_strings, weekday)
  weekday
}

pub fn short_string_weekday(value: DateTime) -> String {
  let weekday = weekday(value)
  let assert Ok(#(_, weekday)) = list.key_find(weekday_strings, weekday)
  weekday
}

pub fn month(value: DateTime) -> Month {
  let #(#(_, month, _), _, _) = to_parts(value)
  let assert Ok(month) = list.at(months, month - 1)
  month
}

pub fn string_month(value: DateTime) -> String {
  let month = month(value)
  let assert Ok(#(month, _)) = list.key_find(month_strings, month)
  month
}

pub fn short_string_month(value: DateTime) -> String {
  let month = month(value)
  let assert Ok(#(_, month)) = list.key_find(month_strings, month)
  month
}

/// can be used to create a time range starting from time `a` with step `s`
///
/// if `b` is `option.None` the range will be infinite
pub fn range(
  from a: DateTime,
  to b: option.Option(DateTime),
  step s: duration.Duration,
) -> iterator.Iterator(DateTime) {
  let assert Ok(range) = case b {
    option.Some(b) ->
      ranger.create(
        validate: fn(_) { True },
        negate_step: fn(duration) {
          let duration.Duration(value) = duration
          duration.Duration(-1 * value)
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
  |> ranger.unwrap
}

pub fn set_timezone(
  value: DateTime,
  new_timezone: String,
) -> Result(DateTime, Nil) {
  case
    zones.list
    |> list.key_find(new_timezone)
  {
    Ok(new_offset_number) -> {
      case value {
        DateTime(wall_time: t, offset: _, timezone: _, monotonic_time: mt) ->
          DateTime(
            t,
            new_offset_number * 1_000_000,
            option.Some(new_timezone),
            mt,
          )
          |> Ok
      }
    }

    Error(Nil) -> Error(Nil)
  }
}

pub fn get_timezone(value: DateTime) -> option.Option(String) {
  let DateTime(_, _, timezone, _) = value
  timezone
}

/// use this to change the offset of a given time value.
///
/// Some examples of acceptable offsets:
///
/// `"+330", "03:30", "-8:00","-7", "-0400", "03", "Z"`
pub fn set_offset(value: DateTime, new_offset: String) -> Result(DateTime, Nil) {
  use new_offset_number <- result.then(parse_offset(new_offset))
  case value {
    DateTime(wall_time: t, offset: _, timezone: timezone, monotonic_time: mt) ->
      DateTime(t, new_offset_number, timezone, mt)
      |> Ok
  }
}

pub fn get_offset(value: DateTime) -> String {
  let DateTime(_, offset, _, _) = value
  let assert Ok(offset) = generate_offset(offset)
  offset
}

pub fn set_date(value: DateTime, date: Date) -> DateTime {
  let #(_, time, offset) = to_parts(value)
  let Date(year, month, day) = date
  let assert Ok(new_value) = from_parts(#(year, month, day), time, offset)
  new_value
}

pub fn get_date(value: DateTime) -> Date {
  let #(#(year, month, day), _, _) = to_parts(value)
  Date(year, month, day)
}

pub fn set_time(value: DateTime, time: Time) -> DateTime {
  let #(date, _, offset) = to_parts(value)
  let Time(hour, minute, second, milli_second) = time
  let assert Ok(new_value) =
    from_parts(date, #(hour, minute, second, milli_second), offset)
  new_value
}

pub fn get_time(value: DateTime) -> Time {
  let #(_, #(hour, minute, second, milli_second), _) = to_parts(value)
  Time(hour, minute, second, milli_second)
}

@target(erlang)
/// calculates erlang datetime using the offset in the DateTime value
pub fn to_erlang_datetime(
  value: DateTime,
) -> #(#(Int, Int, Int), #(Int, Int, Int)) {
  let #(date, #(hour, minute, second, _), _) = to_parts(value)
  #(date, #(hour, minute, second))
}

@target(erlang)
/// calculates the universal erlang datetime regardless of the offset in the DateTime value
pub fn to_erlang_universal_datetime(
  value: DateTime,
) -> #(#(Int, Int, Int), #(Int, Int, Int)) {
  let assert Ok(value) = set_offset(value, "Z")
  let #(date, #(hour, minute, second, _), _) = to_parts(value)
  #(date, #(hour, minute, second))
}

@target(erlang)
/// calculates the DateTime value from the erlang datetime using the local offset of the system
pub fn from_erlang_local_datetime(
  erlang_datetime: #(#(Int, Int, Int), #(Int, Int, Int)),
) -> DateTime {
  let #(date, time) = erlang_datetime
  let offset_in_minutes = ffi_local_offset()

  let DateTime(wall_time, _, option.None, option.None) =
    unix_epoch
    |> set_date(Date(date.0, date.1, date.2))
    |> set_time(Time(time.0, time.1, time.2, 0))

  let timezone = local_timezone()

  DateTime(
    wall_time,
    offset_in_minutes * 60_000_000,
    option.map(
      timezone,
      fn(tz) {
        case
          zones.list
          |> list.any(fn(item) { item.0 == tz })
        {
          True -> option.Some(tz)
          False -> option.None
        }
      },
    )
    |> option.flatten,
    option.None,
  )
}

@target(erlang)
/// calculates the DateTime value from the erlang datetime in UTC
pub fn from_erlang_universal_datetime(
  erlang_datetime: #(#(Int, Int, Int), #(Int, Int, Int)),
) -> DateTime {
  let #(date, time) = erlang_datetime
  let assert Ok(new_value) =
    unix_epoch
    |> set_date(Date(date.0, date.1, date.2))
    |> set_time(Time(time.0, time.1, time.2, 0))
    |> set_timezone("Etc/UTC")
  new_value
}

fn to_parts(
  value: DateTime,
) -> #(#(Int, Int, Int), #(Int, Int, Int, Int), String) {
  case value {
    DateTime(wall_time: t, offset: o, timezone: _, monotonic_time: _) -> {
      let #(date, time) = ffi_to_parts(t, o)
      let assert Ok(offset) = generate_offset(o)
      #(date, time, offset)
    }
  }
}

fn from_parts(
  date: #(Int, Int, Int),
  time: #(Int, Int, Int, Int),
  offset: String,
) -> Result(DateTime, Nil) {
  use offset_number <- result.then(parse_offset(offset))
  ffi_from_parts(#(date, time), offset_number)
  |> DateTime(offset_number, option.None, option.None)
  |> Ok
}

fn parse_offset(offset: String) -> Result(Int, Nil) {
  use <- bool.guard(list.contains(["Z", "z"], offset), Ok(0))
  let assert Ok(re) = regex.from_string("([+-])")

  use #(sign, offset) <- result.then(case regex.split(re, offset) {
    ["", "+", offset] -> Ok(#(1, offset))
    ["", "-", offset] -> Ok(#(-1, offset))
    [_] -> Ok(#(1, offset))
    _ -> Error(Nil)
  })

  case string.split(offset, ":") {
    [hour_str, minute_str] -> {
      use hour <- result.then(int.parse(hour_str))
      use minute <- result.then(int.parse(minute_str))
      Ok(sign * { hour * 60 + minute } * 60 * 1_000_000)
    }
    [offset] ->
      case string.length(offset) {
        1 | 2 -> {
          use hour <- result.then(int.parse(offset))
          Ok(sign * hour * 3600 * 1_000_000)
        }
        3 -> {
          let assert Ok(hour_str) = string.first(offset)
          let minute_str = string.slice(offset, 1, 2)
          use hour <- result.then(int.parse(hour_str))
          use minute <- result.then(int.parse(minute_str))
          Ok(sign * { hour * 60 + minute } * 60 * 1_000_000)
        }
        4 -> {
          let hour_str = string.slice(offset, 0, 2)
          let minute_str = string.slice(offset, 2, 2)
          use hour <- result.then(int.parse(hour_str))
          use minute <- result.then(int.parse(minute_str))
          Ok(sign * { hour * 60 + minute } * 60 * 1_000_000)
        }
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn generate_offset(offset: Int) -> Result(String, Nil) {
  use <- bool.guard(offset == 0, Ok("Z"))
  case
    [#(offset, duration.MicroSecond)]
    |> duration.new
    |> duration.decompose
  {
    [#(hour, duration.Hour), #(minute, duration.Minute)] ->
      [
        case hour > 0 {
          True ->
            string.concat([
              "+",
              hour
              |> int.to_string
              |> string.pad_left(2, "0"),
            ])
          False ->
            string.concat([
              "-",
              hour
              |> int.absolute_value
              |> int.to_string
              |> string.pad_left(2, "0"),
            ])
        },
        minute
        |> int.absolute_value
        |> int.to_string
        |> string.pad_left(2, "0"),
      ]
      |> string.join(":")
      |> Ok

    [#(hour, duration.Hour)] ->
      [
        case hour > 0 {
          True ->
            string.concat([
              "+",
              hour
              |> int.to_string
              |> string.pad_left(2, "0"),
            ])
          False ->
            string.concat([
              "-",
              hour
              |> int.absolute_value
              |> int.to_string
              |> string.pad_left(2, "0"),
            ])
        },
        "00",
      ]
      |> string.join(":")
      |> Ok
    _ -> Error(Nil)
  }
}

fn parse_date(date: String) -> Result(List(Int), Nil) {
  let assert Ok(dash_pattern) =
    regex.from_string(
      "(\\d{4})(?:-(1[0-2]|0?[0-9]))?(?:-(3[0-1]|[1-2][0-9]|0?[0-9]))?",
    )

  case regex.scan(dash_pattern, date) {
    [regex.Match(_, [option.Some(major)])] -> [int.parse(major), Ok(1), Ok(1)]

    [regex.Match(_, [option.Some(major), option.Some(middle)])] -> [
      int.parse(major),
      int.parse(middle),
      Ok(1),
    ]

    [
      regex.Match(
        _,
        [option.Some(major), option.Some(middle), option.Some(minor)],
      ),
    ] -> [int.parse(major), int.parse(middle), int.parse(minor)]

    _ ->
      parse_iso_section(
        date,
        "(\\d{4})(1[0-2]|0?[0-9])?(3[0-1]|[1-2][0-9]|0?[0-9])?",
        1,
      )
  }
  |> list.try_map(function.identity)
}

fn parse_time(time: String) -> Result(List(Int), Nil) {
  parse_iso_section(
    time,
    "(2[0-3]|1[0-9]|0?[0-9])([1-5][0-9]|0?[0-9])?([1-5][0-9]|0?[0-9])?",
    0,
  )
  |> list.try_map(function.identity)
}

fn parse_iso_section(
  section: String,
  pattern_string: String,
  default: Int,
) -> List(Result(Int, Nil)) {
  let assert Ok(pattern) = regex.from_string(pattern_string)
  case regex.scan(pattern, section) {
    [regex.Match(_, [option.Some(major)])] -> [
      int.parse(major),
      Ok(default),
      Ok(default),
    ]

    [regex.Match(_, [option.Some(major), option.Some(middle)])] -> [
      int.parse(major),
      int.parse(middle),
      Ok(default),
    ]

    [
      regex.Match(
        _,
        [option.Some(major), option.Some(middle), option.Some(minor)],
      ),
    ] -> [int.parse(major), int.parse(middle), int.parse(minor)]

    _ -> [Error(Nil)]
  }
}

@target(erlang)
const weekdays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

@target(javascript)
const weekdays = [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

const months = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

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

@external(erlang, "birl_ffi", "now")
@external(javascript, "../birl_ffi.mjs", "now")
fn ffi_now() -> Int

@external(erlang, "birl_ffi", "local_offset")
@external(javascript, "../birl_ffi.mjs", "local_offset")
fn ffi_local_offset() -> Int

@external(erlang, "birl_ffi", "monotonic_now")
@external(javascript, "../birl_ffi.mjs", "monotonic_now")
fn ffi_monotonic_now() -> Int

@external(erlang, "birl_ffi", "to_parts")
@external(javascript, "../birl_ffi.mjs", "to_parts")
fn ffi_to_parts(a: Int, b: Int) -> #(#(Int, Int, Int), #(Int, Int, Int, Int))

@external(erlang, "birl_ffi", "from_parts")
@external(javascript, "../birl_ffi.mjs", "from_parts")
fn ffi_from_parts(a: #(#(Int, Int, Int), #(Int, Int, Int, Int)), b: Int) -> Int

@external(erlang, "birl_ffi", "weekday")
@external(javascript, "../birl_ffi.mjs", "weekday")
fn ffi_weekday(a: Int, b: Int) -> Int

@external(erlang, "birl_ffi", "local_timezone")
@external(javascript, "../birl_ffi.mjs", "local_timezone")
pub fn local_timezone() -> option.Option(String)
