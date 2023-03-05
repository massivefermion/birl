import gleam/int
import gleam/list
import gleam/order
import gleam/regex
import gleam/result
import gleam/string
import gleam/option
import birl/duration

pub opaque type Time {
  Time(wall_time: Int, offset: Int, monotonic_time: option.Option(Int))
}

/// you can use the add function and this constant to create a time value given a unix timestamp
pub const unix_time_origin = Time(0, 0, option.None)

pub type WeekDay {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

/// use this to get the current time in the local timezone offset
pub fn now() {
  let now = ffi_now()
  let offset_in_minutes = ffi_local_offset()
  let monotonic_now = ffi_monotonic_now()
  Time(now, offset_in_minutes * 60_000_000, option.Some(monotonic_now))
}

/// use this to get the current time in utc
pub fn utc_now() {
  let now = ffi_now()
  let monotonic_now = ffi_monotonic_now()
  Time(now, 0, option.Some(monotonic_now))
}

/// use this the get the current time with a given offset.
///
/// Some examples of acceptable offsets:
///
/// `"+330", "03:30", "-8:00","-7", "-0400", "03"`
pub fn now_with_offset(offset: String) -> Result(Time, Nil) {
  use offset <- result.then(parse_offset(offset))
  let now = ffi_now()
  let monotonic_now = ffi_monotonic_now()
  Time(now, offset, option.Some(monotonic_now))
  |> Ok
}

/// Some examples of acceptable offsets:
///
/// `"+330", "03:30", "-8:00","-7", "-0400", "03"`
pub fn change_offset(value: Time, new_offset: String) -> Result(Time, Nil) {
  use new_offset_number <- result.then(parse_offset(new_offset))
  case value {
    Time(wall_time: t, offset: _, monotonic_time: mt) ->
      Time(t, new_offset_number, mt)
      |> Ok
  }
}

pub fn to_parts(
  value: Time,
) -> #(#(Int, Int, Int), #(Int, Int, Int, Int), String) {
  case value {
    Time(wall_time: t, offset: o, monotonic_time: _) -> {
      let #(date, time) = ffi_to_parts(t + o)
      let assert Ok(offset) = generate_offset(o)
      #(date, time, offset)
    }
  }
}

pub fn from_parts(
  date: #(Int, Int, Int),
  time: #(Int, Int, Int, Int),
  offset offset: String,
) -> Result(Time, Nil) {
  use offset_number <- result.then(parse_offset(offset))
  ffi_from_parts(#(date, time), offset_number)
  |> Time(offset_number, option.None)
  |> Ok
}

pub fn to_iso8601(value: Time) -> String {
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

pub fn from_iso8601(value: String) -> Result(Time, Nil) {
  let assert Ok(offset_pattern) = regex.from_string("(.*)([+|\\-].*)")
  let value = string.trim(value)

  let #(date_string, offsetted_time_string) = case string.split(value, "T") {
    [date_string] -> #(date_string, "00")
    [date_string, offsetted_time_string] -> #(
      date_string,
      offsetted_time_string,
    )
  }

  let #(time_string, offset_string) = case
    string.ends_with(offsetted_time_string, "Z")
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

  let date_string = string.replace(date_string, "-", "")
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
      use [year, month, day] <- result.then(parse_iso_section(
        date_string,
        "(\\d{4})(\\d{2})?(\\d{2})?",
        1,
      ))

      use [hour, minute, second] <- result.then(parse_iso_section(
        time_string,
        "(\\d{2})(\\d{2})?(\\d{2})?",
        0,
      ))

      case
        from_parts(
          #(year, month, day),
          #(hour, minute, second, milli_seconds),
          offset_string,
        )
      {
        Ok(Time(timestamp, offset, option.None)) ->
          Ok(Time(timestamp, offset, option.None))

        Error(Nil) -> Error(Nil)
      }
    }

    Error(Nil) -> Error(Nil)
  }
}

pub fn compare(a: Time, b: Time) -> order.Order {
  let Time(wall_time: wta, offset: oa, monotonic_time: mta) = a
  let Time(wall_time: wtb, offset: ob, monotonic_time: mtb) = b

  let #(ta, tb) = case #(mta, mtb) {
    #(option.Some(ta), option.Some(tb)) -> #(ta, tb)
    _ -> #(wta + oa, wtb + ob)
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

pub fn difference(a: Time, b: Time) -> duration.Duration {
  let Time(wall_time: wta, offset: oa, monotonic_time: mta) = a
  let Time(wall_time: wtb, offset: ob, monotonic_time: mtb) = b

  let #(ta, tb) = case #(mta, mtb) {
    #(option.Some(ta), option.Some(tb)) -> #(ta, tb)
    _ -> #(wta + oa, wtb + ob)
  }

  duration.Duration(ta - tb)
}

pub fn add(value: Time, duration: duration.Duration) -> Time {
  let Time(wall_time: wt, offset: o, monotonic_time: mt) = value
  let duration.Duration(duration) = duration
  case mt {
    option.Some(mt) ->
      Time(
        wall_time: wt + duration,
        offset: o,
        monotonic_time: option.Some(mt + duration),
      )
    option.None ->
      Time(wall_time: wt + duration, offset: o, monotonic_time: option.None)
  }
}

pub fn subtract(value: Time, duration: duration.Duration) -> Time {
  let Time(wall_time: wt, offset: o, monotonic_time: mt) = value
  let duration.Duration(duration) = duration
  case mt {
    option.Some(mt) ->
      Time(
        wall_time: wt - duration,
        offset: o,
        monotonic_time: option.Some(mt - duration),
      )
    option.None ->
      Time(wall_time: wt - duration, offset: o, monotonic_time: option.None)
  }
}

pub fn weekday(value: Time) -> WeekDay {
  case value {
    Time(wall_time: t, offset: o, monotonic_time: _) -> {
      let assert Ok(weekday) = list.at(weekdays, ffi_weekday(t + o))
      weekday
    }
  }
}

fn parse_offset(offset: String) -> Result(Int, Nil) {
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
  case offset {
    0 -> Ok("Z")
    _ ->
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
}

fn parse_iso_section(
  section: String,
  pattern_string: String,
  default: Int,
) -> Result(List(Int), Nil) {
  let assert Ok(pattern) = regex.from_string(pattern_string)
  case regex.scan(pattern, section) {
    [regex.Match(_, [option.Some(major)])]
    | [regex.Match(_, [option.Some(major), option.None])] -> [
      int.parse(major),
      Ok(default),
      Ok(default),
    ]

    [regex.Match(_, [option.Some(major), option.Some(middle)])]
    | [regex.Match(_, [option.Some(major), option.Some(middle), option.None])] -> [
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
  |> list.try_map(fn(part) {
    case part {
      Ok(inner) -> Ok(inner)
      Error(Nil) -> Error(Nil)
    }
  })
}

if erlang {
  const weekdays = [
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
  ]
}

if javascript {
  const weekdays = [
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
  ]
}

if erlang {
  external fn ffi_now() -> Int =
    "birl_ffi" "now"

  external fn ffi_local_offset() -> Int =
    "birl_ffi" "local_offset"

  external fn ffi_monotonic_now() -> Int =
    "birl_ffi" "monotonic_now"

  external fn ffi_to_parts(Int) -> #(#(Int, Int, Int), #(Int, Int, Int, Int)) =
    "birl_ffi" "to_parts"

  external fn ffi_from_parts(
    #(#(Int, Int, Int), #(Int, Int, Int, Int)),
    Int,
  ) -> Int =
    "birl_ffi" "from_parts"

  external fn ffi_weekday(Int) -> Int =
    "birl_ffi" "weekday"
}

if javascript {
  external fn ffi_now() -> Int =
    "../birl_ffi.mjs" "now"

  external fn ffi_local_offset() -> Int =
    "../birl_ffi.mjs" "local_offset"

  external fn ffi_monotonic_now() -> Int =
    "../birl_ffi.mjs" "monotonic_now"

  external fn ffi_to_parts(Int) -> #(#(Int, Int, Int), #(Int, Int, Int, Int)) =
    "../birl_ffi.mjs" "to_parts"

  external fn ffi_from_parts(
    #(#(Int, Int, Int), #(Int, Int, Int, Int)),
    Int,
  ) -> Int =
    "../birl_ffi.mjs" "from_parts"

  external fn ffi_weekday(Int) -> Int =
    "../birl_ffi.mjs" "weekday"
}
