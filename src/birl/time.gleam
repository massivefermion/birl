import gleam/io
import gleam/int
import gleam/list
import gleam/order
import gleam/regex
import gleam/string
import gleam/option
import gleam/string_builder
import birl/duration

pub opaque type Time {
  Time(wall_time: Int, offset: Int, monotonic_time: option.Option(Int))
}

pub type WeekDay {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

/// use this to get the current time in utc
pub fn now() {
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
  try offset = parse_offset(offset)
  let now = ffi_now()
  let monotonic_now = ffi_monotonic_now()
  Time(now, offset, option.Some(monotonic_now))
  |> Ok
}

pub fn to_parts(value: Time) -> #(#(Int, Int, Int), #(Int, Int, Int), String) {
  case value {
    Time(wall_time: t, offset: o, monotonic_time: _) -> {
      let #(date, time) = ffi_to_parts(t)
      assert Ok(offset) = generate_offset(o)
      #(date, time, offset)
    }
  }
}

pub fn from_parts(
  date: #(Int, Int, Int),
  time: #(Int, Int, Int),
  offset: String,
) -> Result(Time, Nil) {
  io.println(offset)

  try offset_number = parse_offset(offset)
  io.debug(offset_number)
  try offset = generate_offset(offset_number)

  io.println(offset)

  let string_date = case date {
    #(year, month, day) ->
      [int.to_string(year), int.to_string(month), int.to_string(day)]
      |> list.map(string_builder.from_string)
      |> string_builder.join("-")
  }

  io.debug(string_date)

  let string_time = case time {
    #(hour, minute, second) ->
      [int.to_string(hour), int.to_string(minute), int.to_string(second)]
      |> list.map(string_builder.from_string)
      |> string_builder.join(":")
      |> string_builder.append(".000")
      |> string_builder.append(offset)
  }

  io.debug(string_time)

  string_builder.join([string_date, string_time], "T")
  |> string_builder.to_string
  |> io.debug
  |> from_iso
  |> io.debug
}

pub fn to_iso(value: Time) -> String {
  case value {
    Time(wall_time: t, offset: o, monotonic_time: _) -> ffi_to_iso(t + o)
  }
}

pub fn from_iso(value: String) -> Result(Time, Nil) {
  assert Ok(pattern) =
    regex.from_string(
      "\\d{4}-\\d{1,2}-\\d{1,2}T\\d{1,2}:\\d{1,2}:\\d{1,2}.\\d{3}\\+|\\-\\d{2}:\\d{2}",
    )
  io.println(value)
  case regex.check(pattern, value) {
    True ->
      value
      |> ffi_from_iso
      |> Time(0, option.None)
      |> Ok
    False -> Error(Nil)
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

pub fn get_weekday(value: Time) -> WeekDay {
  case value {
    Time(wall_time: t, offset: o, monotonic_time: _) -> {
      assert Ok(weekday) = list.at(weekdays, ffi_get_weekday(t + o))
      weekday
    }
  }
}

fn parse_offset(offset: String) -> Result(Int, Nil) {
  assert Ok(re) = regex.from_string("[+-]")
  try #(sign, offset) = case regex.split(re, offset) {
    ["", "+", offset] -> Ok(#(1, offset))
    ["", "-", offset] -> Ok(#(-1, offset))
    [_] -> Ok(#(1, offset))
    _ -> Error(Nil)
  }

  case string.split(offset, ":") {
    [hour_str, minute_str] -> {
      try hour = int.parse(hour_str)
      try minute = int.parse(minute_str)
      io.println("@@@@@@@@@@@@@@@1")
      Ok(sign * { hour * 60 + minute } * 60 * 1_000_000)
    }
    [offset] ->
      case string.length(offset) {
        1 | 2 -> {
          io.println("@@@@@@@@@@@@@@@2")
          try hour = int.parse(offset)
          Ok(sign * hour * 3600 * 1_000_000)
        }
        3 -> {
          io.println("@@@@@@@@@@@@@@@3")
          assert Ok(hour_str) = string.first(offset)
          let minute_str = string.slice(offset, 1, 2)
          try hour = int.parse(hour_str)
          try minute = int.parse(minute_str)
          io.debug(hour)
          io.debug(minute)
          io.debug(sign)
          Ok(sign * { hour * 60 + minute } * 60 * 1_000_000)
        }
        4 -> {
          io.println("@@@@@@@@@@@@@@@4")
          let hour_str = string.slice(offset, 0, 2)
          let minute_str = string.slice(offset, 2, 2)
          try hour = int.parse(hour_str)
          try minute = int.parse(minute_str)
          Ok(sign * { hour * 60 + minute } * 60 * 1_000_000)
        }
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

pub fn generate_offset(offset: Int) -> Result(String, Nil) {
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
    parts -> {
      io.println("@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      io.debug(parts)
      io.println("@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      Error(Nil)
    }
  }
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

  external fn ffi_monotonic_now() -> Int =
    "birl_ffi" "monotonic_now"

  external fn ffi_to_parts(Int) -> #(#(Int, Int, Int), #(Int, Int, Int)) =
    "birl_ffi" "to_parts"

  external fn ffi_to_iso(Int) -> String =
    "birl_ffi" "to_iso"

  external fn ffi_from_iso(String) -> Int =
    "birl_ffi" "from_iso"

  external fn ffi_get_weekday(Int) -> Int =
    "birl_ffi" "get_weekday"
}

if javascript {
  external fn ffi_now() -> Int =
    "../birl_ffi.mjs" "now"

  external fn ffi_monotonic_now() -> Int =
    "../birl_ffi.mjs" "monotonic_now"

  external fn ffi_to_parts(Int) -> #(#(Int, Int, Int), #(Int, Int, Int)) =
    "../birl_ffi.mjs" "to_parts"

  external fn ffi_to_iso(Int) -> String =
    "../birl_ffi.mjs" "to_iso"

  external fn ffi_from_iso(String) -> Int =
    "../birl_ffi.mjs" "from_iso"

  external fn ffi_get_weekday(Int) -> Int =
    "../birl_ffi.mjs" "get_weekday"
}
