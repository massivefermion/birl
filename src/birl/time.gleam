import gleam/int
import gleam/list
import gleam/order
import gleam/regex
import gleam/option
import gleam/string_builder
import birl/duration

pub opaque type Time {
  Time(wall_time: Int, monotonic_time: option.Option(Int))
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

pub fn now() {
  let now = ffi_now()
  let monotonic_now = ffi_monotonic_now()
  Time(now, option.Some(monotonic_now))
}

pub fn to_parts(value: Time) -> #(#(Int, Int, Int), #(Int, Int, Int)) {
  case value {
    Time(wall_time: t, monotonic_time: _) -> ffi_to_parts(t)
  }
}

pub fn from_parts(date: #(Int, Int, Int), time: #(Int, Int, Int)) -> Time {
  let string_date = case date {
    #(year, month, day) ->
      [int.to_string(year), int.to_string(month), int.to_string(day)]
      |> list.map(string_builder.from_string)
      |> string_builder.join("-")
  }

  let string_time = case time {
    #(hour, minute, second) ->
      [int.to_string(hour), int.to_string(minute), int.to_string(second)]
      |> list.map(string_builder.from_string)
      |> string_builder.join(":")
      |> string_builder.append(".000Z")
  }

  assert Ok(value) =
    string_builder.join([string_date, string_time], "T")
    |> string_builder.to_string
    |> from_iso

  value
}

pub fn to_iso(value: Time) -> String {
  case value {
    Time(wall_time: t, monotonic_time: _) -> ffi_to_iso(t)
  }
}

const pattern = "\\d{4}-\\d{1,2}-\\d{1,2}T\\d{1,2}:\\d{1,2}:\\d{1,2}.\\d{3}Z"

pub fn from_iso(value: String) -> Result(Time, Nil) {
  assert Ok(pattern) = regex.from_string(pattern)
  case regex.check(pattern, value) {
    True ->
      value
      |> ffi_from_iso
      |> Time(option.None)
      |> Ok
    False -> Error(Nil)
  }
}

pub fn compare(a: Time, b: Time) -> order.Order {
  let Time(wall_time: wta, monotonic_time: mta) = a
  let Time(wall_time: wtb, monotonic_time: mtb) = b

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

pub fn difference(a: Time, b: Time) -> duration.Duration {
  let Time(wall_time: wta, monotonic_time: mta) = a
  let Time(wall_time: wtb, monotonic_time: mtb) = b

  let #(ta, tb) = case #(mta, mtb) {
    #(option.Some(ta), option.Some(tb)) -> #(ta, tb)
    _ -> #(wta, wtb)
  }

  duration.Duration(ta - tb)
}

pub fn add(value: Time, duration: duration.Duration) -> Time {
  let Time(wall_time: wt, monotonic_time: mt) = value
  let duration.Duration(duration) = duration
  case mt {
    option.Some(mt) ->
      Time(wall_time: wt + duration, monotonic_time: option.Some(mt + duration))
    option.None -> Time(wall_time: wt + duration, monotonic_time: option.None)
  }
}

pub fn subtract(value: Time, duration: duration.Duration) -> Time {
  let Time(wall_time: wt, monotonic_time: mt) = value
  let duration.Duration(duration) = duration
  case mt {
    option.Some(mt) ->
      Time(wall_time: wt - duration, monotonic_time: option.Some(mt - duration))
    option.None -> Time(wall_time: wt - duration, monotonic_time: option.None)
  }
}

pub fn get_weekday(value: Time) -> WeekDay {
  case value {
    Time(wall_time: t, monotonic_time: _) -> {
      assert Ok(weekday) = list.at(weekdays, ffi_get_weekday(t))
      weekday
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
