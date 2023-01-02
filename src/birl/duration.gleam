import gleam/int
import gleam/list
import gleam/regex
import gleam/string
import gleam/option

pub type Duration {
  Duration(Int)
}

pub type Unit {
  MicroSecond
  MilliSecond
  Second
  Minute
  Hour
  Day
  Week
  Month
  Year
}

const milli_second = 1_000

const second = 1_000_000

const minute = 60_000_000

const hour = 3_600_000_000

const day = 86_400_000_000

const week = 604_800_000_000

const month = 2_592_000_000_000

const year = 31_536_000_000_000

const accurate_month = 2_629_746_000_000

const accurate_year = 31_556_952_000_000

/// Use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn new(values: List(#(Int, Unit))) -> Duration {
  values
  |> list.fold(
    0,
    fn(total, current) {
      let #(amount, unit) = current
      case unit {
        MicroSecond -> total + amount
        MilliSecond -> total + amount * milli_second
        Second -> total + amount * second
        Minute -> total + amount * minute
        Hour -> total + amount * hour
        Day -> total + amount * day
        Week -> total + amount * week
        Month -> total + amount * month
        Year -> total + amount * year
      }
    },
  )
  |> Duration
}

/// Use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_new(values: List(#(Int, Unit))) -> Duration {
  values
  |> list.fold(
    0,
    fn(total, current) {
      let #(amount, unit) = current
      case unit {
        MicroSecond -> total + amount
        MilliSecond -> total + amount * milli_second
        Second -> total + amount * second
        Minute -> total + amount * minute
        Hour -> total + amount * hour
        Day -> total + amount * day
        Week -> total + amount * week
        Month -> total + amount * accurate_month
        Year -> total + amount * accurate_year
      }
    },
  )
  |> Duration
}

/// Use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn decompose(duration: Duration) -> List(#(Int, Unit)) {
  let Duration(value) = duration
  let absolute_value = int.absolute_value(value)
  let #(years, remaining) = extract(absolute_value, year)
  let #(months, remaining) = extract(remaining, month)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)
  let #(milli_seconds, remaining) = extract(remaining, milli_second)

  [
    #(years, Year),
    #(months, Month),
    #(days, Day),
    #(hours, Hour),
    #(minutes, Minute),
    #(seconds, Second),
    #(milli_seconds, MilliSecond),
    #(remaining, MicroSecond),
  ]
  |> list.filter(fn(item) { item.0 > 0 })
  |> list.map(fn(item) {
    case value < 0 {
      True -> #(-1 * item.0, item.1)
      False -> item
    }
  })
}

/// Use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_decompose(duration: Duration) -> List(#(Int, Unit)) {
  let Duration(value) = duration
  let absolute_value = int.absolute_value(value)
  let #(years, remaining) = extract(absolute_value, accurate_year)
  let #(months, remaining) = extract(remaining, accurate_month)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)
  let #(milli_seconds, remaining) = extract(remaining, milli_second)

  [
    #(years, Year),
    #(months, Month),
    #(days, Day),
    #(hours, Hour),
    #(minutes, Minute),
    #(seconds, Second),
    #(milli_seconds, MilliSecond),
    #(remaining, MicroSecond),
  ]
  |> list.filter(fn(item) { item.0 > 0 })
  |> list.map(fn(item) {
    case value < 0 {
      True -> #(-1 * item.0, item.1)
      False -> item
    }
  })
}

const year_units = ["y", "year", "years"]

const month_units = ["mon", "month", "months"]

const week_units = ["w", "week", "weeks"]

const day_units = ["d", "day", "days"]

const hour_units = ["h", "hour", "hours"]

const minute_units = ["m", "min", "minute", "minutes"]

const second_units = ["s", "sec", "secs", "second", "seconds"]

const milli_second_units = [
  "ms", "msec", "msecs", "millisecond", "milliseconds",
]

const units = [
  #(Year, year_units),
  #(Month, month_units),
  #(Week, week_units),
  #(Day, day_units),
  #(Hour, hour_units),
  #(Minute, minute_units),
  #(Second, second_units),
  #(MilliSecond, milli_second_units),
]

const pattern = "(\\d+)\\s*(\\w+)"

/// You can use this function to create a new duration using expressions like:
///
///     "accurate: 1 Year + 2days + 152M + 25 years + 25secs"
///
/// where the units are:
///
///     Year:         y, Y, YEAR, years, Years, ...
///
///     Month:        mon, Month, mONths, ...
///
///     Week:         w, W, Week, weeks, ...
///
///     Day:          d, D, day, Days, ...
///
///     Hour:         h, H, Hour, Hours, ...
///
///     Minute:       m, M, Min, minute, Minutes, ...
///
///     Second:       s, S, sec, Secs, second, Seconds, ...
///
///     MilliSecond:  ms, Msec, mSecs, milliSecond, MilliSecond, ...
///
/// Specifying `accurate:` is equivalent to using `accurate_new`.
pub fn parse(expression: String) -> Result(Duration, Nil) {
  assert Ok(re) = regex.from_string(pattern)

  let #(accurate, expression) = case
    string.starts_with(expression, "accurate:")
  {
    True -> {
      let [_, expression] = string.split(expression, ":")
      #(True, expression)
    }
    False -> #(False, expression)
  }

  case
    expression
    |> string.split("+")
    |> list.map(string.trim)
    |> list.map(string.lowercase)
    |> list.map(regex.scan(re, _))
    |> list.try_map(fn(item) {
      case item {
        [regex.Match(_, [option.Some(amount_string), option.Some(unit)])] -> {
          try amount = int.parse(amount_string)
          try #(unit, _) =
            list.find(units, fn(item) { list.contains(item.1, unit) })
          #(amount, unit)
          |> Ok
        }
        _ -> Error(Nil)
      }
    })
  {
    Ok(values) ->
      case accurate {
        True -> accurate_new(values)
        False -> new(values)
      }
      |> Ok

    Error(Nil) -> Error(Nil)
  }
}

fn extract(duration: Int, unit: Int) -> #(Int, Int) {
  assert Ok(amount) = int.divide(duration, unit)
  assert Ok(remaining) = int.modulo(duration, unit)
  #(amount, remaining)
}
