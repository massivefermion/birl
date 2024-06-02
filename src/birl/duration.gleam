import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/regex
import gleam/result
import gleam/string

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

pub fn add(a: Duration, b: Duration) -> Duration {
  let Duration(a) = a
  let Duration(b) = b
  Duration(a + b)
}

pub fn subtract(a: Duration, b: Duration) -> Duration {
  let Duration(a) = a
  let Duration(b) = b
  Duration(a - b)
}

pub fn scale_up(value: Duration, factor: Int) -> Duration {
  let Duration(value) = value
  Duration(value * factor)
}

pub fn scale_down(value: Duration, factor: Int) -> Duration {
  let Duration(value) = value
  Duration(value / factor)
}

pub fn micro_seconds(value: Int) -> Duration {
  Duration(value)
}

pub fn milli_seconds(value: Int) -> Duration {
  Duration(value * milli_second)
}

pub fn seconds(value: Int) -> Duration {
  Duration(value * second)
}

pub fn minutes(value: Int) -> Duration {
  Duration(value * minute)
}

pub fn hours(value: Int) -> Duration {
  Duration(value * hour)
}

pub fn days(value: Int) -> Duration {
  Duration(value * day)
}

pub fn weeks(value: Int) -> Duration {
  Duration(value * week)
}

pub fn months(value: Int) -> Duration {
  Duration(value * month)
}

pub fn years(value: Int) -> Duration {
  Duration(value * year)
}

pub fn compare(a: Duration, b: Duration) -> order.Order {
  let Duration(dta) = a
  let Duration(dtb) = b

  case dta == dtb, dta < dtb {
    True, _ -> order.Eq
    _, True -> order.Lt
    _, False -> order.Gt
  }
}

/// use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn new(values: List(#(Int, Unit))) -> Duration {
  values
  |> list.fold(0, fn(total, current) {
    case current {
      #(amount, MicroSecond) -> total + amount
      #(amount, MilliSecond) -> total + amount * milli_second
      #(amount, Second) -> total + amount * second
      #(amount, Minute) -> total + amount * minute
      #(amount, Hour) -> total + amount * hour
      #(amount, Day) -> total + amount * day
      #(amount, Week) -> total + amount * week
      #(amount, Month) -> total + amount * month
      #(amount, Year) -> total + amount * year
    }
  })
  |> Duration
}

/// use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_new(values: List(#(Int, Unit))) -> Duration {
  values
  |> list.fold(0, fn(total, current) {
    case current {
      #(amount, MicroSecond) -> total + amount
      #(amount, MilliSecond) -> total + amount * milli_second
      #(amount, Second) -> total + amount * second
      #(amount, Minute) -> total + amount * minute
      #(amount, Hour) -> total + amount * hour
      #(amount, Day) -> total + amount * day
      #(amount, Week) -> total + amount * week
      #(amount, Month) -> total + amount * accurate_month
      #(amount, Year) -> total + amount * accurate_year
    }
  })
  |> Duration
}

/// use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn decompose(duration: Duration) -> List(#(Int, Unit)) {
  let Duration(value) = duration
  let absolute_value = int.absolute_value(value)
  let #(years, remaining) = extract(absolute_value, year)
  let #(months, remaining) = extract(remaining, month)
  let #(weeks, remaining) = extract(remaining, week)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)
  let #(milli_seconds, remaining) = extract(remaining, milli_second)

  [
    #(years, Year),
    #(months, Month),
    #(weeks, Week),
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

/// use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_decompose(duration: Duration) -> List(#(Int, Unit)) {
  let Duration(value) = duration
  let absolute_value = int.absolute_value(value)
  let #(years, remaining) = extract(absolute_value, accurate_year)
  let #(months, remaining) = extract(remaining, accurate_month)
  let #(weeks, remaining) = extract(remaining, week)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)
  let #(milli_seconds, remaining) = extract(remaining, milli_second)

  [
    #(years, Year),
    #(months, Month),
    #(weeks, Week),
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

/// approximates the duration by only the given unit
/// 
/// if the duration is not an integer multiple of the unit,
/// the remainder will be disgarded if it's less than two thirds of the unit,
/// otherwise a single unit will be added to the multiplier.
/// 
///   - `blur_to(days(16), Month)` ->  `0`
///   - `blur_to(days(20), Month)` ->  `1`
pub fn blur_to(duration: Duration, unit: Unit) -> Int {
  let assert Ok(unit_value) = list.key_find(unit_values, unit)
  let Duration(value) = duration
  let #(unit_counts, remaining) = extract(value, unit_value)
  case remaining >= unit_value * 2 / 3 {
    True -> unit_counts + 1
    False -> unit_counts
  }
}

/// approximates the duration by a value in a single unit
pub fn blur(duration: Duration) -> #(Int, Unit) {
  case decompose(duration) {
    [] -> #(0, MicroSecond)
    decomposed ->
      decomposed
      |> list.reverse
      |> inner_blur
  }
}

const milli_second = 1000

const second = 1_000_000

const minute = 60_000_000

const hour = 3_600_000_000

const day = 86_400_000_000

const week = 604_800_000_000

const month = 2_592_000_000_000

const year = 31_536_000_000_000

const accurate_month = 2_629_746_000_000

const accurate_year = 31_556_952_000_000

const unit_values = [
  #(Year, year), #(Month, month), #(Week, week), #(Day, day), #(Hour, hour),
  #(Minute, minute), #(Second, second), #(MilliSecond, milli_second),
  #(MicroSecond, 1),
]

fn inner_blur(values: List(#(Int, Unit))) -> #(Int, Unit) {
  let assert [second, leading, ..] = values
  let assert Ok(leading_unit) = list.key_find(unit_values, leading.1)
  let assert Ok(second_unit) = list.key_find(unit_values, second.1)

  let leading = case second.0 * second_unit < { leading_unit * 2 / 3 } {
    True -> leading
    False -> #(leading.0 + 1, leading.1)
  }

  case list.drop(values, 2) {
    [] -> leading
    chopped -> inner_blur([leading, ..chopped])
  }
}

const year_units = ["y", "year", "years"]

const month_units = ["mon", "month", "months"]

const week_units = ["w", "week", "weeks"]

const day_units = ["d", "day", "days"]

const hour_units = ["h", "hour", "hours"]

const minute_units = ["m", "min", "minute", "minutes"]

const second_units = ["s", "sec", "secs", "second", "seconds"]

const milli_second_units = [
  "ms", "msec", "msecs", "millisecond", "milliseconds", "milli-second",
  "milli-seconds", "milli_second", "milli_seconds",
]

const units = [
  #(Year, year_units), #(Month, month_units), #(Week, week_units),
  #(Day, day_units), #(Hour, hour_units), #(Minute, minute_units),
  #(Second, second_units), #(MilliSecond, milli_second_units),
]

/// you can use this function to create a new duration using expressions like:
///
///     "accurate: 1 Year - 2days + 152M -1h + 25 years + 25secs"
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
/// numbers with no unit are considered as microseconds.
/// specifying `accurate:` is equivalent to using `accurate_new`.
pub fn parse(expression: String) -> Result(Duration, Nil) {
  let assert Ok(re) = regex.from_string("([+|\\-])?\\s*(\\d+)\\s*(\\w+)?")

  let #(constructor, expression) = case
    string.starts_with(expression, "accurate:")
  {
    True -> {
      let assert [_, expression] = string.split(expression, ":")
      #(accurate_new, expression)
    }
    False -> #(new, expression)
  }

  case
    expression
    |> string.lowercase
    |> regex.scan(re, _)
    |> list.try_map(fn(item) {
      case item {
        regex.Match(_, [sign_option, option.Some(amount_string)]) -> {
          use amount <- result.then(int.parse(amount_string))

          case sign_option {
            option.Some("-") -> Ok(#(-1 * amount, MicroSecond))
            option.None | option.Some("+") -> Ok(#(amount, MicroSecond))
            option.Some(_) -> Error(Nil)
          }
        }

        regex.Match(
          _,
          [sign_option, option.Some(amount_string), option.Some(unit)],
        ) -> {
          use amount <- result.then(int.parse(amount_string))
          use #(unit, _) <- result.then(
            list.find(units, fn(item) { list.contains(item.1, unit) }),
          )

          case sign_option {
            option.Some("-") -> Ok(#(-1 * amount, unit))
            option.None | option.Some("+") -> Ok(#(amount, unit))
            option.Some(_) -> Error(Nil)
          }
        }

        _ -> Error(Nil)
      }
    })
  {
    Ok(values) ->
      values
      |> constructor
      |> Ok

    Error(Nil) -> Error(Nil)
  }
}

fn extract(duration: Int, unit_value: Int) -> #(Int, Int) {
  #(duration / unit_value, duration % unit_value)
}
