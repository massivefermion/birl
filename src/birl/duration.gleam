import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/duration as time_duration

/// Duration is now an alias for gleam_time's Duration type.
/// It represents a span of time stored in nanoseconds.
pub type Duration =
  time_duration.Duration

pub type Unit {
  NanoSecond
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
  time_duration.add(a, b)
}

pub fn subtract(a: Duration, b: Duration) -> Duration {
  time_duration.difference(b, a)
}

pub fn scale_up(value: Duration, factor: Int) -> Duration {
  to_nanoseconds(value) * factor
  |> time_duration.nanoseconds
}

pub fn scale_down(value: Duration, factor: Int) -> Duration {
  to_nanoseconds(value) / factor
  |> time_duration.nanoseconds
}

pub fn nano_seconds(value: Int) -> Duration {
  time_duration.nanoseconds(value)
}

pub fn micro_seconds(value: Int) -> Duration {
  time_duration.nanoseconds(value * 1000)
}

pub fn milli_seconds(value: Int) -> Duration {
  time_duration.nanoseconds(value * milli_second)
}

pub fn seconds(value: Int) -> Duration {
  time_duration.seconds(value)
}

pub fn minutes(value: Int) -> Duration {
  time_duration.minutes(value)
}

pub fn hours(value: Int) -> Duration {
  time_duration.hours(value)
}

pub fn days(value: Int) -> Duration {
  time_duration.nanoseconds(value * day)
}

pub fn weeks(value: Int) -> Duration {
  time_duration.nanoseconds(value * week)
}

pub fn months(value: Int) -> Duration {
  time_duration.nanoseconds(value * month)
}

pub fn years(value: Int) -> Duration {
  time_duration.nanoseconds(value * year)
}

pub fn compare(a: Duration, b: Duration) -> order.Order {
  int.compare(to_nanoseconds(a), to_nanoseconds(b))
}

/// Convert duration to total nanoseconds (internal helper)
fn to_nanoseconds(d: Duration) -> Int {
  let #(seconds, nanoseconds) = time_duration.to_seconds_and_nanoseconds(d)
  seconds * 1_000_000_000 + nanoseconds
}

/// use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn new(values: List(#(Int, Unit))) -> Duration {
  new_with_constants(values, month, year)
}

/// use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_new(values: List(#(Int, Unit))) -> Duration {
  new_with_constants(values, accurate_month, accurate_year)
}

fn new_with_constants(
  values: List(#(Int, Unit)),
  month_nanos: Int,
  year_nanos: Int,
) -> Duration {
  values
  |> list.fold(0, fn(total, current) {
    let #(amount, unit) = current
    total + amount * case unit {
      NanoSecond -> 1
      MicroSecond -> 1000
      MilliSecond -> milli_second
      Second -> second
      Minute -> minute
      Hour -> hour
      Day -> day
      Week -> week
      Month -> month_nanos
      Year -> year_nanos
    }
  })
  |> time_duration.nanoseconds
}

/// use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn decompose(duration: Duration) -> List(#(Int, Unit)) {
  decompose_with_constants(duration, month, year)
}

/// use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_decompose(duration: Duration) -> List(#(Int, Unit)) {
  decompose_with_constants(duration, accurate_month, accurate_year)
}

fn decompose_with_constants(
  duration: Duration,
  month_nanos: Int,
  year_nanos: Int,
) -> List(#(Int, Unit)) {
  let value = to_nanoseconds(duration)
  let absolute_value = int.absolute_value(value)
  let #(years, remaining) = extract(absolute_value, year_nanos)
  let #(months, remaining) = extract(remaining, month_nanos)
  let #(weeks, remaining) = extract(remaining, week)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)
  let #(milli_seconds, remaining) = extract(remaining, milli_second)
  let #(micro_seconds, remaining) = extract(remaining, 1000)

  [
    #(years, Year),
    #(months, Month),
    #(weeks, Week),
    #(days, Day),
    #(hours, Hour),
    #(minutes, Minute),
    #(seconds, Second),
    #(milli_seconds, MilliSecond),
    #(micro_seconds, MicroSecond),
    #(remaining, NanoSecond),
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
  let unit_value = unit_values(unit)
  let value = to_nanoseconds(duration)
  let #(unit_counts, remaining) = extract(value, unit_value)
  case remaining >= unit_value * 2 / 3 {
    True -> unit_counts + 1
    False -> unit_counts
  }
}

/// approximates the duration by a value in a single unit
pub fn blur(duration: Duration) -> #(Int, Unit) {
  decompose(duration)
  |> list.reverse
  |> inner_blur
}

// All constants are now in nanoseconds
const milli_second = 1_000_000

const second = 1_000_000_000

const minute = 60_000_000_000

const hour = 3_600_000_000_000

const day = 86_400_000_000_000

const week = 604_800_000_000_000

const month = 2_592_000_000_000_000

const year = 31_536_000_000_000_000

const accurate_month = 2_629_746_000_000_000

const accurate_year = 31_556_952_000_000_000

fn unit_values(unit: Unit) {
  case unit {
    Year -> year
    Month -> month
    Week -> week
    Day -> day
    Hour -> hour
    Minute -> minute
    Second -> second
    MilliSecond -> milli_second
    MicroSecond -> 1000
    NanoSecond -> 1
  }
}

fn inner_blur(values: List(#(Int, Unit))) -> #(Int, Unit) {
  case values {
    [] -> #(0, NanoSecond)
    [single] -> single
    [smaller, larger, ..rest] -> {
      let smaller_unit_value = unit_values(smaller.1)
      let larger_unit_value = unit_values(larger.1)

      let at_least_two_thirds =
        smaller.0 * smaller_unit_value < { larger_unit_value * 2 / 3 }

      let rounded = case at_least_two_thirds {
        True -> larger
        False -> #(larger.0 + 1, larger.1)
      }

      inner_blur([rounded, ..rest])
    }
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
  #(Year, year_units),
  #(Month, month_units),
  #(Week, week_units),
  #(Day, day_units),
  #(Hour, hour_units),
  #(Minute, minute_units),
  #(Second, second_units),
  #(MilliSecond, milli_second_units),
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
/// numbers with no unit are considered as nanoseconds.
/// specifying `accurate:` is equivalent to using `accurate_new`.
pub fn parse(expression: String) -> Result(Duration, Nil) {
  let assert Ok(re) = regexp.from_string("([+|\\-])?\\s*(\\d+)\\s*(\\w+)?")

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
    |> regexp.scan(re, _)
    |> list.try_map(fn(item) {
      case item {
        regexp.Match(_, [sign_option, option.Some(amount_string)]) -> {
          use amount <- result.try(int.parse(amount_string))

          case sign_option {
            option.Some("-") -> Ok(#(-1 * amount, NanoSecond))
            option.None | option.Some("+") -> Ok(#(amount, NanoSecond))
            option.Some(_) -> Error(Nil)
          }
        }

        regexp.Match(
          _,
          [sign_option, option.Some(amount_string), option.Some(unit)],
        ) -> {
          use amount <- result.try(int.parse(amount_string))
          use #(unit, _) <- result.try(
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

// ---------------------------------------------------------------------------
// gleam_time interoperability (deprecated — Duration is now gleam_time Duration)
// ---------------------------------------------------------------------------

/// Convert birl Duration to gleam_time Duration.
///
/// Deprecated: Duration is now the same type as gleam_time Duration.
/// This function is kept for backward compatibility and is a no-op.
@deprecated("Duration is now gleam_time's Duration type. This function is a no-op.")
pub fn to_gleam_duration(d: Duration) -> time_duration.Duration {
  d
}

/// Convert gleam_time Duration to birl Duration.
///
/// Deprecated: Duration is now the same type as gleam_time Duration.
/// This function is kept for backward compatibility and is a no-op.
@deprecated("Duration is now gleam_time's Duration type. This function is a no-op.")
pub fn from_gleam_duration(d: time_duration.Duration) -> Duration {
  d
}
