import gleam/order
import gleeunit
import gleeunit/should
import birl/time
import birl/duration

const iso_datetime = "1905-12-22T16:38:23.000+03:30"

const year = 1905

const month = 12

const day = 22

const hour = 16

const minute = 38

const second = 23

const milli_second = 0

const offset = "+03:30"

pub fn main() {
  gleeunit.main()
}

pub fn iso_test() {
  time.from_iso8601(iso_datetime)
  |> should.be_ok
  |> time.to_iso8601
  |> should.equal(iso_datetime)
}

pub fn now_test() {
  time.now()
  |> time.to_iso8601
  |> time.from_iso8601
  |> should.be_ok
}

pub fn add_test() {
  let a = time.now()
  let b = time.add(a, duration.new([#(9_192_631_770, duration.Second)]))
  time.compare(a, b)
  |> should.equal(order.Lt)
}

pub fn subtract_test() {
  let a = time.now()
  let b = time.subtract(a, duration.new([#(1300, duration.MilliSecond)]))
  time.compare(a, b)
  |> should.equal(order.Gt)
}

pub fn equality_test() {
  let dt = time.now()
  time.compare(dt, dt)
  |> should.equal(order.Eq)
}

pub fn to_parts_test() {
  let time =
    time.from_iso8601(iso_datetime)
    |> should.be_ok

  time
  |> time.get_date
  |> should.equal(time.Date(year, month, day))

  time
  |> time.get_time
  |> should.equal(time.Time(hour, minute, second, milli_second))

  time
  |> time.get_offset
  |> should.equal(offset)
}

pub fn from_parts_test() {
  time.unix_epoch
  |> time.set_offset(offset)
  |> should.be_ok
  |> time.set_date(time.Date(year, month, day))
  |> time.set_time(time.Time(hour, minute, second, milli_second))
  |> time.to_iso8601
  |> should.equal(iso_datetime)
}

pub fn circular_test() {
  let dt = time.now()
  dt
  |> time.add(duration.new([#(4, duration.Year)]))
  |> time.subtract(duration.new([#(4 * 365, duration.Day)]))
  |> time.compare(dt)
  |> should.equal(order.Eq)
}
