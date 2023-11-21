![birl](https://raw.githubusercontent.com/massivefermion/birl/main/banner.png)

[![Package Version](https://img.shields.io/hexpm/v/birl)](https://hex.pm/packages/birl)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/birl/)

# birl

Date/Time handling for gleam

## <img width=32 src="https://raw.githubusercontent.com/massivefermion/birl/main/icon.png"> Quick start

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## <img width=32 src="https://raw.githubusercontent.com/massivefermion/birl/main/icon.png"> Installation

This package can be added to your Gleam project:

```sh
gleam add birl
```

and its documentation can be found at <https://hexdocs.pm/birl>.

## <img width=32 src="https://raw.githubusercontent.com/massivefermion/birl/main/icon.png"> Usage

```gleam
import birl
import birl/duration

pub fn main() {
    let now = birl.now()
    let two_weeks_later = birl.add(now, duration.weeks(2))
    birl.to_iso8601(two_weeks_later)
}
```