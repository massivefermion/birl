-module(birl_ffi).

-export([now/0, to_parts/1, to_iso/1, from_iso/1]).

-define(DaysInMonths, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).

now() -> os:system_time(millisecond).

to_parts(Timestmap) ->
    calendar:system_time_to_universal_time(Timestmap, millisecond).

to_iso(Timestmap) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = to_parts(Timestmap),
    MilliSec = Timestmap rem 1000,
    iolist_to_binary(
        io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
            [Year, Month, Day, Hour, Min, Sec, MilliSec]
        )
    ).

from_iso(ISODate) ->
    [Date, Time] = string:split(ISODate, "T"),
    [YearStr, MonthStr, DayStr] = string:split(Date, "-", all),
    [HoursStr, MinutesStr, Rest] = string:split(Time, ":", all),
    [SecondsStr, MilliSecondsStr] = string:split(Rest, "."),
    Year = binary_to_integer(YearStr),
    Month = binary_to_integer(MonthStr),
    Day = binary_to_integer(DayStr),
    Hours = binary_to_integer(HoursStr),
    Minutes = binary_to_integer(MinutesStr),
    Seconds = binary_to_integer(SecondsStr),
    MilliSeconds = binary_to_integer(string:slice(MilliSecondsStr, 0, 3)),
    DaysInYears = calculate_days_from_year(Year - 1, 0),
    DaysInMonths = calculate_days_from_month(Year, Month - 1, 0),
    Days = DaysInYears + DaysInMonths + Day - 1,
    (((Days * 3600 * 24) + (Hours * 3600) + (Minutes * 60) + Seconds) * 1000) + MilliSeconds.

calculate_days_from_year(1969, Days) ->
    Days;
calculate_days_from_year(Year, Days) ->
    case calendar:is_leap_year(Year) of
        true ->
            calculate_days_from_year(Year - 1, Days + 366);
        false ->
            calculate_days_from_year(Year - 1, Days + 365)
    end.

calculate_days_from_month(_, 0, Days) ->
    Days;
calculate_days_from_month(Year, Month, Days) ->
    case calendar:is_leap_year(Year) and (Month == 2) of
        true ->
            calculate_days_from_month(Year, 1, Days + 29);
        false ->
            calculate_days_from_month(Year, Month - 1, Days + lists:nth(Month, ?DaysInMonths))
    end.
