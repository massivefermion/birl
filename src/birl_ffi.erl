-module(birl_ffi).

-export([
    now/0,
    local_offset/0,
    monotonic_now/0,
    to_parts/2,
    from_parts/2,
    weekday/2
]).

-define(DaysInMonths, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).

now() -> os:system_time(microsecond).

local_offset() ->
    Timestamp = erlang:timestamp(),
    {{_, _, LD}, {LH, LM, _}} = calendar:now_to_local_time(Timestamp),
    {{_, _, UD}, {UH, UM, _}} = calendar:now_to_universal_time(Timestamp),
    if
        LD - UD == 0 -> (LH - UH) * 60 + LM - UM;
        true -> (LD - UD) * ((23 - UH) * 60 + (60 - UM) + LH * 60 + LM)
    end.

monotonic_now() ->
    StartTime = erlang:system_info(start_time),
    CurrentTime = erlang:monotonic_time(),
    (CurrentTime - StartTime) div 1_000.

to_parts(Timestamp, Offset) ->
    {Date, {Hour, Minute, Second}} = calendar:system_time_to_universal_time(
        Timestamp + Offset, microsecond
    ),
    MilliSecond = (Timestamp rem 1_000_000) div 1_000,
    {Date,
        {Hour, Minute, Second,
            if
                MilliSecond == 0 -> MilliSecond;
                Timestamp >= 0 -> MilliSecond;
                true -> 1000 + MilliSecond
            end}}.

from_parts(Parts, Offset) ->
    {{Year, Month, Day}, {Hour, Minute, Second, MilliSecond}} = Parts,
    DaysInYears = calculate_days_from_year(Year - 1, 0),
    DaysInMonths = calculate_days_from_month(Year, Month - 1, 0),
    Days =
        if
            DaysInYears >= 0 -> DaysInYears + DaysInMonths + Day - 1;
            true -> DaysInYears + DaysInMonths + Day
        end,
    Seconds = (Days * 3600 * 24) + (Hour * 3600) + (Minute * 60) + Second,
    Seconds * 1_000_000 + MilliSecond * 1_000 - Offset.

weekday(Timestamp, Offset) ->
    {Date, _} = to_parts(Timestamp, Offset),
    calendar:day_of_the_week(Date) - 1.

calculate_days_from_year(1969, Days) ->
    Days;
calculate_days_from_year(Year, Days) when Year > 1969 ->
    case calendar:is_leap_year(Year) of
        true ->
            calculate_days_from_year(Year - 1, Days + 366);
        false ->
            calculate_days_from_year(Year - 1, Days + 365)
    end;
calculate_days_from_year(Year, Days) when Year < 1969 ->
    case calendar:is_leap_year(Year) of
        true ->
            calculate_days_from_year(Year + 1, Days - 366);
        false ->
            calculate_days_from_year(Year + 1, Days - 365)
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
