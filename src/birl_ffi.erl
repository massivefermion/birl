-module(birl_ffi).

-export([
    now/0,
    local_offset/0,
    monotonic_now/0,
    to_parts/2,
    from_parts/2,
    weekday/2,
    local_timezone/0
]).

-define(DaysInMonths, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).
-define(Regions, [
    "Africa",
    "America",
    "Antarctica",
    "Arctic",
    "Asia",
    "Atlantic",
    "Australia",
    "Brazil",
    "Canada",
    "Chile",
    "Etc",
    "Europe",
    "Indian",
    "Mexico",
    "Pacific",
    "US"
]).

now() -> os:system_time(microsecond).

local_offset() ->
    Timestamp = erlang:timestamp(),
    {{_, LMo, LD}, {LH, LM, _}} = calendar:now_to_local_time(Timestamp),
    {{_, UMo, UD}, {UH, UM, _}} = calendar:now_to_universal_time(Timestamp),
    if
        LD == UD ->
            (LH - UH) * 60 + LM - UM;
        (LD > UD andalso LMo == UMo) or (LD == 1 andalso LMo > UMo) ->
            (23 - UH) * 60 + (60 - UM) + LH * 60 + LM;
        true ->
            -((23 - LH) * 60 + (60 - LM) + UH * 60 + UM)
    end.

local_timezone() ->
    case os:type() of
        {unix, _} ->
            case os:getenv("TZ") of
                ":" ++ Path ->
                    case timezone_from_path(Path) of
                        <<>> -> local_timezone_from_etc();
                        TZ -> {some, TZ}
                    end;
                false ->
                    local_timezone_from_etc();
                TZOrPath ->
                    case timezone_from_path(TZOrPath) of
                        <<>> -> local_timezone_from_etc();
                        TZ -> {some, TZ}
                    end
            end;
        {win32, _} ->
            {ok, RegHandle} = win32reg:open([read]),
            win32reg:change_key(
                RegHandle,
                "\\local_machine\\SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation"
            ),
            {ok, Values} = win32reg:values(RegHandle),
            case lists:keyfind("TimeZoneKeyName", 1, Values) of
                {"TimeZoneKeyName", WinZone} -> win_to_iana(string:trim(WinZone));
                false -> none
            end
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

local_timezone_from_etc() ->
    case file:read_file("/etc/timezone") of
        {ok, NewLinedTimezone} ->
            {some, string:trim(NewLinedTimezone)};
        {error, _} ->
            case file:read_link("/etc/localtime") of
                {ok, Path} ->
                    {some, timezone_from_path(Path)};
                {error, _} ->
                    none
            end
    end.

timezone_from_path(Path) ->
    Split = string:split(Path, "/", all),
    ZoneParts = lists:dropwhile(
        fun(Segment) -> not lists:member(Segment, ?Regions) end, Split
    ),
    binary:list_to_bin(string:join(ZoneParts, "/")).

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

win_to_iana("Afghanistan Standard Time") -> {some, <<"Asia/Kabul">>};
win_to_iana("Alaskan Standard Time") -> {some, <<"America/Anchorage">>};
win_to_iana("Aleutian Standard Time") -> {some, <<"America/Adak">>};
win_to_iana("Altai Standard Time") -> {some, <<"Asia/Barnaul">>};
win_to_iana("Arab Standard Time") -> {some, <<"Asia/Riyadh">>};
win_to_iana("Arabian Standard Time") -> {some, <<"Asia/Dubai">>};
win_to_iana("Arabic Standard Time") -> {some, <<"Asia/Baghdad">>};
win_to_iana("Argentina Standard Time") -> {some, <<"America/Argentina/La_Rioja">>};
win_to_iana("Astrakhan Standard Time") -> {some, <<"Europe/Astrakhan">>};
win_to_iana("Atlantic Standard Time") -> {some, <<"Atlantic/Bermuda">>};
win_to_iana("AUS Central Standard Time") -> {some, <<"Australia/Darwin">>};
win_to_iana("Aus Central W. Standard Time") -> {some, <<"Australia/Eucla">>};
win_to_iana("AUS Eastern Standard Time") -> {some, <<"Australia/Sydney">>};
win_to_iana("Azerbaijan Standard Time") -> {some, <<"Asia/Baku">>};
win_to_iana("Azores Standard Time") -> {some, <<"Atlantic/Azores">>};
win_to_iana("Bahia Standard Time") -> {some, <<"America/Bahia">>};
win_to_iana("Bangladesh Standard Time") -> {some, <<"Asia/Dhaka">>};
win_to_iana("Belarus Standard Time") -> {some, <<"Europe/Minsk">>};
win_to_iana("Bougainville Standard Time") -> {some, <<"Pacific/Bougainville">>};
win_to_iana("Canada Central Standard Time") -> {some, <<"America/Regina">>};
win_to_iana("Cape Verde Standard Time") -> {some, <<"Atlantic/Cape_Verde">>};
win_to_iana("Caucasus Standard Time") -> {some, <<"Asia/Yerevan">>};
win_to_iana("Cen. Australia Standard Time") -> {some, <<"Australia/Adelaide">>};
win_to_iana("Central America Standard Time") -> {some, <<"America/Guatemala">>};
win_to_iana("Central Asia Standard Time") -> {some, <<"Asia/Almaty">>};
win_to_iana("Central Brazilian Standard Time") -> {some, <<"America/Campo_Grande">>};
win_to_iana("Central Europe Standard Time") -> {some, <<"Europe/Budapest">>};
win_to_iana("Central European Standard Time") -> {some, <<"Europe/Warsaw">>};
win_to_iana("Central Pacific Standard Time") -> {some, <<"Pacific/Guadalcanal">>};
win_to_iana("Central Standard Time") -> {some, <<"CST6CDT">>};
win_to_iana("Central Standard Time (Mexico)") -> {some, <<"America/Mexico_City">>};
win_to_iana("Chatham Islands Standard Time") -> {some, <<"Pacific/Chatham">>};
win_to_iana("China Standard Time") -> {some, <<"Asia/Shanghai">>};
win_to_iana("Cuba Standard Time") -> {some, <<"America/Havana">>};
win_to_iana("Dateline Standard Time") -> {some, <<"Etc/GMT+12">>};
win_to_iana("E. Africa Standard Time") -> {some, <<"Africa/Nairobi">>};
win_to_iana("E. Australia Standard Time") -> {some, <<"Australia/Brisbane">>};
win_to_iana("E. Europe Standard Time") -> {some, <<"Europe/Chisinau">>};
win_to_iana("E. South America Standard Time") -> {some, <<"America/Sao_Paulo">>};
win_to_iana("Easter Island Standard Time") -> {some, <<"Pacific/Easter">>};
win_to_iana("Eastern Standard Time") -> {some, <<"EST5EDT">>};
win_to_iana("Eastern Standard Time (Mexico)") -> {some, <<"America/Cancun">>};
win_to_iana("Egypt Standard Time") -> {some, <<"Africa/Cairo">>};
win_to_iana("Ekaterinburg Standard Time") -> {some, <<"Asia/Yekaterinburg">>};
win_to_iana("Fiji Standard Time") -> {some, <<"Pacific/Fiji">>};
win_to_iana("FLE Standard Time") -> {some, <<"Europe/Riga">>};
win_to_iana("Georgian Standard Time") -> {some, <<"Asia/Tbilisi">>};
win_to_iana("GMT Standard Time") -> {some, <<"Europe/London">>};
win_to_iana("Greenland Standard Time") -> {some, <<"Etc/GMT+2">>};
win_to_iana("Greenwich Standard Time") -> {some, <<"Africa/Monrovia">>};
win_to_iana("GTB Standard Time") -> {some, <<"Europe/Bucharest">>};
win_to_iana("Haiti Standard Time") -> {some, <<"America/Port-au-Prince">>};
win_to_iana("Hawaiian Standard Time") -> {some, <<"Etc/GMT+10">>};
win_to_iana("Iran Standard Time") -> {some, <<"Asia/Tehran">>};
win_to_iana("Israel Standard Time") -> {some, <<"Asia/Jerusalem">>};
win_to_iana("Jordan Standard Time") -> {some, <<"Asia/Amman">>};
win_to_iana("Kaliningrad Standard Time") -> {some, <<"Europe/Kaliningrad">>};
win_to_iana("Korea Standard Time") -> {some, <<"Asia/Seoul">>};
win_to_iana("Libya Standard Time") -> {some, <<"Africa/Tripoli">>};
win_to_iana("Line Islands Standard Time") -> {some, <<"Pacific/Kiritimati">>};
win_to_iana("Lord Howe Standard Time") -> {some, <<"Australia/Lord_Howe">>};
win_to_iana("Magadan Standard Time") -> {some, <<"Asia/Magadan">>};
win_to_iana("Magallanes Standard Time") -> {some, <<"America/Punta_Arenas">>};
win_to_iana("Marquesas Standard Time") -> {some, <<"Pacific/Marquesas">>};
win_to_iana("Mauritius Standard Time") -> {some, <<"Indian/Mauritius">>};
win_to_iana("Middle East Standard Time") -> {some, <<"Asia/Beirut">>};
win_to_iana("Montevideo Standard Time") -> {some, <<"America/Montevideo">>};
win_to_iana("Morocco Standard Time") -> {some, <<"Africa/Casablanca">>};
win_to_iana("Mountain Standard Time") -> {some, <<"MST7MDT">>};
win_to_iana("Mountain Standard Time (Mexico)") -> {some, <<"America/Mazatlan">>};
win_to_iana("N. Central Asia Standard Time") -> {some, <<"Asia/Novosibirsk">>};
win_to_iana("Namibia Standard Time") -> {some, <<"Africa/Windhoek">>};
win_to_iana("New Zealand Standard Time") -> {some, <<"Pacific/Auckland">>};
win_to_iana("Newfoundland Standard Time") -> {some, <<"America/St_Johns">>};
win_to_iana("Norfolk Standard Time") -> {some, <<"Pacific/Norfolk">>};
win_to_iana("North Asia East Standard Time") -> {some, <<"Asia/Irkutsk">>};
win_to_iana("North Asia Standard Time") -> {some, <<"Asia/Krasnoyarsk">>};
win_to_iana("North Korea Standard Time") -> {some, <<"Asia/Pyongyang">>};
win_to_iana("Omsk Standard Time") -> {some, <<"Asia/Omsk">>};
win_to_iana("Pacific SA Standard Time") -> {some, <<"America/Santiago">>};
win_to_iana("Pacific Standard Time") -> {some, <<"PST8PDT">>};
win_to_iana("Pacific Standard Time (Mexico)") -> {some, <<"America/Tijuana">>};
win_to_iana("Pakistan Standard Time") -> {some, <<"Asia/Karachi">>};
win_to_iana("Paraguay Standard Time") -> {some, <<"America/Asuncion">>};
win_to_iana("Qyzylorda Standard Time") -> {some, <<"Asia/Qyzylorda">>};
win_to_iana("Romance Standard Time") -> {some, <<"Europe/Paris">>};
win_to_iana("Russia Time Zone 10") -> {some, <<"Asia/Srednekolymsk">>};
win_to_iana("Russia Time Zone 11") -> {some, <<"Asia/Kamchatka">>};
win_to_iana("Russia Time Zone 3") -> {some, <<"Europe/Samara">>};
win_to_iana("Russian Standard Time") -> {some, <<"Europe/Moscow">>};
win_to_iana("SA Eastern Standard Time") -> {some, <<"America/Santarem">>};
win_to_iana("SA Pacific Standard Time") -> {some, <<"Etc/GMT+5">>};
win_to_iana("SA Western Standard Time") -> {some, <<"America/Santo_Domingo">>};
win_to_iana("Saint Pierre Standard Time") -> {some, <<"America/Miquelon">>};
win_to_iana("Sakhalin Standard Time") -> {some, <<"Asia/Sakhalin">>};
win_to_iana("Samoa Standard Time") -> {some, <<"Pacific/Apia">>};
win_to_iana("Sao Tome Standard Time") -> {some, <<"Africa/Sao_Tome">>};
win_to_iana("Saratov Standard Time") -> {some, <<"Europe/Saratov">>};
win_to_iana("SE Asia Standard Time") -> {some, <<"Asia/Bangkok">>};
win_to_iana("Singapore Standard Time") -> {some, <<"Asia/Singapore">>};
win_to_iana("South Africa Standard Time") -> {some, <<"Africa/Johannesburg">>};
win_to_iana("South Sudan Standard Time") -> {some, <<"Africa/Juba">>};
win_to_iana("Sri Lanka Standard Time") -> {some, <<"Asia/Colombo">>};
win_to_iana("Sudan Standard Time") -> {some, <<"Africa/Khartoum">>};
win_to_iana("Syria Standard Time") -> {some, <<"Asia/Damascus">>};
win_to_iana("Taipei Standard Time") -> {some, <<"Asia/Taipei">>};
win_to_iana("Tasmania Standard Time") -> {some, <<"Australia/Hobart">>};
win_to_iana("Tocantins Standard Time") -> {some, <<"America/Araguaina">>};
win_to_iana("Tokyo Standard Time") -> {some, <<"Asia/Tokyo">>};
win_to_iana("Tomsk Standard Time") -> {some, <<"Asia/Tomsk">>};
win_to_iana("Tonga Standard Time") -> {some, <<"Pacific/Tongatapu">>};
win_to_iana("Transbaikal Standard Time") -> {some, <<"Asia/Chita">>};
win_to_iana("Turkey Standard Time") -> {some, <<"Europe/Istanbul">>};
win_to_iana("Turks And Caicos Standard Time") -> {some, <<"America/Grand_Turk">>};
win_to_iana("Ulaanbaatar Standard Time") -> {some, <<"Asia/Ulaanbaatar">>};
win_to_iana("US Eastern Standard Time") -> {some, <<"America/Indiana/Marengo">>};
win_to_iana("US Mountain Standard Time") -> {some, <<"Etc/GMT+7">>};
win_to_iana("UTC") -> {some, <<"Etc/GMT">>};
win_to_iana("UTC-02") -> {some, <<"Etc/GMT+2">>};
win_to_iana("UTC-08") -> {some, <<"Pacific/Pitcairn">>};
win_to_iana("UTC-09") -> {some, <<"Pacific/Gambier">>};
win_to_iana("UTC-11") -> {some, <<"Etc/GMT+11">>};
win_to_iana("UTC+12") -> {some, <<"Etc/GMT-12">>};
win_to_iana("UTC+13") -> {some, <<"Pacific/Fakaofo">>};
win_to_iana("Venezuela Standard Time") -> {some, <<"America/Caracas">>};
win_to_iana("Vladivostok Standard Time") -> {some, <<"Asia/Vladivostok">>};
win_to_iana("Volgograd Standard Time") -> {some, <<"Europe/Volgograd">>};
win_to_iana("W. Australia Standard Time") -> {some, <<"Australia/Perth">>};
win_to_iana("W. Central Africa Standard Time") -> {some, <<"Etc/GMT-1">>};
win_to_iana("W. Europe Standard Time") -> {some, <<"Europe/Berlin">>};
win_to_iana("W. Mongolia Standard Time") -> {some, <<"Asia/Hovd">>};
win_to_iana("West Asia Standard Time") -> {some, <<"Asia/Tashkent">>};
win_to_iana("West Bank Standard Time") -> {some, <<"Asia/Gaza">>};
win_to_iana("West Pacific Standard Time") -> {some, <<"Pacific/Port_Moresby">>};
win_to_iana("Yakutsk Standard Time") -> {some, <<"Asia/Yakutsk">>};
win_to_iana("Yukon Standard Time") -> {some, <<"America/Whitehorse">>};
win_to_iana(_) -> none.
