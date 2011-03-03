%%%-------------------------------------------------------------------
%% @author Vitalii Kulchevych <coolchevy@gmail.com>
%% @copyright coolchevy
%% @doc DateTime validate functions
%% @end
%%%-------------------------------------------------------------------
-module(datetime).


%% API
-export([
         validate_time/1,
         validate_date/1,
         validate_datetime/1
     ]).


-define(YEAR, "(?<year>[0-9]{4})").
-define(MONTH, "(?<month>[0-9]{2})").
-define(DAY, "(?<day>[0-9]{2})").
-define(DATE_DELIMITER,"[/\\-\\.]").
-define(HOUR, "(?<hour>[0-9]{2})").
-define(MINUTE, "(?<minute>[0-9]{2})").
-define(SECOND, "(?<second>[0-9]{2})").
-define(TIME_DELIMITER,"[:]").

-define(DATE_FORMAT, ?YEAR++?DATE_DELIMITER++?MONTH++?DATE_DELIMITER++?DAY).
-define(TIME_FORMAT, ?HOUR++?TIME_DELIMITER++?MINUTE++?TIME_DELIMITER++?SECOND).

%%====================================================================
%% API
%%====================================================================

validate_date(Date) ->
    case re:run(string:strip(Date), "^"++?DATE_FORMAT++"$", [{capture,[year,month,day],list}]) of
        nomatch ->
            {error, <<"Invalid date format. Example: YYYY-MM-DD">>};
        {match,[_Year, _Month, _Day]} ->
            %{Year, Month, Day},
            true
    end.

validate_time(Date) ->
    case re:run(string:strip(Date), "^"++?TIME_FORMAT++"$", [{capture,[hour,minute,second],list}]) of
        nomatch ->
            {error, <<"Invalid time format. Example: HH:MM:SS">>};
        {match,[_Hour, _Minute, _Second]} ->
            %{Hour, Minute, Second},
            true
    end.

validate_datetime(DateTime) ->
    case re:run(string:strip(DateTime), "^(?<date>"++?DATE_FORMAT++")[\s]+(?<time>"++?TIME_FORMAT++")$", [{capture,[date,time],list}]) of
        nomatch ->
            {error, <<"Invalid date time format. Example: YYYY-MM-DD HH:MM:SS">>};
        {match,[Date, Time]} ->
            case validate_date(Date) andalso validate_time(Time) of
                true ->
                    true;
                Error ->
                    Error
            end
    end.
