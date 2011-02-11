%%%-------------------------------------------------------------------
%% @author Vitalii Kulchevych <coolchevy@gmail.com>
%% @copyright coolchevy
%% @doc DateTime validate functions
%% @end
%%%-------------------------------------------------------------------
-module(datetime).

%-include_lib("eunit/include/eunit.hrl").

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

-define(DATE_FORMAT, "^"++?YEAR++?DATE_DELIMITER++?MONTH++?DATE_DELIMITER++?DAY++"$").
-define(TIME_FORMAT, "^"++?HOUR++?TIME_DELIMITER++?MINUTE++?TIME_DELIMITER++?SECOND++"$").

%%====================================================================
%% API
%%====================================================================

validate_date(Date) ->
    case re:run(string:strip(Date), ?DATE_FORMAT, [{capture,[year,month,day]}]) of
        nomatch ->
            {error, <<"Invalid date format. Example: YYYY-MM-DD">>};
        {match,[{YearS,YearL},{MonthS,MonthL},{DayS,DayL}]} ->
            {string:substr(Date,YearS+1,YearL+1),string:substr(Date,MonthS+1,MonthL+1), string:substr(Date,DayS+1,DayL+1)},
            true
    end.

validate_time(Date) ->
    case re:run(string:strip(Date), ?TIME_FORMAT, [{capture,[hour,minute,second]}]) of
        nomatch ->
            {error, <<"Invalid time format. Example: HH:MM:SS">>};
        {match,[{HourS,HourL},{MinuteS,MinuteL},{SecondS,SecondL}]} ->
            {string:substr(Date,HourS+1,HourL+1),string:substr(Date,MinuteS+1,MinuteL+1), string:substr(Date,SecondS+1,SecondL+1)},
            true
    end.

validate_datetime(Date) ->
    case re:run(string:strip(Date), "^(?<date>"++?DATE_FORMAT++")[\s]+(?<time>"++?TIME_FORMAT++")$", [{capture,[date,time]}]) of
        nomatch ->
            {error, <<"Invalid date time format. Example: YYYY-MM-DD HH:MM:SS">>};
        {match,[{DateS,DateL},{TimeS,TimeL}]} ->
            case validate_date(string:substr(Date, DateS+1, DateL+1)) andalso validate_time(string:substr(Date, TimeS+1, TimeL+1)) of
                true ->
                    true;
                Error ->
                    Error
            end
    end.
