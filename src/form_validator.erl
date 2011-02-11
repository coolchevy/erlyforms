%%%-------------------------------------------------------------------
%% @author Vitalii Kulchevych <coolchevy@gmail.com>
%% @doc HTML Form validation functions
%% @end
%%%-------------------------------------------------------------------
-module(form_validator).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([validate/2
         ,is_valid/1
         ,has_error/1
         ,invalid_fields/1
         ,field_errors/2
         ,errors/1
         ,valid_fields/2
         ,validate_rule/2
         ,rule_fields/1]).

%%====================================================================
%% API
%%====================================================================

%% @type validation() = {RuleNmae::string(), [predicate_result()]}.
%% @type validation_result() = [validation()].

%% @type form_data() = [{FieldName::string(), FieldValue::string()}].

%% @spec validate([rule()], form_data()) -> validation_result()
validate(Rules,Data) when is_list(Rules), is_list(Data) ->
    lists:map(fun (Rule) -> validate_rule(Rule, Data) end,
              Rules).

%% @spec invalid_fields(validation_result()) -> field()
invalid_fields(Results) ->
    [Field || {Field, Errors} <- Results,
              length(Errors) >= 1].

%% @spec is_valid(validation_result()) -> bool()
is_valid(Results) ->
    invalid_fields(Results) =:= [].

%% @spec has_error(validation_result()) -> bool()
has_error(Results) ->
    is_valid(Results) =:= false.

%% @spec field_errors(validation_result(), field()) -> validation()
field_errors(Name, Results) ->
    case lists:keysearch(Name, 1, Results) of
        false -> erlang:error(no_such_field);
        {value, V} -> V
    end.

%% @spec errors(validation_result()) -> field()
errors(Results) ->
    [Field || Field = {_, Errors} <- Results,
              length(Errors) >= 1].

%% @spec valid_fields(validation_result(), form_data()) -> form_data()
valid_fields(Results, Data) ->
    Fields = [ Field || {Field, []} <- Results],
    [ {K,V} || {K,V} <- Data,
               lists:member(K, Fields) ].

%% @spec validate_rule(rule(), form_data()) -> validation()
validate_rule({Name, Predicates}, Data) when is_list(Name), is_list(Predicates) ->
    {Name,
     lists:flatmap(fun (Predicate) ->
                           case validate_predicate(Predicate, Name, Data) of
                               true -> [];
                               false -> [{error, Predicate, false}];
                               {error, Reason} -> [{error, Predicate, Reason}]
                           end
                  end, normalize_predicates(Predicates))}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @type field() = string().
%% A field name.

%% @type rule() = {Name, [predicate()]}.
%%  where Name = fields() | string()
%% A form validation rule. Most predicates assume the name of the rule
%% is also the name of the field to validate.

%% @type predicate() = {'duplication', [field()]} |
%%                     'not_empty' |
%%                     'string' |
%%                     {'length', LengthSpec} |
%%                     {'predicate', function()} |
%%                     {'not_predicate', function()} |
%%                     'email_address' |
%%                     {'regex', Regex::string()} |
%%                     {'member', [term()]}.
%% where
%%     LengthSpec = [Exact::integer()] | [Min::integer(), Max::integer()].
%%
%% A test that determines the validity of field values.
%% <ul>
%% <li>{'duplication', [field()]} requires that the list of fields given have the same value.</li>
%% <li>not_empty - the value must not be missing or the empty string</li>
%% <li>string - the value must not be missing</li>
%% <li>length - reqires that the value is either exactly Exact chars long or between Min and Max inclusive.</li>
%% <li>{predicate, Fun} - requrires Fun(Value) to return true</li>
%% <li>{not_predicate, Fun} - requrires Fun(Value) to return false</li>
%% <li>email_address - requires the value to validate as an email address</li>
%% <li>{regex, Regex} - requires that the value match the regex (using the rvre engine)</li>
%% <li>{member, List} - requires that the value be a member of the given List.</li>
%% </ul>

%% @spec validate_predicate(predicate(), field(), Data) -> true | false | {error, Reason}
%% @doc Checks a single predicate in a rule.
%% @end
validate_predicate({duplication, [Field|Duplicates]}, _Name, Data)
  when is_list(Duplicates), length(Duplicates) >= 1 ->
    FieldValue = proplists:get_value(Field, Data),
    same_value(Field, FieldValue, Duplicates, Data);
    
validate_predicate(Predicate, Name, Data) ->
    validate_predicate_simple(Predicate, proplists:get_value(Name, Data)).


%% @private
validate_predicate_simple(not_empty, L) when is_list(L), length(L) > 0 -> true;
validate_predicate_simple(not_empty, _) -> {error,<<"This field is required">>};
validate_predicate_simple(string, L) when is_list(L) -> true;
validate_predicate_simple(string, _) -> false;
validate_predicate_simple({length, [Exact]}, L) ->
    validate_predicate_simple({length, [Exact, Exact]}, L);
validate_predicate_simple({length, [Min,Max]}, L) when is_list(L) -> 
    case length(L) of
        Len when Min =< Len, Len =< Max -> true;
        _ -> 
            case Max of
                infinity ->
                    {error,list_to_binary(io_lib:format("This field must contain minimum ~p characters",[Min]))};
                    _ ->
                    {error,list_to_binary(io_lib:format("This field must contain from ~p to ~p characters",[Min,Max]))} 
            end
    end;
validate_predicate_simple({length, [_Min,_Max]}, _L) -> false;
validate_predicate_simple({predicate, P}, L) -> P(L);
validate_predicate_simple({not_predicate, P}, L) -> P(L) =:= false;
validate_predicate_simple(email_address, L) when is_list(L) ->
    email_address:validate(L);
validate_predicate_simple(datetime, L) when is_list(L) ->
    true;
validate_predicate_simple(date, L) when is_list(L) ->
    true;
validate_predicate_simple(email_address, undefined) -> false;
validate_predicate_simple({regex, RE}, L) when is_list(L) ->
    case re:run(L, RE) of
        nomatch -> false;
        {match, _} -> true;
        match -> true
    end;
validate_predicate_simple({regex, _RE}, _L) ->
    {error, not_a_string};
validate_predicate_simple({member, List}, L) ->
    case lists:member(L, List) of
        true -> 
            true;
        false -> {error,<<"Selected variant not in list">>}
    end;
validate_predicate_simple({members, _}, undefined) -> true;
validate_predicate_simple({members, List}, L) when is_list(L) ->
    case ordsets:intersection(ordsets:from_list(List), ordsets:from_list(L)) of
        [] ->
            {error,<<"Selected variant not in list">>};
        _ ->
            true
    end;
%%sql
validate_predicate_simple(_, null) -> true;
validate_predicate_simple(P, V) -> erlang:error({not_implemented, {P, V}}).

%% @private
normalize_predicates(Predicates) when is_list(Predicates) ->
    proplists:normalize(Predicates, [{expand, [{password, string}]}]).

%% @private
same_value(_Field, _FieldValue, [], _Data) -> true;
same_value(Field, FieldValue, [Duplicate|Rest], Data) ->
    case proplists:get_value(Duplicate, Data) of
        undefined -> {error, {missing, Field}};
        V when V =:= FieldValue ->
            same_value(Field, FieldValue, Rest, Data);
        %_ -> {error, {different_value, Field, Duplicate}}
        _ -> {error, list_to_binary(io_lib:format("different_value ~p, ~p",[Field, Duplicate]))}
    end.

%% @spec rule_fields(rule()) -> field()
rule_fields({Name, Rules}) ->
    lists:usort(lists:append([ predicate_fields(Name, Rule)
                               || Rule <- Rules ])).

%% @private
predicate_fields(_Name, {duplication, List}) ->
    List;
predicate_fields(Name, not_empty) -> [Name];
predicate_fields(Name, string) -> [Name];
predicate_fields(Name, {regex, _}) -> [Name];
predicate_fields(Name, {predicate, _}) -> [Name];
predicate_fields(Name, email_address) -> [Name];
predicate_fields(Name, {length, _}) -> [Name];
predicate_fields(Name, {member, _}) -> [Name];
predicate_fields(Name, _) -> erlang:error({unknown_predicate, Name}).
