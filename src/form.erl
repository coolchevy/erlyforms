%%%-------------------------------------------------------------------
%% @author Vitalii Kulchevych <coolchevy@gmail.com>
%% @doc Form generation, validation and processing library
%% @end
%%%-------------------------------------------------------------------
-module(form).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([create/5,
         create/6,
         render/1,
         render_with_data/2,
         render_with_validation/3,
         render_with_fields/2,
         field/1,
         text/1,
         date/1,
         datetime/1,
         password/1,
         submit/1,
         hidden/1,
         select/1,
         multiple_select/1,
         validate/2,
         validate_field/3,
         valid_fields/3,
         valid_post/2,
         rules/1]).

-record(form, {title::string(), formname::string(), action::string(), fields = []::list(), rules = []::list(), template::atom()}).
%-record(vform, {form, data, validation_result}).
-record(field, {
    name::string(),
    type::atom(),
    title::string(),
    id::string(),
    value::string(),
    rules = []::list(),
    choices = []::list(),
    attrs = []::list(),
    template::atom(),
    required::boolean()
    }).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create form structure used default template
%% @spec create(Title::string(), 
%%              FormName::string(),
%%              Action::string(),
%%              Fields::list(),
%%              Rules::list()
%%                              ) -> #form{} 
%% @end
%%--------------------------------------------------------------------

-spec(create(Title::string(), FormName::string(),
        Action::string(), Fields::list(), 
        Rules::list()) -> #form{}).
create(Title, FormName, Action, Fields, Rules) ->
    create(Title, FormName, form_template_dtl, Action, Fields, Rules).

%%--------------------------------------------------------------------
%% @doc
%% Create form structure
%% @spec create(Title::string(), 
%%              FormName::string(),
%%              FormTemplate::atom(),
%%              Action::string(),
%%              Fields::list(),
%%              Rules::list()
%%                              ) -> #form{}
%% @end
%%--------------------------------------------------------------------

-spec(create(Title::string(), FormTemplate::atom(), FormName::string(),
        Action::string(), Fields::list(), 
        Rules::list()) -> #form{}).
create(Title, FormName, FormTemplate, Action, Fields, Rules) ->
    #form{title=Title,
          formname=FormName,
          action=Action,
          fields=Fields,
          template=FormTemplate,
          rules=Rules}.

field(Field) ->
    Name = proplists:get_value(name,Field,"field"),
    #field{
        name = Name,
        type = proplists:get_value(type,Field,text),
        title = proplists:get_value(title,Field,Name),
        id = field_id(Name),
        value = proplists:get_value(value,Field),
        rules = proplists:get_value(rules,Field,[]),
        choices = proplists:get_value(choices,Field,[]),
        attrs = proplists:get_value(attrs,Field,[]),
        template = proplists:get_value(template, Field, input_field_template_dtl),
        required = field_required(proplists:get_value(rules,Field,[]))
    }.


text(Field) ->
    field(lists:ukeysort(1, Field ++ [{type,text}])).

datetime(Field) ->
    Rules = proplists:get_value(rules,Field,[]) ++ ['datetime'],
    Attrs = proplists:get_value(attrs,Field,[]) ++ [{class,"datetimecalendar"}],
    field(lists:ukeysort(1, [{attrs,Attrs}, {rules, Rules}] ++ Field ++ [{type,text}])).

date(Field) ->
    Rules = proplists:get_value(rules,Field,[]) ++ ['datetime'],
    Attrs = proplists:get_value(attrs,Field,[]) ++ [{class, "datecalendar"}],
    field(lists:ukeysort(1, [{attrs,Attrs}, {rules, Rules}] ++ Field ++ [{type,text}])).

password(Field) ->
    field(Field ++ [{type,password}]).

submit(Field) ->
    field(Field ++ [{type,submit}]).

hidden(Field) ->
        field(Field ++ [{type,hidden}]).

multiple_select(Field) ->
    DefRules = proplists:get_value(rules,Field,[]),
    DefChoices = proplists:get_value(choices,Field,[]),
    case field_required(DefRules) of
        true ->
            Rules = DefRules ++ [{members,[X || {X,_} <- DefChoices]}];
        false ->
            Rules = DefRules
    end,
    Attrs = proplists:get_value(attrs,Field,[]) ++ [{"multiple","multiple"}],
    field(lists:ukeysort(1, [{attrs,Attrs}, {rules, Rules}] ++ Field ++ [{template,multiple_select_field_template_dtl}])).


select(Field) ->
    DefRules = proplists:get_value(rules,Field,[]),
    DefChoices = proplists:get_value(choices,Field,[]),
    case field_required(DefRules) of
        true ->
                Rules = DefRules ++ [{member,[X || {X,_} <- DefChoices]}];
        false ->
                Rules = DefRules
    end,
    field(lists:ukeysort(1, [{rules, Rules}] ++ Field ++ [{template,select_field_template_dtl}])).

render(F = #form{}) ->
    render_form(F, []).

render_with_data(F = #form{}, Data) ->
    Result = validate(F, Data),
    render_with_validation(F, Result, Data).

render_with_validation(F, Result, Data) ->
    ValidFields = valid_fields(F, Result, Data),
    render_with_fields(F, ValidFields).

render_with_fields(F = #form{}, ValidFields) ->
    render_form(F, ValidFields).

validate(Form, Data) ->
    form_validator:validate(rules(Form), Data).

rules(#form{fields=Fields, rules=FormRules}) ->
    [{Name,Rules}
     || #field{type=Type,name=Name,rules=Rules} <- Fields,
        Type =/= submit]
        ++ FormRules.

validate_field(Form, Field, Data) ->
     case proplists:get_value(Field, rules(Form)) of
         undefined -> erlang:error({no_such_rule, Field});
         List when is_list(List) ->
             form_validator:validate_rule({Field, List}, Data)
     end.

valid_fields(F, Result, Data) ->
    Simple = simple_copy(Result, Data),
    % on a {Name, {duplication, ...} rule, put {Name, Field} into
    Complex = [ case {proplists:get_value(Field, Result),
                      proplists:get_value(Field, Simple)} of
                    {[], Value} ->
                        {Rule, Value};
                    _ ->
                        []
                end
                || {Rule, [{duplication, [Field|_Fields]}]} <- form_rules(F),
                   proplists:get_value(Rule, Result) =:= []],
    lists:flatten([Simple, Complex]).



valid_post(F = #form{}, Data) ->
    %% Fix Array in Post
    UniqData = post_array(Data),
    Result = form:validate(F, UniqData),
    Fields = valid_fields(F, Result, UniqData),
    case form_validator:is_valid(Result) of
        true ->
            {valid, Fields};
        false ->
            {invalid, Result, Fields}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

form_rules(#form{rules=R}) -> R.

simple_copy(Results, Data) ->
    lists:flatmap(fun ({Field, []}) ->
                          case proplists:get_value(Field, Data) of
                              undefined -> [];
                              V ->
                                  [{Field, V}]
                          end;
                      (_) -> []
                  end,
                  Results).

%%--------------------------------------------------------------------
%% @doc render form template
%% @spec render_form(Form :: record(), ValidFields :: list()) -> {ok, list()}
%% @end
%%--------------------------------------------------------------------

-spec(render_form(Form :: record(), ValidFields :: list()) -> {ok, list()}).
render_form(#form{title=Title,
                  formname=FormName,
                  action=Action,
                  template=Template,
                  fields=Fields}, ValidFields) ->
    apply(Template,render,[
                        [{form_action, Action},
                          {form_title, Title},
                          {form_name, FormName},
                          {fields, [ T
                                     || {ok, T} <-
                                            lists:map(fun (F) -> render_field(F, ValidFields) end,
                                                Fields)]}]
                            ]).

%%--------------------------------------------------------------------
%% @doc render field template
%% @spec render_field(Field :: record(),  ValidFields :: list()) -> {ok, list()}
%% @end
%%--------------------------------------------------------------------

-spec(render_field(Field :: record(), ValidFields :: list()) -> {ok, list()}).
render_field(#field{type=Type, name=Name, value=Initial, title=Title, id=Id, choices=Choices, template=Template, required=Required, attrs=Attrs}, ValidFields) ->
    render_field_template(proplists:get_value(Name, ValidFields),Name,Title,Type,Id,Initial,Choices,Template,Required,Attrs).

render_field_template(undefined, Name, Title, Type, Id, Initial, Choices, Template, Required, Attrs)->
    render_field_template(Initial, Name, Title, Type, Id, Choices, Template, Required, Attrs);
render_field_template(Value, Name, Title, Type, Id, _Initial, Choices, Template, Required, Attrs)->
        render_field_template(Value, Name, Title, Type, Id, Choices, Template, Required, Attrs).


render_field_template(undefined, Name, Title, Type, Id, Choices, Template, Required, Attrs)->
 apply(Template,render, [
                             [{name, Name},
                              {title, Title},
                              {type, atom_to_list(Type)},
                              {choices, Choices},
                              {required, Required},
                              {value, []},
                              {attrs, attrs_to_string(Attrs)},
                              {id, Id}]
                      ]);
render_field_template(Value, Name, Title, Type, Id, Choices, Template, Required, Attrs)->
  apply(Template,render, [
                              [{name, Name},
                               {title, Title},
                               {type, atom_to_list(Type)},
                               {choices, Choices},
                               {id, Id},
                               {required, Required},
                               {attrs, attrs_to_string(Attrs)},
                               {value, Value}]
                       ]).

attrs_to_string(Attrs) ->
    string:join([io_lib:format("~ts=\"~ts\"", [Name,Value]) || {Name, Value} <- Attrs], " ").

%%--------------------------------------------------------------------
%% @doc generate field name
%% @spec field_name(Prefix :: string(), Title :: string()) -> string()
%% @end
%%--------------------------------------------------------------------

-spec(field_name(Prefix :: string(), Title :: string()) -> string()).
field_name(Prefix, <<Title/binary>>) ->
    field_name(Prefix, binary_to_list(Title));
field_name(Prefix, Title) when is_list(Title) ->
    S = lists:filter(fun (C) when $A =< C, C =< $Z;
                         $a =< C, C =< $z;
                         $0 =< C, C =< $9;
                         C =:= $_;
                         C =:= $.;
                         C =:= $- ->
                             true;
                         (_) -> false
                     end,
                     Title),
    Prefix ++ string:to_lower(S);
field_name(_Prefix, _Title) ->
    field_name_bad_format.


%field_name_test() ->
%    ?assertMatch("txtpassword", field_name("txt", "Password:")),
%    ?assertMatch("txtpassword", field_name("txt", "pass word")),
%    ?assertMatch("pwpassword", field_name("pw", "Password:")),
%    ?assertMatch("pwpassword", field_name("pw", "pass word")).

%%--------------------------------------------------------------------
%% @doc generate field id
%% @spec field_id(FieldName :: string()) -> string()
%% @end
%%--------------------------------------------------------------------

-spec(field_id(FieldName :: string()) -> string()).
field_id(FieldName) ->
    "id_" ++ FieldName.


field_required(Rules) ->
    lists:member('not_empty',Rules).

%create_test() ->
%    ?assertMatch(#form{},
%                 create("Setup Information", "","",
%                        [text("User Name:", [{length, [3,30]}]),
%                         text("Email address:", [email_address]),
%                         password("Password:", "txtpassword", [{length, [8,infinity]}]),
%                         password("Confirm Password:", "txtpasswordc", []),
%                         submit("Signup")],
%                        [{"passwords", [{duplication, ["txtpassword", "txtpasswordc"]}]}])).

valid_fields_test() ->
    ?assertMatch([{"valid", foo}],
                 valid_fields(#form{},
                              [{"valid", []}],
                              [{"valid", foo}])),
    ?assertMatch([{"valid", foo}],
                 valid_fields(#form{rules=[{"Foo", [{duplication, ["valid", "other"]}]}]},
                              [{"valid", []}],
                              [{"valid", foo},
                               {"invalid", bar},
                               {"other", baz},
                               {"random", baz}])),
    ?assertMatch([{"valid", foo},
                  {"Foo", foo}],
                 valid_fields(#form{rules=[{"Foo", [{duplication, ["valid", "other"]}]}]},
                              [{"valid", []},
                               {"Foo", []}],
                              [{"valid", foo},
                               {"invalid", bar},
                               {"other", baz},
                               {"random", baz}])),
    ?assertMatch([{"valid", foo},
                  {"other", baz},
                  {"Foo", foo}],
                 valid_fields(#form{rules=[{"Foo", [{duplication, ["valid", "other"]}]}]},
                              [{"valid", []},
                               {"other", []},
                               {"Foo", []}],
                              [{"valid", foo},
                               {"invalid", bar},
                               {"other", baz},
                               {"random", baz}])).

simple_copy_test() ->
    ?assertMatch([{"valid", foo}],
                 simple_copy([{"invalid", [error]},
                              {"valid", []}],
                             [{"invalid", invalid},
                              {"valid", foo}])).

post_array(Data) ->
  [{K,post_array_uniq(proplists:get_all_values(K, Data))} || {K,_} <- Data].
post_array_uniq(Data) when is_list(Data), length(Data) > 1 ->
  Data;
post_array_uniq(Data) ->
  lists:last(Data).
