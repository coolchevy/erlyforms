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
         text/2,
         text/3,
         password/2,
         password/3,
         submit/1,
         submit/2,
         hidden/2,
         select/3,
         select/4,
         custom/3,
         custom/4,
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
    template::atom(),
    initial::string(),
    required::bool()
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

text(Title, Rules) ->
    text(Title, field_name("txt", Title), Rules).

text(Title, Name, Rules) ->
    #field{type=text, title=Title, name=Name, id=field_id(Name), rules=Rules, template=input_field_template_dtl, required=field_required(Rules)}.


password(Title, Rules) ->
    password(Title, field_name("pw", Title), Rules).

password(Title, Name, Rules) ->
    #field{type=password, title=Title, name=Name, id=field_id(Name), rules=Rules, template=input_field_template_dtl, required=field_required(Rules)}. 

submit(Title) ->
    submit(Title, field_name("sbt",Title)).

submit(Title, Name) ->
    #field{type=submit, initial=Title, id=field_id(Name), name=Name,  template=input_field_template_dtl}.

hidden(Name, Rules) ->
    #field{type=hidden, name=Name,id=field_id(Name), rules=Rules,  template=input_field_template_dtl}.

select(Title, Choices, Rules) ->
    select(Title, field_name('select',Title), Choices, Rules).

select(Title, Name, Choices, Rules) ->
    Required = field_required(Rules),
    case Required of
        true ->
                DefaultRules = [{member,[X || {X,_} <- Choices]}],
                AllRules = lists:append(DefaultRules,Rules);
        false ->
                AllRules = Rules
    end,
    #field{name=Name, title=Title, choices=Choices, id=field_id(Name), rules=AllRules,  template=select_field_template_dtl, required=Required}.

custom(Title, Rules, Template) ->
    custom(Title, field_name("custom", Title), Rules, Template).

custom(Title, Name, Rules, Template) ->
    #field{title=Title, name=Name, id=field_id(Name), rules=Rules, template=Template, required=field_required(Rules)}.

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
    Result = form:validate(F, Data),
    Fields = valid_fields(F, Result, Data),
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
render_field(#field{type=Type, name=Name, initial=Initial, title=Title, id=Id, choices=Choices, template=Template, required=Required}, ValidFields) ->
    render_field_template(proplists:get_value(Name, ValidFields),Name,Title,Type,Id,Initial,Choices,Template,Required).

render_field_template(undefined, Name, Title, Type, Id, Initial, Choices, Template, Required)->
    render_field_template(Initial, Name, Title, Type, Id, Choices, Template, Required).

render_field_template(undefined, Name, Title, Type, Id, Choices, Template, Required)->
 apply(Template,render, [
                             [{name, Name},
                              {title, Title},
                              {type, atom_to_list(Type)},
                              {choices, Choices},
                              {required, Required},
                              {id, Id}]
                      ]);
render_field_template(Value, Name, Title, Type, Id, Choices, Template, Required)->
  apply(Template,render, [
                              [{name, Name},
                               {title, Title},
                               {type, atom_to_list(Type)},
                               {choices, Choices},
                               {id, Id},
                               {required, Required},
                               {value, Value}]
                       ]).


%%--------------------------------------------------------------------
%% @doc generate field name
%% @spec field_name(Prefix :: string(), Title :: string()) -> string()
%% @end
%%--------------------------------------------------------------------

-spec(field_name(Prefix :: string(), Title :: string()) -> string()).
field_name(Prefix, Title) ->
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
    Prefix ++ string:to_lower(S).

field_name_test() ->
    ?assertMatch("txtpassword", field_name("txt", "Password:")),
    ?assertMatch("txtpassword", field_name("txt", "pass word")),
    ?assertMatch("pwpassword", field_name("pw", "Password:")),
    ?assertMatch("pwpassword", field_name("pw", "pass word")).

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

create_test() ->
    ?assertMatch(#form{},
                 create("Setup Information", "","",
                        [text("User Name:", [{length, [3,30]}]),
                         text("Email address:", [email_address]),
                         password("Password:", "txtpassword", [{length, [8,infinity]}]),
                         password("Confirm Password:", "txtpasswordc", []),
                         submit("Signup")],
                        [{"passwords", [{duplication, ["txtpassword", "txtpasswordc"]}]}])).

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
