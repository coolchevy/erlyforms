
Introduction
============

The 'erlyforms' application makes possible to create forms 
for your erlang application. 
I have a very long time programming in Django, and now began 
to Erlangen. I very much did not have enough tools to create
and validate forms. I found the source by Geoff Cant made 
their workers, and changed them.



Usage
--------

**Simple form:**

    my_form() ->
        form:create("User Registration Form", "reg_form", ".",
                [
                    text([{name,"name"},{rules,[{length, [3,30]}]}]),
                    text([{name,"email"},{rules,[email_address]}]),
                    password([{name,"password"},{rules,[{length, [8,infinity]}]}]),
                    password([{name,"confirm_password"}]),
                    submit([{value,"Signup"}])
                ],
                [{"passwords", [{duplication, ["password",
                "confirm_password"]}]}]
                ).


**Render form:**
    render() ->
        form:render(my_form()).


**Validate form:**

    process_post(Post) ->
        case form:valid_post(my_form(), Post) of
            {invalid, Results, _Fields} ->
                error_logger:info_msg("~p",["invalid",Results]);
            {valid, Fields} ->
               error_logger:info_msg("~p",["valid",Fields])
        end.


**Render form with data:**

    render_with_initial() ->
        InitialObject = [{"name","User"},{"email","test@exapmle.org"}],
        form:render_with_data(my_form(), InitialObject).


