

import colander
import deform

from pyramid.renderers import render

# Schemas

class UserRegistrationSchema(deform.schema.CSRFSchema):
    email = colander.SchemaNode(
        colander.String(allow_empty=False),
        validator=colander.Email(),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    username = colander.SchemaNode(
        colander.String(allow_empty=False),
        validator=colander.Length(max=154),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    password = colander.SchemaNode(
        colander.String(),
        validator=colander.Length(min=8),
        widget=deform.widget.CheckedPasswordWidget(
            redisplay=True,
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )

class UserLoginSchema(deform.schema.CSRFSchema):
    email = colander.SchemaNode(
        colander.String(allow_empty=False),
        validator=colander.Email(),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    password = colander.SchemaNode(
        colander.String(),
        validator=colander.Length(min=8),
        widget=deform.widget.PasswordWidget(
            redisplay=True,
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )

# TODO: All Read/Write Models have to be CamelCase because of Go/DynamoDB Backend

class UserProfileSchema(deform.schema.CSRFSchema):
    Location = colander.SchemaNode(
        colander.String(allow_empty=False),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    AvatarHash = colander.SchemaNode(
        colander.String(allow_empty=False),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    Name = colander.SchemaNode(
        colander.String(allow_empty=False),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    Age = colander.SchemaNode(
        colander.Int(),
        widget=deform.widget.TextInputWidget(
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )
    AboutMe = colander.SchemaNode(
        colander.String(allow_empty=False),
        widget=deform.widget.RichTextWidget(
            options=(('browser_spellcheck', True),),
            css_class='input is-large',
            item_css_class='control',
            error_class='is-danger'
        )
    )


# Helper Methods

def get_user_registration_form(
        request,
        button_name='register',
        use_ajax=True
):
    schema = UserRegistrationSchema().bind(request=request)
    form = deform.Form(
        schema,
        action=request.route_url('register'),
        use_ajax=use_ajax,
        buttons=(button_name,),
        css_class='form'
    )
    return form

def get_user_login_form(
        request,
        button_name='login',
        use_ajax=True
):
    schema = UserLoginSchema().bind(request=request)
    form = deform.Form(
        schema,
        action=request.route_url('login'),
        use_ajax=use_ajax,
        buttons=(button_name,),
        css_class='form'
    )
    return form

def get_user_profile_form(
        request,
        button_name='update',
        use_ajax=True
):
    schema = UserProfileSchema().bind(request=request)
    form = deform.Form(
        schema,
        action=request.route_url('profile'),
        use_ajax=use_ajax,
        buttons=(button_name,),
        css_class='form'
    )
    return form
