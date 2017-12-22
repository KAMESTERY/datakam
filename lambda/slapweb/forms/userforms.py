

import colander
import deform

from pyramid.renderers import render

# Schemas

class UserRegistrationSchema(deform.schema.CSRFSchema):
    email = colander.SchemaNode(
        colander.String(allow_empty=False),
        validator=colander.Email()
    )
    username = colander.SchemaNode(
        colander.String(allow_empty=False),
        validator=colander.Length(max=154)
    )
    password = colander.SchemaNode(
        colander.String(),
        validator=colander.Length(min=8),
        widget=deform.widget.CheckedPasswordWidget(redisplay=True)
    )

class UserLoginSchema(deform.schema.CSRFSchema):
    email = colander.SchemaNode(
        colander.String(allow_empty=False),
        validator=colander.Email()
    )
    password = colander.SchemaNode(
        colander.String(),
        validator=colander.Length(min=8),
        widget=deform.widget.PasswordWidget(redisplay=True)
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
        buttons=(button_name,)
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
        buttons=(button_name,)
    )
    return form
