

import colander
import deform

from pyramid.renderers import render

class UserRegistrationSchema(deform.schema.CSRFSchema):
    email = colander.SchemaNode(
        colander.String(encoding='utf-8', allow_empty=False),
        validator=colander.Length(max=154)
    )
    username = colander.SchemaNode(
        colander.String(encoding='utf-8', allow_empty=False),
        validator=colander.Length(max=154)
    )
    password = colander.SchemaNode(
        colander.String(),
        validator=colander.Length(min=8),
        widget=deform.widget.CheckedPasswordWidget(redisplay=True)
    )

def get_user_registration_form(
        request,
        button_name='submit',
        use_ajax=True
):
    schema = UserRegistrationSchema().bind(request=request)
    form = deform.Form(
        schema,
        use_ajax=use_ajax,
        buttons=(button_name,)
    )
    return form
