

from formencode import Schema
from formencode.validators import (
    NotEmpty,
    Bool)

from pyramid.renderers import render
from pyramid_simpleform import Form
from pyramid_simpleform.renderers import FormRenderer

class UserAttributesSchema(Schema):
    allow_extra_fields = True
    filter_extra_fields = True
    username = NotEmpty
    project = NotEmpty
    active = Bool
