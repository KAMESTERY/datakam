__author__ = 'outcastgeek'

from pyramid.renderers import render
from pyramid.security import has_permission

def hello(name):
    greeting = "Hello %s!!!!" % name
    response = render('templates/admin/hello.jinja2', dict(greeting=greeting))
    return response

def has_cred(permission, request):
    return has_permission(permission, request.context, request)
