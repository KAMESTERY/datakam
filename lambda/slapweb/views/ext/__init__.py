__author__ = 'outcastgeek'

try:
    from slapweb.models.slapalicious_client import (
        token_to_userid
    )
except:
    from models.slapalicious_client import (
        token_to_userid
    )



from pyramid.renderers import render
# from pyramid.security import has_permission

def hello(name):
    greeting = "Hello %s!!!!" % name
    response = render('templates/admin/hello.jinja2', dict(greeting=greeting))
    return response

def has_cred(permission, request):
    return request.has_permission(permission, request.context)
    # return has_permission(permission, request.context, request)

def user_id(request):
    token = request.authenticated_userid
    user_id = token_to_userid(token)
    return user_id

