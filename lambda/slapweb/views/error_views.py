__author__ = 'outcastgeek'

import logging

from pyramid.view import (
    notfound_view_config,
    view_config,
)

logging.basicConfig()
log = logging.getLogger(__file__)

# @view_config(context=Exception, renderer='templates/error.jinja2')
# def error_view(exc, request):
#     # If the view has two formal arguments, the first is the context.
#     # The context is always available as ``request.context`` too.
#     msg = exc.args[0] if exc.args else ""
#     log.error("\n\nERROR::::\n%s\n\n" % msg)
#     request.response_status = '500 Error'
#     return dict(error='Error', message='Things do break you know ...')

@view_config(context=Exception, renderer='templates/error/500.jinja2')
def error_view(exc, request):
    # If the view has two formal arguments, the first is the context.
    # The context is always available as ``request.context`` too.
    msg = exc.args[0] if exc.args else ""
    log.error("\n\nERROR::::\n%s\n\n" % msg)
    request.response_status = '500 Error'
    return dict(error='Error', message='We\'re sorry, but something went wrong.')

@notfound_view_config(renderer='templates/error/404.jinja2')
def not_found(request):
    request.response_status = '404 Not Found'
    return dict(error='Not Found', message='Not Found, dude!')
