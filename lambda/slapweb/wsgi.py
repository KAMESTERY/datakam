__author__ = 'outcastgeek'

try:
    from slapweb.security import groupfinder
except:
    from security import groupfinder

import os

import deform

from pyramid.authentication import SessionAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy
from pyramid.session import SignedCookieSessionFactory

from pyramid.config import Configurator
from pyramid.request import Request
from pyramid.url import (
    IRoutesMapper,
    _join_elements,
    get_current_registry,
    parse_url_overrides
)
# from pyramid_zodbconn import get_connection

import logging
from logging.config import dictConfig

logging_config = dict(
    version = 1,
    formatters = {
        'f': {'format':
                  '%(asctime)s %(levelname)-5.5s [%(name)s:%(lineno)s][%(threadName)s] %(message)s'}
        # '%(asctime)s %(name)-12s %(levelname)-8s %(message)s'}
    },
    handlers = {
        'h': {'class': 'logging.StreamHandler',
              'formatter': 'f',
              'level': logging.INFO}
    },
    root = {
        'handlers': ['h'],
        'level': logging.INFO,
    },
)

dictConfig(logging_config)

# from slapweb.backend.models import appmaker


# def root_factory(request):
#     conn = get_connection(request)
#     return appmaker(conn.root())


class LambdaRequest(Request):
    def route_url(self, route_name, *elements, **kw):
        try:
            reg = self.registry
        except AttributeError:
            reg = get_current_registry() # b/c
        mapper = reg.getUtility(IRoutesMapper)
        route = mapper.get_route(route_name)

        if route is None:
            raise KeyError('No such route named %s' % route_name)

        if route.pregenerator is not None:
            elements, kw = route.pregenerator(self, elements, kw)

        app_url, qs, anchor = parse_url_overrides(self, kw)

        path = route.generate(kw) # raises KeyError if generate fails

        if elements:
            suffix = _join_elements(elements)
            if not path.endswith('/'):
                suffix = '/' + suffix
        else:
            suffix = ''

        return app_url + '/production' + path + suffix + qs + anchor

    # def static_url(self, path, **kw):
    #     pass


def configure_app(settings):
    """ This function returns a Pyramid WSGI application.
    """

    authn_policy = SessionAuthenticationPolicy(callback=groupfinder)
    authz_policy = ACLAuthorizationPolicy()
    session_factory = SignedCookieSessionFactory(
        'itsaslapmanwebprodseekreet',
        cookie_name='slapmanwebprod',
        secure=True
    )

    with Configurator(
            request_factory=LambdaRequest,
            settings=settings
    ) as config:
        config.include('pyramid_chameleon')
        config.include('pyramid_jinja2')
        config.include('pyramid_tm')
        config.include('pyramid_retry')
        # config.include('slapweb.views', route_prefix='/web')
        config.include('slapweb.views')
        # config.include('pyramid_zodbconn')
        # config.set_root_factory(root_factory)
        deform.renderer.configure_zpt_renderer()
        config.add_static_view('static_deform', 'deform:static')
        config.add_static_view('static', 'static', cache_max_age=3600)

        config.set_authentication_policy(authn_policy)
        config.set_authorization_policy(authz_policy)
        config.set_session_factory(session_factory)

        return config.make_wsgi_app()


settings = {'pyramid.reload_templates': False,
            'pyramid.debug_authorization': False,
            'pyramid.debug_notfound': False,
            'pyramid.debug_routematch': False,
            'pyramid.default_locale_name': 'en',
            'pyramid.includes': ['pyramid_beaker'],

            'zodbconn.uri': 'file://Data.fs?connection_cache_size=20000',

            'retry.attempts': 3,

            'jinja2.filters': {'model_url': 'pyramid_jinja2.filters:model_url_filter',
                               'route_url': 'pyramid_jinja2.filters:route_url_filter',
                               'static_url': 'pyramid_jinja2.filters:static_url_filter'},
            'jinja2.globals': {'hello': 'slapweb.views.ext.hello',
                               'has_cred': 'slapweb.views.ext.has_cred',
                               'now': 'datetime.datetime.now'}}

app = configure_app(settings)
