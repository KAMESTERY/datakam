
__author__ = 'outcastgeek'

try:
    from slapweb.security import (
        groupfinder,
        RootFactory
    )
except:
    from security import (
        groupfinder,
        RootFactory
    )

import os

import deform

from waitress import serve

from pyramid.authentication import SessionAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy
from pyramid.session import SignedCookieSessionFactory

from pyramid.config import Configurator
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
              'level': logging.DEBUG}
    },
    root = {
        'handlers': ['h'],
        'level': logging.DEBUG,
    },
)

dictConfig(logging_config)

log = logging.getLogger(__name__)

# from slapweb.backend.models import appmaker


# def root_factory(request):
#     conn = get_connection(request)
#     return appmaker(conn.root())

def configure_app(settings):
    """ This function returns a Pyramid WSGI application.
    """
    authn_policy = SessionAuthenticationPolicy(callback=groupfinder, debug=True)
    authz_policy = ACLAuthorizationPolicy()
    session_factory = SignedCookieSessionFactory(
        'itsaslapmanwebdevseekreet',
        cookie_name='slapmanwebdev',
        secure=False
    )

    with Configurator(
            root_factory=RootFactory,
            settings=settings
    ) as config:
        config.include('pyramid_chameleon')
        config.include('pyramid_jinja2')
        config.include('pyramid_tm')
        config.include('pyramid_retry')
        # config.include('views', route_prefix='/web')
        config.include('views')
        # config.include('pyramid_zodbconn')
        # config.set_root_factory(root_factory)
        deform.renderer.configure_zpt_renderer()
        config.add_static_view('static_deform', 'deform:static')
        config.add_static_view('static', 'static', cache_max_age=3600)

        config.set_authentication_policy(authn_policy)
        config.set_authorization_policy(authz_policy)
        config.set_session_factory(session_factory)

        return config.make_wsgi_app()


settings = {'pyramid.reload_templates': True,
            'pyramid.reload_all': True,
            'pyramid.debug_authorization': True,
            'pyramid.debug_notfound': False,
            'pyramid.debug_routematch': True,
            'pyramid.default_locale_name': 'en',
            'pyramid.includes': ['pyramid_debugtoolbar',
                                 'pyramid_beaker'],

            'zodbconn.uri': 'file://Data.fs?connection_cache_size=20000',

            'retry.attempts': 3,

            'jinja2.filters': {'model_url': 'pyramid_jinja2.filters:model_url_filter',
                               'route_url': 'pyramid_jinja2.filters:route_url_filter',
                               'static_url': 'pyramid_jinja2.filters:static_url_filter'},
            'jinja2.globals': {'hello': 'views.ext.hello',
                               'has_cred': 'views.ext.has_cred',
                               'now': 'datetime.datetime.now'}}

log.debug(f"Settings: {settings}")

app = configure_app(settings)


if __name__ == '__main__':

    port = 6543

    serve(app, host='0.0.0.0', port=port)
