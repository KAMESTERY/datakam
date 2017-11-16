__author__ = 'outcastgeek'


import os


from pyramid.paster import (
    get_app,
    get_appsettings,
    get_config_loader,
    setup_logging
)

from plaster.loaders import get_settings

from pyramid.config import Configurator
from pyramid_zodbconn import get_connection
# from slapweb.backend.models import appmaker


def root_factory(request):
    conn = get_connection(request)
    return appmaker(conn.root())


def configure_app(settings):
    """ This function returns a Pyramid WSGI application.
    """
    with Configurator(settings=settings) as config:
        config.include('pyramid_chameleon')
        config.include('pyramid_jinja2')
        config.include('pyramid_tm')
        config.include('pyramid_retry')
        # config.include('pyramid_zodbconn')
        # config.set_root_factory(root_factory)
        config.add_static_view('static', 'static', cache_max_age=3600)
        config.add_route('home', '/web')
        config.scan('slapweb.backend')
        return config.make_wsgi_app()


script_dir = os.path.dirname(__file__) #<-- absolute dir the script is in

# init_path = 'development.ini'
init_path = os.path.join(script_dir, 'development.ini')

setup_logging(init_path)

# settings = get_settings(init_path)

settings = {'pyramid.reload_templates': True,
            'pyramid.debug_authorization': False,
            'pyramid.debug_notfound': False,
            'pyramid.debug_routematch': False,
            'pyramid.default_locale_name': 'en',
            'pyramid.includes': ['pyramid_debugtoolbar'],

            'zodbconn.uri': 'file://Data.fs?connection_cache_size=20000',

            'retry.attempts': 3}

print(f"Settings: {settings}")

app = configure_app(settings)

# app = get_app(init_path, 'main')
