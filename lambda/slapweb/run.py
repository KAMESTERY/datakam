
__author__ = 'outcastgeek'

import os

from waitress import serve

from pyramid.paster import setup_logging

from pyramid.config import Configurator
from pyramid_zodbconn import get_connection
# from slapweb.backend.models import appmaker


# def root_factory(request):
#     conn = get_connection(request)
#     return appmaker(conn.root())


def configure_app(settings):
    """ This function returns a Pyramid WSGI application.
    """
    with Configurator(settings=settings) as config:
        config.include('pyramid_chameleon')
        config.include('pyramid_jinja2')
        config.include('pyramid_tm')
        config.include('pyramid_retry')
        # config.include('views', route_prefix='/web')
        config.include('views')
        # config.include('pyramid_zodbconn')
        # config.set_root_factory(root_factory)
        config.add_static_view('static', 'static', cache_max_age=3600)
        return config.make_wsgi_app()


script_dir = os.path.dirname(__file__) #<-- absolute dir the script is in

# init_path = 'development.ini'
init_path = os.path.join(script_dir, 'development.ini')

setup_logging(init_path)

# settings = get_settings(init_path)

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

print(f"Settings: {settings}")

app = configure_app(settings)


if __name__ == '__main__':

    port = 6543

    serve(app, host='0.0.0.0', port=port)
