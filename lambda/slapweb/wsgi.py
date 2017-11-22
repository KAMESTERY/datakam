__author__ = 'outcastgeek'


import os

from pyramid.paster import setup_logging
from pyramid.config import Configurator
from pyramid.request import Request
from pyramid.url import (
    IRoutesMapper,
    _join_elements,
    get_current_registry,
    parse_url_overrides
)
# from pyramid_zodbconn import get_connection
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
        config.add_static_view('static', 'static', cache_max_age=3600)
        return config.make_wsgi_app()


script_dir = os.path.dirname(__file__) #<-- absolute dir the script is in

init_path = os.path.join(script_dir, 'development.ini')

setup_logging(init_path)

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
