from pyramid.view import view_config
# from .models import MyModel


# @view_config(context=MyModel, renderer='templates/mytemplate.pt')
# @view_config(context=MyModel, renderer='templates/home.jinja2')
@view_config(
    route_name='home',
    renderer='templates/home.jinja2'
)
def my_view(request):
    return {'project': 'Slapman Web',
            'my_list': [0, 1, 2, 3, 4, 5]}
