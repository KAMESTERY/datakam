from pyramid.view import view_config
# from .models import MyModel

try:
    from slapweb.models.userinfo import (
        User,
        UserProfile
    )
    from slapweb.models.things import (
        Thing,
        Data
    )
except:
    from models.userinfo import (
        User,
        UserProfile
    )
    from models.things import (
        Thing,
        Data
    )


# @view_config(context=MyModel, renderer='templates/mytemplate.pt')
# @view_config(context=MyModel, renderer='templates/home.jinja2')
@view_config(
    route_name='home',
    renderer='templates/home.jinja2'
)
def my_view(request):
    for user in User.email_index.query('lambert@awesome.com'):
    # for user in User.query('Email', User.email == 'lambert@awesome.com'):
    # # for user in User.rate_limited_scan():
    # for user in User.scan():
    #     print("Scanned User: {0}".format(user))
    #     # print("Rate Limited Scanned User: {0}".format(user))
        print("User queried from index: {0}".format(user))

    for thing in Thing.rate_limited_scan():
    # for thing in Thing.scan():
        print("Scanned Thing: {0}".format(thing))
        # print("Rate Limited Scanned Thing: {0}".format(thing))

    request.response.set_cookie('CookieLB', ':-):-)(-:(-:')

    return {'project': 'Slapman Web',
            'my_list': [user.username for user in User.rate_limited_scan()]}
