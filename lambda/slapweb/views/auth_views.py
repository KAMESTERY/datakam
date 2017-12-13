
try:
    from slapweb.forms.userforms import (
        get_user_registration_form
    )
except:
    from forms.userforms import (
        get_user_registration_form
    )

import colander
import deform

from pyramid.view import view_config

from pyramid.httpexceptions import HTTPFound

from pyramid.view import (
    forbidden_view_config
)
from pyramid.security import (
    remember,
    forget,
)

@view_config(route_name='login', renderer='templates/login.jinja2')
@view_config(route_name='register', renderer='templates/login.jinja2')
@forbidden_view_config(renderer='templates/login.jinja2')
def login(request):
    login_route_url = request.route_url('login')
    register_route_url = request.route_url('register')
    referrer = request.url
    session = request.session
    if referrer == login_route_url or referrer == register_route_url:
        home_route_url = request.route_url('home')
        referrer = home_route_url # never use the login from itself as came_from
        # referrer = '/' # never use the login from itself as came_from
    else:
        session.flash("You lack the credentials")
    came_from = request.params.get('came_from', referrer)
    session['came_from'] = came_from
    print(f"Came from: {came_from}")
    form = get_user_registration_form(request)
    rendered_form = None
    if 'submit' in request.POST:
        try:
            form_data = form.validate(request.POST.items())
            print(f"User Data: {form_data}")
            username = form_data['username']
            email = form_data['email']
            session.flash(f"Welcome {username}!")
            headers = remember(request, email)
            return HTTPFound(
                location=came_from,
                headers=headers
            )
        except deform.ValidationFailure as e:
            # Render a form version where errors are visible next to the fields,
            # and the submitted values are posted back
            rendered_form = e.render()
        except Exception as e:
            print(f"ERROR:::: {e}")
            # Ouch!!
            # Do a form level error message
            exc = colander.Invalid(form.widget, f"Ouch!! {e}")
            form.widget.handle_error(form, exc)

    if rendered_form is None:
        rendered_form = form.render()

    return {
        'form': rendered_form
    }
