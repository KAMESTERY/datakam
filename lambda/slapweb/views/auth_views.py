
import logging

try:
    from slapweb.forms.userforms import (
        get_user_login_form,
        get_user_registration_form
    )
    from slapweb.models.userinfo import (
        User
    )
except:
    from forms.userforms import (
        get_user_login_form,
        get_user_registration_form
    )
    from models.userinfo import (
        User
    )

import colander
import deform

from pyramid.view import (
    view_config,
    view_defaults
)

from pyramid.httpexceptions import HTTPFound

from pyramid.view import (
    forbidden_view_config
)
from pyramid.security import (
    remember,
    forget,
)

logging.basicConfig()
log = logging.getLogger(__file__)

@view_defaults(renderer='templates/login.jinja2')
class AuthViews:
    def __init__(self, request):

        self.request = request
        self.session = request.session
        self.registration_form = get_user_registration_form(request)
        self.login_form = get_user_login_form(request)

        login_route_url = request.route_url('login')
        register_route_url = request.route_url('register')
        referrer = request.url
        log.debug(f"Referrer: {referrer}")
        if referrer == login_route_url or referrer == register_route_url:
            home_route_url = request.route_url('home')
            referrer = home_route_url # never use the login from itself as came_from
            # referrer = '/' # never use the login from itself as came_from
        else:
            self.session.flash("You lack the credentials")

        self.came_from = request.route_url('home')
        # self.came_from = request.params.get('came_from', referrer) # TODO: Revisit this!!!!
        self.session['came_from'] = self.came_from
        log.debug(f"Came from: {self.came_from}")

    def process_form(self, form, login=True):
        rendered_form = None
        try:
            form_data = form.validate(self.request.POST.items())
            log.debug(f"User Data: {form_data}")
            username = form_data['username']
            email = form_data['email']
            password = form_data['password']
            if login and User.check_password(email, password):
                pass
            elif len(User.email_index.query('lambert@awesome.com')) == 0:
                pass
            self.session.flash(f"Welcome {username}!")
            headers = remember(self.request, email)
            log.info(f"Processed successfully for: {username}")
            return HTTPFound(
                location=self.came_from,
                headers=headers
            )
        except deform.ValidationFailure as e:
            log.warning(f"WARNING:::: {e}")
            # Render a form version where errors are visible next to the fields,
            # and the submitted values are posted back
            rendered_form = e.render()
        except Exception as e:
            log.error(f"ERROR:::: {e}")
            # Ouch!!
            # Do a form level error message
            exc = colander.Invalid(form.widget, f"Ouch!! {e}")
            form.widget.handle_error(form, exc)
        if rendered_form is None:
            rendered_form = form.render()
        return rendered_form

    @view_config(route_name='login', request_method='GET')
    @view_config(route_name='register', request_method='GET')
    def forms_display(self):
        return {
            'registration_form': self.registration_form.render(),
            'login_form': self.login_form.render()
        }

    @view_config(route_name='register', request_method='POST')
    def handle_registration(self):
        return {
            'registration_form': self.process_form(self.registration_form),
            'login_form': self.login_form.render()
        }

    @view_config(route_name='login', request_method='POST')
    def handle_login(self):
        return {
            'registration_form': self.registration_form.render(),
            'login_form': self.process_form(self.login_form)
        }
