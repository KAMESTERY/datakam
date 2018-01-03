
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
            username = form_data.get('username', None)
            email = form_data.get('email', None)
            password = form_data.get('password', None)
            if login:
                success, user = User.check_password(email, password)
                if success:
                    log.info(f"Successfully logged in: {username or email}")
                    user.update_last_seen()
                    log.debug(f"Logged In User: {user}")
                    return self.redirect_success(email, username)
                else:
                    err_msg = f"Could not login: {username or email}!"
                    self.session.flash(err_msg)
                    raise Exception(err_msg)
            else:
                existing_user_ids = [user.user_id for user in User.query(email)]
                user_exists = len(existing_user_ids) > 0
                if user_exists:
                    log.warning(f"Existing User IDs: {existing_user_ids}")
                    err_msg = f"Could not register: {username or email}!"
                    self.session.flash(err_msg)
                    raise Exception(err_msg)
                else:
                    User.create(
                        email=email,
                        username=username,
                        password=password
                    )
                    log.info(f"Successfully registered: {username or email}")
                    return self.redirect_success(email, username)
        except deform.ValidationFailure as e:
            log.warning(f"WARNING:::: {e}")
            # Render a form version where errors are visible next to the fields,
            # and the submitted values are posted back
            rendered_form = e.render()
        except HTTPFound as e:
            raise e
        except Exception as e:
            log.error(f"ERROR:::: {e}")
            # Ouch!!
            # Do a form level error message
            exc = colander.Invalid(form.widget, f"Ouch!! {e}")
            form.widget.handle_error(form, exc)
        if rendered_form is None:
            rendered_form = form.render()
        return rendered_form

    def redirect_success(self, email, username):
        self.session.flash(f"Welcome {username or email}!")
        headers = remember(self.request, email)
        raise HTTPFound(
            location=self.came_from,
            headers=headers
        )

    @view_config(route_name='login', request_method='GET')
    @view_config(route_name='register', request_method='GET')
    @forbidden_view_config()
    def forms_display(self):
        return {
            'registration_form': self.registration_form.render(),
            'login_form': self.login_form.render()
        }

    @view_config(route_name='register', request_method='POST')
    def handle_registration(self):
        return {
            'registration_form': self.process_form(self.registration_form, login=False),
            'login_form': self.login_form.render()
        }

    @view_config(route_name='login', request_method='POST')
    def handle_login(self):
        return {
            'registration_form': self.registration_form.render(),
            'login_form': self.process_form(self.login_form)
        }
