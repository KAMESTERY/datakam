__author__ = 'outcastgeek'

def includeme(config):
    config.scan(__name__)
    config.add_route('home', '/home')
    config.add_route('api', '/api')
    config.add_route('login', '/login')
    config.add_route('logout', '/logout')
    config.add_route('register', '/register')
    config.add_route('profile', '/profile')
