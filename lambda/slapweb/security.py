import bcrypt

from pyramid.security import (
    Allow,
    Authenticated,
    Everyone,
    ALL_PERMISSIONS
)


class RootFactory(object):
    __acl__ = [(Allow, 'admin', ALL_PERMISSIONS),
               (Allow, Everyone, 'view'),
               (Allow, Authenticated, 'edit'),
               (Allow, Authenticated, 'user'),
               (Allow, Authenticated, 'employee'),
               ]

    def __init__(self, request):
        pass


def hash_password(password: bytes, rounds=14):
    """
    Hash a password for the first time, with a certain number of rounds
    :param password: password as bytes
    :param rounds: 14 by default
    :return: hashed_password as the hashed password
    """
    hashed_password = bcrypt.hashpw(password, bcrypt.gensalt(rounds))
    return hashed_password


def check_password_hash(password: bytes, hashed_password: bytes):
    """
    Check that a unhashed password matches one that has previously been
    :param password: password as bytes
    :param hashed_password: hashed password to compare against
    :return: valid_password as a boolean to indicate wether or not the password is valid
    """
    valid_password = bcrypt.checkpw(password, hashed_password)
    return valid_password
