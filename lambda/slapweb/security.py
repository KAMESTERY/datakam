import bcrypt


def hash_password(password, rounds=14):
    """
    Hash a password for the first time, with a certain number of rounds
    :param password: password as bytes
    :param rounds: 14 by default
    :return: hashed_password as the hashed password
    """
    hashed_password = bcrypt.hashpw(password.encode('utf8'), bcrypt.gensalt(rounds))
    return hashed_password.decode('utf8')


def check_password_hash(password, hashed_password):
    """
    Check that a unhashed password matches one that has previously been
    :param password: password as bytes
    :param hashed_password: hashed password to compare against
    :return: valid_password as a boolean to indicate wether or not the password is valid
    """
    valid_password = bcrypt.checkpw(password.encode('utf8'), hashed_password.encode('utf8'))
    return valid_password


from pyramid.security import (
    Allow,
    Authenticated,
    Everyone,
    ALL_PERMISSIONS
)


class Group:
    ADMIN = 'admin'
    SLAPMAN_COLLABO = 'slapmancollabo'
    SLAPMAN_USER = 'user'


class RootFactory(object):
    __acl__ = [
        (Allow, Everyone, 'view'),
        (Allow, Authenticated, 'user'),
        (Allow, Group.SLAPMAN_COLLABO, ['create', 'edit']),
        (Allow, 'admin', ALL_PERMISSIONS)
    ]

    def __init__(self, request):
        self.request = request


def groupfinder(userid, request):
    try:
        from slapweb.models.userinfo import (
            User,
            UserGroup
        )
    except:
        from models.userinfo import (
            User,
            UserGroup
        )
    user = next(iter([u for u in User.query(userid)]))
    if user:
        groups = UserGroup.by_userid(user.user_id)
        group_names = [g.name for g in groups]
        return group_names
    return ['user']
