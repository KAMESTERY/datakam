
from datetime import datetime

try:
    from slapweb.security import (
        hash_password,
        check_password_hash
    )
except:
    from security import (
        hash_password,
        check_password_hash
    )

from pyramid.security import (
    Allow,
    ALL_PERMISSIONS
)

from pynamodb.attributes import (
    BooleanAttribute,
    NumberAttribute,
    UnicodeAttribute,
    UTCDateTimeAttribute
)

from . import (
    PartialModel,
    PartialIndex
)

################ User Model and Indices

class EmailIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'EmailIndex'
    email = UnicodeAttribute(attr_name='Email', hash_key=True)
    confirmed = BooleanAttribute(attr_name='Confirmed', range_key=True)


class RoleIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'RoleIndex'
    role = NumberAttribute(attr_name='Role', hash_key=True)
    confirmed = BooleanAttribute(attr_name='Confirmed', range_key=True)


class UsernameIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'UsernameIndex'
    username = UnicodeAttribute(attr_name='Username', hash_key=True)


class PasswordHashIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'PasswordHashIndex'
    password_hash = UnicodeAttribute(attr_name='PasswordHash', hash_key=True)


class LastSeenIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'LastSeenIndex'
    last_seen = UnicodeAttribute(attr_name='LastSeen', hash_key=True)
    email = UnicodeAttribute(attr_name='Email', range_key=True)


class User(PartialModel):
    """
    A DynamoDB Slapman User
    """
    class Meta(PartialModel.Meta):
        table_name = "User"
    user_id = UnicodeAttribute(attr_name='UserID', hash_key=True)
    email = UnicodeAttribute(attr_name='Email', range_key=True)
    email_index = EmailIndex()
    username = UnicodeAttribute(attr_name='Username')
    username_index = UsernameIndex()
    roles = NumberAttribute(attr_name='Roles', default=0)
    role_index = RoleIndex()
    confirmed = BooleanAttribute('Confirmed', default=False)
    password_hash = UnicodeAttribute(attr_name='PasswordHash')
    password_hash_index = PasswordHashIndex()
    last_seen = UnicodeAttribute(attr_name='LastSeen')
    # last_seen = UTCDateTimeAttribute(attr_name='LastSeen', default=datetime.now())
    last_seen_index = LastSeenIndex()

    @property
    def __acl__(self):
        return [
            (Allow, 'admin', ALL_PERMISSIONS),
            (Allow, self.username, ['view', 'edit']),
        ]

    def update_last_seen(self):
        self.last_seen = User.get_current_timestamp()
        self.save()

    def _get_password(self):
        return self.password_hash

    def _set_password(self, password):
        self.password_hash = hash_password(password)

    password = property(_get_password, _set_password)

    @classmethod
    def check_password(cls, email, password):
        user = next(iter([u for u in cls.query(email)]))
        if not user:
            return False, None
        return check_password_hash(password, user.password), user

    @classmethod
    def check_password(cls, email, password):
        user = next(iter([u for u in cls.query(email)]))
        if not user:
            return False, None
        return check_password_hash(password, user.password), user

    @classmethod
    def create(cls, email=None, password=None, username=None):
        current_timestamp = User.get_current_timestamp()
        user = User(
            user_id = email,
            email = email,
            username = username,
            last_seen =current_timestamp
        )
        user.password = password
        return user.save()

    @classmethod
    def get_current_timestamp(cls):
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


################ UserProfile Model and Indices

class LocationIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'LocationIndex'
    location = UnicodeAttribute(attr_name='Location', hash_key=True)
    age = UnicodeAttribute(attr_name='Age', range_key=True)


class AvatarHashIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'AvatarHashIndex'
    avatar_hash = UnicodeAttribute(attr_name='AvatarHash', hash_key=True)
    age = UnicodeAttribute(attr_name='Age', range_key=True)


class NameIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'NameIndex'
    name = UnicodeAttribute(attr_name='Name', hash_key=True)
    about_me = UnicodeAttribute(attr_name='AboutMe', range_key=True)


class MemberSinceIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'MemberSinceIndex'
    member_since = UnicodeAttribute(attr_name='MemberSince', hash_key=True)
    location = UnicodeAttribute(attr_name='Location', range_key=True)


class UserProfile(PartialModel):
    """
    A DynamoDB Slapman UserProfile
    """
    class Meta(PartialModel.Meta):
        table_name = "UserProfile"
    user_id = UnicodeAttribute(attr_name='UserID', hash_key=True)
    location = UnicodeAttribute(attr_name='Location', range_key=True)
    location_index = LocationIndex()
    avatar_hash = UnicodeAttribute(attr_name='AvatarHash')
    avatar_hash_index = AvatarHashIndex()
    name = UnicodeAttribute(attr_name='Name')
    name_index = NameIndex()
    age = NumberAttribute(attr_name='Age')
    about_me = UnicodeAttribute(attr_name='AboutMe')
    member_since = UnicodeAttribute(attr_name='MemberSince')
    member_since_index = MemberSinceIndex()

################ UserGroup Model and Indices

class UserIDIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'UserIDIndex'
    user_id = UnicodeAttribute(attr_name='UserID', hash_key=True)
    name = UnicodeAttribute(attr_name='Name', range_key=True)

class GroupNameIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'NameIndex'
    name = UnicodeAttribute(attr_name='Name', hash_key=True)
    user_id = UnicodeAttribute(attr_name='UserID', range_key=True)


class UserGroup(PartialModel):
    """
    A DynamoDB Slapman Thing
    """
    class Meta(PartialModel.Meta):
        table_name = "UserGroups"
    group_id = UnicodeAttribute(attr_name='GroupID', hash_key=True)
    user_id = UnicodeAttribute(attr_name='UserID', range_key=True)
    user_id_index = UserIDIndex()
    name = UnicodeAttribute(attr_name='Name')
    name_index = GroupNameIndex()

    @property
    def __acl__(self):
        return [
            (Allow, 'admin', ALL_PERMISSIONS),
        ]

    @classmethod
    def by_userid(cls, userid):
        groups = [g for g in cls.user_id_index.query(userid)]
        return groups
