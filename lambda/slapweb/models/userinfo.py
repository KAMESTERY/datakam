
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
        users = [u for u in cls.query(email)]
        if len(users) < 1:
            return False, None
        user = next(iter(users))
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
            last_seen = current_timestamp
        )
        user.password = password
        return (
            user.save(),
            UserProfile.create(
                user_id=email,
                name=username,
                member_since=current_timestamp
            ),
            UserGroup.create(user_id=email)
        )

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
    location = UnicodeAttribute(attr_name='Location', default='Somewhere Nice')
    location_index = LocationIndex()
    avatar_hash = UnicodeAttribute(attr_name='AvatarHash', default='23948esdfaouyhuihasd')
    avatar_hash_index = AvatarHashIndex()
    name = UnicodeAttribute(attr_name='Name')
    name_index = NameIndex()
    age = NumberAttribute(attr_name='Age', default=0)
    about_me = UnicodeAttribute(attr_name='AboutMe', default='About Me')
    member_since = UnicodeAttribute(attr_name='MemberSince', range_key=True)
    member_since_index = MemberSinceIndex()

    # def do_update(self, location=None, avatar_hash=None, name=None, age=None, about_me=None, member_since=None,
    #               condition=None, conditional_operator=None, **expected_values):
    #     self.update(
    #         actions=[
    #             UserProfile.location.set(location or self.location),
    #             UserProfile.avatar_hash.set(avatar_hash or self.avatar_hash),
    #             UserProfile.name.set(name or self.name),
    #             UserProfile.age.set(age or self.age),
    #             UserProfile.about_me.set(about_me or self.about_me),
    #             # UserProfile.member_since.set(member_since or self.member_since) # TODO: Should this be user editable????
    #         ],
    #         condition=condition)

    def do_update(self, location=None, avatar_hash=None, name=None, age=None, about_me=None, member_since=None,
                  condition=None, conditional_operator=None, **expected_values):
        self.update(dict(
            location=dict(value=location, action='PUT'),
            avatar_hash=dict(value=avatar_hash, action='PUT'),
            name=dict(value=name, action='PUT'),
            age=dict(value=age, action='PUT'),
            about_me=dict(value=about_me, action='PUT'),
            # member_since=dict(value=member_since, action='PUT')
        ))

    @classmethod
    def by_userid(cls, userid):
        profiles = [p for p in cls.query(userid)]
        profile = None
        if len(profiles) > 0:
            profile = next(iter(profiles))
        return profile

    @classmethod
    def create(cls, user_id=None, name=None, member_since=None):
        user_profile = UserProfile(
            user_id=user_id,
            name=name,
            member_since=member_since
        )
        return user_profile.save()

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

    @classmethod
    def create(cls, user_id=None, group_id='user', name='user'):
        user_group = UserGroup(
            group_id=group_id,
            user_id=user_id,
            name=name
        )
        return user_group.save()
