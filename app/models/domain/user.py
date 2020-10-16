
import datetime

from pydantic import EmailStr

from app.models.common import (
    ModelConfigMixin
)


EMAIL = "Email"
USERNAME = "Username"
ROLE = "Role"
CONFIRMED = "Confirmed"
PASSWORDHASH = "PasswordHash"
LASTSEEN = "LastSeen"

class User(ModelConfigMixin):
    user_id: EmailStr
    email: EmailStr
    username: str
    role: int
    confirmed: bool
    password_hash: str
    last_seen: datetime.datetime


GROUPID = "GroupID"
NAME = "Name"


class UserGroup(ModelConfigMixin):
    group_id: str
    user_id: EmailStr
    name: str


NAME = "Name"
AGE = "Age"
AVATARHASH = "AvatarHash"
LOCATION = "Location"
MEMBERSINCE = "MemberSince"
ABOUTME = "AboutMe"


class UserProfile(ModelConfigMixin):
    user_id: EmailStr
    name: str
    age: int
    avatar_hash: str
    location: str
    member_since: datetime.datetime
    aboutme: str
