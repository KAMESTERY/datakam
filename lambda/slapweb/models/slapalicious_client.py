
import boto3
import json
import logging
import jwt


logging.basicConfig()
log = logging.getLogger(__file__)


def invoke_gql(query: str):
    client = boto3.client('lambda')
    raw = client.invoke(
        FunctionName='slapman_slapalicious_POST',
        InvocationType='RequestResponse',
        Payload=json.dumps(
            dict(query=query)
        ))
    resp = json.loads(raw['Payload'].read())
    return resp

def token_to_userinfo(token: str):
    user_info = jwt.decode(token, verify=False)
    log.debug(f"User Info: {user_info}")
    return user_info

def token_to_userid(token: str):
    user_info = token_to_userinfo(token)
    user_id = user_info.get('UserInfo').get('User').get('UserID')
    return user_id

def token_to_usergroups(token: str):
    user_info = token_to_userinfo(token)
    user_groups = user_info.get('UserInfo').get('UserGroups')
    user_group_names = [user_group.get('Name') for user_group in user_groups]
    return user_group_names

def token_to_userprofile(token: str):
    user_info = token_to_userinfo(token)
    user_profile = user_info.get('UserInfo').get('UserProfile')
    return user_profile

def user_login(email, password :str):
    response = invoke_gql(
        'query UserLogin { userLogin(email: "%s", password: "%s") {token}}' % (email, password)
    )
    if response.get('data') is None:
        error = response.get('errorMessage')
        raise Exception(error)
    token = response.get('data').get('userLogin').get('token')
    log.debug(f"TOKEN: {token}")
    return token

def user_create(email, username, password, confirm_password: str):
    response = invoke_gql(
        'mutation UserCreate { userCreate(email: "%s", username: "%s", password: "%s", confirmPassword: "%s") }'
        % (email, username, password, confirm_password)
    )
    if response.get('data') is None:
        error = response.get('errorMessage')
        raise Exception(error)
    success = response.get('data').get('userCreate')
    return success


def userprofile_update(token, Location=None, AvatarHash=None, Name=None, Age=None, AboutMe=None, **expected_values):
    userid = token_to_userid(token)
    userprofile = token_to_userprofile(token)
    response = invoke_gql(
        'mutation UserProfileUpdate { userProfileUpdate(userId: "%s", location: "%s", avatar_hash: "%s", name: "%s", age: %s, about_me: "%s", token: "%s") {table,update{UserId,AvatarHash,Name,Age,AboutMe,Location,MemberSince}} }'
        % (
            userid,
            Location or userprofile.get('Location'),
            AvatarHash or userprofile.get('AvatarHash'),
            Name or userprofile.get('Name'),
            Age or userprofile.get('Age'),
            AboutMe or userprofile.get('AboutMe'),
            token or userprofile.get('token')
        )
    )
    if response.get('data') is None:
        error = response.get('errorMessage')
        raise Exception(error)
    success = response.get('data').get('userProfileUpdate')
    return success
