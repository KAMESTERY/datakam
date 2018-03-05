
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

def user_login(email, password :str):
    token = invoke_gql(
        '{ userLogin(email: "%s", password: "%s") {token}}' % (email, password)
    ).get('data').get('userLogin').get('token')
    signed_data = jwt.decode(token, verify=False)
    log.debug(f"SIGNED_DATA: {signed_data}")
    return signed_data, token
