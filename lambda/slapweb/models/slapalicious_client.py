
import boto3
import json

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
