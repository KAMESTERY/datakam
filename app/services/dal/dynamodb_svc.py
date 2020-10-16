
import asyncio
import boto3

from contextlib import asynccontextmanager


# @asynccontextmanager
async def put_item(tbl_name: str, item: dict, dynamodb = None):
    if not dynamodb:
        dynamodb = boto3.resource('dynamodb')

    table = dynamodb.Table(tbl_name)

    loop = asyncio.get_event_loop()

    response = await loop.run_in_executor(
        None,
        lambda: table.put_item(Item=item)
    )

    return response


@asynccontextmanager
async def update_item(tbl_name: str, key: dict, expression_attribute_values: dict, dynamodb: None):
    if not dynamodb:
        dynamodb = boto3.resource('dynamodb')

    table = dynamodb.Table(tbl_name)

    loop = asyncio.get_event_loop()

    response = await loop.run_in_executor(
        None,
        table.update_item,
        **dict(
            Key=key,
            ExpressionAttributeValues=expression_attribute_values,
            ReturnValues="UPDATED_NEW"
        )
    )

    return response
