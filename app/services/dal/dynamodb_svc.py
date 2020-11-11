
import asyncio
from functools import lru_cache
from typing import (
    Dict,
    List
)

import boto3
from boto3.dynamodb.conditions import (
    And,
    Attr,
    Key
)

from botocore.exceptions import ClientError
from loguru import logger


@lru_cache(maxsize=1)
def get_dynamodb_resource():
    dynamodb = boto3.resource('dynamodb')
    return dynamodb


async def put_item(tbl_name: str, item: dict):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        loop = asyncio.get_event_loop()

        response = await loop.run_in_executor(
            None,
            lambda: table.put_item(Item=item)
        )

        return response

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while putting item: {error.response['Error']['Message']}")
        raise error


async def batch_put_item(tbl_name: str, items: List[dict]):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        responses = await put_all_items(table, items)

        return responses

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while batch putting item: {error.response['Error']['Message']}")
        raise error


async def put_all_items(table, items: List[dict]):
    loop = asyncio.get_event_loop()

    tasks = []

    with table.batch_writer() as batch:
        for item in items:
            task = loop.run_in_executor(
                None,
                batch.put_item(Item=item)
            )
            tasks.append(task)

    responses = await asyncio.wait(tasks)

    return responses


async def get_item(tbl_name: str, key: dict):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        loop = asyncio.get_event_loop()

        response = await loop.run_in_executor(
            None,
            lambda: table.get_item(
                **dict(
                    Key=key
                )
            )
        )

        return response.get('Item', dict())

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while retrieving item: {error.response['Error']['Message']}")
        raise error


async def batch_get_item(params: Dict[str, List[dict]]):

    dynamodb = get_dynamodb_resource()

    try:

        loop = asyncio.get_event_loop()

        request_items = dict(ReturnConsumedCapacity='TOTAL')

        for tbl_name in params:
            keys = params[tbl_name]
            request_items[tbl_name] = dict(
                Key=keys,
                ConsistentRead=True,
            )

        response = await loop.run_in_executor(
            None,
            lambda: dynamodb.batch_get_item(
                **dict(RequestItems=request_items)
            )
        )

        return response.get('Responses', dict())

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while batch retrieving items: {error.response['Error']['Message']}")
        raise error


async def query_by_partition(
        tbl_name: str,
        partition_name: str,
        partition_value: str,
):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        loop = asyncio.get_event_loop()

        response = await loop.run_in_executor(
            None,
            lambda: table.query(
                **dict(
                    # IndexName="UserIDIndex",
                    KeyConditionExpression=Key(partition_name).eq(partition_value),
                    ScanIndexForward=False # true = ascending, false = descending
                )
            )
        )

        logger.debug(f"Query Results: {response}")

        return response.get('Items', [])

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while querying items: {error.response['Error']['Message']}")
        raise error


async def delete_item(tbl_name: str, key: dict):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        loop = asyncio.get_event_loop()

        response = await loop.run_in_executor(
            None,
            lambda: table.delete_item(
                **dict(
                    Key=key
                )
            )
        )

        return response

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while deleting item: {error.response['Error']['Message']}")
        raise error


async def batch_delete_item(tbl_name: str, keys: List[dict]):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        responses = await delete_all_items(table, keys)

        return responses

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while batch deleting item: {error.response['Error']['Message']}")
        raise error


async def delete_all_items(table, keys: List[dict]):
    loop = asyncio.get_event_loop()

    tasks = []

    with table.batch_writer() as batch:
        for key in keys:
            task = loop.run_in_executor(
                None,
                batch.delete_item(Key=key)
            )
            tasks.append(task)

    responses = await asyncio.wait(tasks)

    return responses


async def update_item(tbl_name: str, key: dict, new_item: dict):

    dynamodb = get_dynamodb_resource()

    try:
        table = dynamodb.Table(tbl_name)

        loop = asyncio.get_event_loop()

        response = await loop.run_in_executor(
            None,
            lambda: table.update_item(
                **dict(
                    Key=key,
                    UpdateExpression=get_update_expression(new_item),
                    ExpressionAttributeValues=get_expression_attribute_values(new_item),
                    ReturnValues="UPDATED_NEW"
                )
            )
        )

        return response

    except ClientError as error:
        handle_error(error)
    except BaseException as error:
        logger.error(f"Unknown error while updating item: {error.response['Error']['Message']}")
        raise error


UPDATE_PREFIX = ":new_"


def get_update_expression(new_item: dict) -> str:
    update_expression = "SET"
    n = 0

    for key in new_item:
        if n == 0:
            update_expression += f" {key} = {UPDATE_PREFIX}{key} "
            n += 1
        else:
            update_expression += f", {key} = {UPDATE_PREFIX}{key} "

    logger.debug(f"Update Expression: {update_expression}")

    return update_expression


def get_expression_attribute_values(new_item: dict) -> dict:
    expression_attribute_values = dict()

    for k, v in new_item.items():
        expression_attribute_values[f"{UPDATE_PREFIX}{k}"] = v

    return expression_attribute_values


def handle_error(error):
    error_code = error.response['Error']['Code']
    error_message = error.response['Error']['Message']

    error_help_string = ERROR_HELP_STRINGS[error_code]

    logger.error(f"[{error_code}] {error_help_string}. Error message: {error_message}")

    raise error


ERROR_HELP_STRINGS = {
    # Operation specific errors
    'ConditionalCheckFailedException': 'Condition check specified in the operation failed, review and update the condition check before retrying',
    'TransactionConflictException': 'Operation was rejected because there is an ongoing transaction for the item, generally safe to retry with exponential back-off',
    'ItemCollectionSizeLimitExceededException': 'An item collection is too large, you\'re using Local Secondary Index and exceeded size limit of items per partition key.' +
                                                ' Consider using Global Secondary Index instead',
    # Common Errors
    'InternalServerError': 'Internal Server Error, generally safe to retry with exponential back-off',
    'ProvisionedThroughputExceededException': 'Request rate is too high. If you\'re using a custom retry strategy make sure to retry with exponential back-off.' +
                                              'Otherwise consider reducing frequency of requests or increasing provisioned capacity for your table or secondary index',
    'ResourceNotFoundException': 'One of the tables was not found, verify table exists before retrying',
    'ServiceUnavailable': 'Had trouble reaching DynamoDB. generally safe to retry with exponential back-off',
    'ThrottlingException': 'Request denied due to throttling, generally safe to retry with exponential back-off',
    'UnrecognizedClientException': 'The request signature is incorrect most likely due to an invalid AWS access key ID or secret key, fix before retrying',
    'ValidationException': 'The input fails to satisfy the constraints specified by DynamoDB, fix input before retrying',
    'RequestLimitExceeded': 'Throughput exceeds the current throughput limit for your account, increase account level throughput before retrying',
}
