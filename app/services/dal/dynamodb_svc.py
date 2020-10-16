
import asyncio
import boto3

from botocore.exceptions import ClientError
from loguru import logger


async def put_item(tbl_name: str, item: dict, dynamodb = None):
    if not dynamodb:
        dynamodb = boto3.resource('dynamodb')

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


async def get_item(tbl_name: str, key: dict, dynamodb: None):
    if not dynamodb:
        dynamodb = boto3.resource('dynamodb')

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
        logger.error(f"Unknown error while updating item: {error.response['Error']['Message']}")
        raise error


async def delete_item(tbl_name: str, key: dict, dynamodb: None):
    if not dynamodb:
        dynamodb = boto3.resource('dynamodb')

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
        logger.error(f"Unknown error while updating item: {error.response['Error']['Message']}")
        raise error


async def update_item(tbl_name: str, key: dict, new_item: dict, dynamodb: None):
    if not dynamodb:
        dynamodb = boto3.resource('dynamodb')

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
