import os.path
import site

# add `lib` subdirectory as a site packages directory, so our `main` module can load
# third-party libraries.
site.addsitedir(os.path.join(os.path.dirname(__file__), 'lib'))

"""
Lambda example with external dependency
"""

import json
import logging
import requests
import asyncio
#import uvloop

logger = logging.getLogger()
logger.setLevel(logging.INFO)

from worker import (
    launch,
    terminate
)

async def fetch(url):
    logger.info(f"Fetching: {url}")
    raw = requests.get(url)
    logger.info("%s", raw)
    response = raw.json()
    logger.info("%s", response)
    return response


async def post(url, data):
    logger.info(f"Posting to: {url} with data: {data}")
    raw = requests.post(
        url,
        data=json.dumps(data),
        verify=False,
        timeout=None
    )
    logger.info("%s", raw)
    response = raw.json()
    logger.info("%s", response)
    return response


async def execute_query(query: str):
    resp = await post("http://localhost:1112/api/graphql", dict(query=query))
    if isinstance(resp, dict):
        data = resp.get('data', {})
        return data
    elif isinstance(resp, list):
        data = resp
        return data


def process(*tasks):
    #asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()
    results = loop.run_until_complete(asyncio.gather(*tasks))
    # loop.close()
    return results


# Launch Worker in the Background
launch()

def handle(event, context):
    """
    Lambda handler
    """

    logger.info("%s - %s", event, context)

    query = event.get('query', '{serverip}')
    #query = event.get('query', None)

    tasks = [
        execute_query(query)
    ]
    [data] = process(*tasks)
    metadata = dict(
        # event=event,
        msg='You have been Officially Slapped by a Py!!:-)',
    )
    logger.info("Collected Metadata: %s", metadata)
    logger.info("Collected Data: %s", data)

    response = dict(
        metadata=metadata,
        data=data,
    )

    # terminate()

    return response
