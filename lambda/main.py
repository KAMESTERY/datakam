import os.path
import site

# add `lib` subdirectory as a site packages directory, so our `main` module can load
# third-party libraries.
site.addsitedir(os.path.join(os.path.dirname(__file__), 'lib'))

"""
Lambda example with external dependency
"""

import logging
import requests
import asyncio
import uvloop

logger = logging.getLogger()
logger.setLevel(logging.INFO)

from worker import (
    launch,
    terminate
)

from datagql.data import setup
from datagql.schema import execute_query_async

async def fetch(url):
    logger.info("Fetching: %s", url)
    raw = requests.get(url)
    logger.info("%s", raw)
    response = raw.json()
    logger.info("%s", response)
    return response


async def execute_query(query: str):
    setup()
    result = await execute_query_async(query)
    logger.info("Result: %s", result)
    response = result.data
    logger.info("Response: %s", response)
    return response


def process(*tasks):
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()
    results = loop.run_until_complete(asyncio.gather(*tasks))
    # loop.close()
    return results


launch()

def handle(event, context):
    """
    Lambda handler
    """
    logger.info("%s - %s", event, context)

    tasks = [
        execute_query("""
                        query CollectMetadata {
                            currentip
                            weather {
                                location
                                description
                                temp
                                pressure
                                humidity
                                speed
                                deg
                            }
                        }
                        """),
        execute_query(event.get('query', None)),
        fetch("http://localhost:8000/operations/19/7924")
    ]
    [m, data, slap] = process(*tasks)
    metadata = dict(
        event=event,
        msg='You have been Officially Slapped by a Py!!:-)',
        ip=m['currentip'],
        weather=m['weather']
    )
    logger.info("Collected Metadata: %s", metadata)
    logger.info("Collected Data: %s", data)
    logger.info("Collected Slap: %s", slap)

    response = dict(
        metadata=metadata,
        data=data,
        slap=slap
    )

    # terminate()

    return response
