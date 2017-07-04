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

logger = logging.getLogger()
logger.setLevel(logging.INFO)

from datagql.data import setup
from datagql.schema import schema

async def fetch(url):
    logger.info("Fetching: %s", url)
    raw = requests.get(url)
    logger.info("%s", raw)
    response = raw.json()
    return response

async def execute_query(query):
    setup()
    result = schema.execute(query)
    logger.info("Result: %s", result)
    response = result.data
    logger.info("Response: %s", response)
    return response

def process(*tasks):
    loop = asyncio.get_event_loop()
    results = loop.run_until_complete(asyncio.gather(*tasks))
    #loop.close()
    return results

def handle(event, context):
    """
    Lambda handler
    """
    logger.info("%s - %s", event, context)

    ipurl = "https://api.ipify.org/?format=json"
    weatherurl = "http://samples.openweathermap.org/data/2.5/weather?id=2172797&appid=b1b15e88fa797225412429c1c50c122a1"

    result = None
    if 'query' in event:
        tasks = fetch(ipurl), fetch(weatherurl), execute_query(event['query'])
        ipresult, weatherresult, result = process(*tasks)
    else:
        tasks = fetch(ipurl), fetch(weatherurl)
        ipresult, weatherresult = process(*tasks)

    ip = ipresult['ip']
    weather = weatherresult['weather']

    logger.info("Lambda IP: %s", ip)
    logger.info("Lambda Weather: %s", weather)

    response = dict(
        metadata=dict(
            event=event,
            msg='You have been Officially Slapped by a Py!!:-)',
            ip=ip,
            weather=weather
        ),
        data=result
    )

    return response
