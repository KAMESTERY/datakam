
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

logger = logging.getLogger()
logger.setLevel(logging.INFO)

from worker import (
    launch,
    terminate,
    PORT
)

launch()

def handle(event, context):
    """
    Lambda handler
    """
    logger.info(f"{event} - {context}")

    response = requests.post(f"http://localhost:{PORT}/graphql", json=event)

    # terminate()

    json = response.json()

    logger.info(f"{json}")

    return json
