
import os.path
import site

# add `lib` subdirectory as a site packages directory, so our `main` module can load
# third-party libraries.
site.addsitedir(os.path.join(os.path.dirname(__file__), 'lib'))

"""
Lambda example with external dependency
"""

import logging
from logging.config import dictConfig

logging_config = dict(
    version = 1,
    formatters = {
        'f': {'format':
                  '%(asctime)s %(levelname)-5.5s [%(name)s:%(lineno)s][%(threadName)s] %(message)s'}
        # '%(asctime)s %(name)-12s %(levelname)-8s %(message)s'}
    },
    handlers = {
        'h': {'class': 'logging.StreamHandler',
              'formatter': 'f',
              'level': logging.INFO}
    },
    root = {
        'handlers': ['h'],
        'level': logging.INFO,
    },
)

dictConfig(logging_config)

import json

from worker import (
    handle_request
)

def handle(event, context):
    """
    Lambda handler
    """
    logger.info(f"{event} - {context}")

    request = event.get('query', '')
    response = json.loads(
        handle_request(request)
    )

    logger.info(f"{response}")

    return response
