
import os.path
import site

# add `lib` subdirectory as a site packages directory, so our `main` module can load
# third-party libraries.
site.addsitedir(os.path.join(os.path.dirname(__file__), 'lib'))

import pprint
import logging

# from boltons.iterutils import remap

from slapweb import adapter_wsgi
from slapweb.wsgi import app

pp = pprint.PrettyPrinter(indent=4)

logger = logging.getLogger()
logger.setLevel(logging.INFO)

# from worker import (
#     launch,
#     terminate
# )
#
# # Launch Worker in the Background
# launch()

def handle(event, context):
    # logger.info("Event: %s - Context: %s", pp.pformat(event), pp.pformat(context))
    # terminate()

    response = adapter_wsgi.response(app, event, context)

    # logger.info("Response::::\n %s", pp.pformat(response))
    return response
