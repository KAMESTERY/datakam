
import os.path
import site

# add `lib` subdirectory as a site packages directory, so our `main` module can load
# third-party libraries.
site.addsitedir(os.path.join(os.path.dirname(__file__), 'lib'))

import pprint
import logging
import awsgi
# import html

from boltons.iterutils import remap

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


drop_none = lambda path, key, value: key is not None and value is not None

def handle(event, context):
    logger.info("Event: %s - Context: %s", pp.pformat(event), pp.pformat(context))
    # terminate()

    clean_event = remap(event, visit=drop_none)

    if 'queryStringParameters' not in clean_event: clean_event['queryStringParameters'] = {}
    if 'headers' not in clean_event: clean_event['headers'] = {'content_type': 'application/json', 'host': 'someip', 'x_forwarded_proto': 'http'}

    # logger.info("Clean Event: %s", pp.pformat(clean_event))

    response = awsgi.response(app, clean_event, context)
    # response = awsgi.response(app, event, context)
    # res = awsgi.response(app, event, context)
    # response = dict(
    #     statusCode= res.get('statusCode'),
    #     headers= res.get('headers'),
    #     body= html.escape(res.get('body'), quote=True)
    # )
    # logger.info("Response::::\n %s", pp.pformat(response))
    return response
