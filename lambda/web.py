
import os.path
import site

# add `lib` subdirectory as a site packages directory, so our `main` module can load
# third-party libraries.
site.addsitedir(os.path.join(os.path.dirname(__file__), 'lib'))

import logging
import awsgi

from flask import (
    Flask,
    render_template,
)

logger = logging.getLogger()
logger.setLevel(logging.INFO)

app = Flask(__name__)


@app.route('/')
def index():
    return render_template('home.jinja2', my_string="Wheeeee!", my_list=[0,1,2,3,4,5])


from worker import (
    launch,
    terminate
)

# Launch Worker in the Background
launch()


def handle(event, context):
    logger.info("%s - %s", event, context)
    # terminate()
    response = awsgi.response(app, event, context)
    logger.info("Response::::\n %s", response)
    return response
