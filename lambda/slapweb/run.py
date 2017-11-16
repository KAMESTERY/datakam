
__author__ = 'outcastgeek'

from wsgi import app
from waitress import serve

if __name__ == '__main__':

    port = 6543

    serve(app, host='0.0.0.0', port=port)
