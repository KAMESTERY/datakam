
import os
import sys

from io import (
    BytesIO,
    StringIO
)

from boltons.iterutils import remap

try:
    # Python 3
    from urllib.parse import urlencode

    # Convert bytes to str, if required
    def convert_str(s):
        return s.decode('utf-8') if isinstance(s, bytes) else s
except:
    # Python 2
    from urllib import urlencode

    # No conversion required
    def convert_str(s):
        return s


def response(app, event, context):
    sr = StartResponse()
    adapted_event = massage_event(event)
    output = app(environ(adapted_event, context), sr)
    return sr.response(output)


class StartResponse:
    def __init__(self):
        self.status = 500
        self.headers = []
        self.body = StringIO()

    def __call__(self, status, headers, exc_info=None):
        self.status = status.split()[0]
        self.headers[:] = headers
        return self.body.write

    def response(self, output):
        return {
            'statusCode': str(self.status),
            'headers': dict(self.headers),
            'body': self.body.getvalue() + ''.join(map(convert_str, output)),
        }

def _drop_none(path, key, value):
    return key is not None and value is not None

def massage_event(event: dict):

    adapted_event = remap(event, visit=_drop_none)

    if 'queryStringParameters' not in adapted_event:
        adapted_event['queryStringParameters'] = {}
    if 'headers' not in adapted_event:
        adapted_event['headers'] = {
            'content_type': 'application/json',
            'host': 'someip',
            'x_forwarded_proto': 'http'
        }
    if 'body' in adapted_event:
        body = BytesIO()
        raw_body = adapted_event.get('body').encode('utf8')
        body.write(raw_body)
        body.seek(0, os.SEEK_END) # go to the end
        content_length = body.tell() # grab the end location
        adapted_event['content_length'] = content_length
        body.seek(0) # rewind to the beginning
        adapted_event['body'] = body

    return adapted_event

def environ(event, context):
    environ = {
        'REQUEST_METHOD': event['httpMethod'],
        'SCRIPT_NAME': '',
        'PATH_INFO': event['path'],
        'QUERY_STRING': urlencode(event['queryStringParameters'] or {}),
        'REMOTE_ADDR': '127.0.0.1',
        'CONTENT_LENGTH': event.get('content_length', 0),
        'HTTP': 'on',
        'SERVER_PROTOCOL': 'HTTP/1.1',
        'wsgi.version': (1, 0),
        'wsgi.input': event.get('body'),
        'wsgi.errors': sys.stderr,
        'wsgi.multithread': False,
        'wsgi.multiprocess': False,
        'wsgi.run_once': False,
    }
    headers = event.get('headers', {})
    for k, v in headers.items():
        k = k.upper().replace('-', '_')

        if k == 'CONTENT_TYPE':
            environ['CONTENT_TYPE'] = v
        elif k == 'HOST':
            environ['SERVER_NAME'] = v
        elif k == 'X_FORWARDED_FOR':
            environ['REMOTE_ADDR'] = v.split(', ')[0]
        elif k == 'X_FORWARDED_PROTO':
            environ['wsgi.url_scheme'] = v
        elif k == 'X_FORWARDED_PORT':
            environ['SERVER_PORT'] = v

        environ['HTTP_' + k] = v

    return environ
