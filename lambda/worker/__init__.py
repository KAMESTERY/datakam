import os

from ctypes import cdll

workerext = cdll.LoadLibrary(
    os.path.join(os.path.dirname(__file__), 'libworkerext.so')
)

def handle_request(req):
    resp = workerext.workerext(req)
    return resp
