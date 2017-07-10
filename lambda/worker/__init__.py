
import json
import os
from cffi import FFI

ffi = FFI()

worker = ffi.dlopen(os.path.join(os.path.dirname(__file__), 'worker.so'))

ffi.cdef("""
    char* ExecuteQuery(const char *query);
""")


def executeQuery(query: str):
    queryBytes = ffi.new("char[]", query.encode())
    respBytes = worker.ExecuteQuery(queryBytes)
    resp = json.loads(ffi.string(respBytes))
    return resp['data']
