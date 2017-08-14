import json
import os
from cffi import FFI

ffi = FFI()

slapman = ffi.dlopen(os.path.join(os.path.dirname(__file__), 'slapman.so'))

ffi.cdef("""
    int fibo(int n);
""")

def fibo(n: int):
    return slapman.fibo(n)
