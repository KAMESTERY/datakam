import os
import stat
import subprocess

from subprocess import Popen

PORT = 8088

EXE = os.path.join(os.path.dirname(__file__), 'worker-exe')

st = os.stat(EXE)
os.chmod(EXE, st.st_mode | stat.S_IEXEC)

def launch():
    proc = Popen(EXE, shell=True)
    pid = proc.pid
    print(f"Process started: {pid}")

def terminate():
    subprocess.call(f"killall {EXE}", shell=True)
    print("Process terminated")
