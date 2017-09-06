
import logging
import os
import subprocess

from subprocess import Popen

logger = logging.getLogger()
logger.setLevel(logging.INFO)

PORT = 8000

EXE = os.path.join(os.path.dirname(__file__), 'slapman')

def launch():
    proc = Popen(EXE, shell=True)
    pid = proc.pid
    logger.info(f"Process started: {pid}")

def terminate():
    subprocess.call(f"killall {EXE}", shell=True)
    logger.info("Process terminated")

