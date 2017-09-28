
import time
import logging
import os
import subprocess

from subprocess import Popen

logger = logging.getLogger()
logger.setLevel(logging.INFO)

PORT = 8000

EXE = os.path.join(os.path.dirname(__file__), 'slapman')
OPT = "serve"

def launch():
    proc = Popen(EXE + " " + OPT, shell=True)
    logger.info("The Worker Command is: {}".format(proc.args))

    # Wait for 1 seconds
    time.sleep(1)

    pid = proc.pid
    logger.info(f"Process started: {pid}")

def terminate():
    subprocess.call(f"killall {EXE}", shell=True)
    logger.info("Process terminated")

