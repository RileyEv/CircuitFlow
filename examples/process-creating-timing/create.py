import time, sys

NUM_RANGE = 100000000

from multiprocessing import Process
import threading


def timefunc(f):
    t = time.time()
    f()
    return time.time() - t


def multiprocess():
    class MultiProcess(Process):
        def __init__(self):
            Process.__init__(self)

        def run(self):
            # # Alter string + test processing speed
            # for i in xrange(NUM_RANGE):
            #     a = 20 * 20
            return

    for _ in range(8000):
        MultiProcess().start()


def multithreading():
    class MultiThread(threading.Thread):
        def __init__(self):
            threading.Thread.__init__(self)

        def run(self):
            # Alter string + test processing speed
            # for i in xrange(NUM_RANGE):
            #     a = 20 * 20
            return

    for _ in range(8000):
        MultiThread().start()
