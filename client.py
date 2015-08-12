#!/usr/bin/env python

import socket
import time


CARBON_SERVER = '0.0.0.0'
CARBON_PORT = 7645


def run():
    sock = socket.socket()
    sock.connect((CARBON_SERVER, CARBON_PORT))
    message = 'device:foobar\n'
    sock.sendall(message)
    while True:
        value = time.time() % 30 + 20
        message = 'temperature (c):thermoprobe:{}\n'.format(value)
        print 'sending message: {}'.format(message)
        sock.sendall(message)

        message = 'settable:pid1:Kp:5\n'.format(value)
        print 'sending message: {}'.format(message)
        sock.sendall(message)
        time.sleep(1)
    sock.close()


if __name__ == '__main__':
    run()
