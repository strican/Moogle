#!/usr/bin/env python
# CS 51 Tools
# Line-length check script

import sys

MAX_LINE_LEN = 80
error_code = 0

def check(filename):
    global error_code
    try:
        f = open(filename)
    except:
        print >> sys.stderr, 'Failed to open "%s" for reading' % filename
        error_code = 2
        return
    for (i, l) in enumerate(f):
        s = l[:-1]  # Strip trailing newline.
        n = len(s)
        if n > MAX_LINE_LEN:
            error_code = 1
            print ('%s:%d (%d > %d): %s' %
                   (filename, i + 1, n, MAX_LINE_LEN, s))

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 1:
        print >> sys.stderr, 'Usage: check_width file [files]'
        sys.exit(255)
    for filename in sys.argv[1:]:
        check(filename)
    exit(error_code)
