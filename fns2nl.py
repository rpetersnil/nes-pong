#!/usr/bin/env python3

import sys, re

with open(sys.argv[1], 'r') as fnsfile:
    for line in fnsfile:
        if ';' not in line:
            parts = re.split(r'\s*=\s*', line.strip())
            print('%s#%s#' % (parts[1], parts[0]))
