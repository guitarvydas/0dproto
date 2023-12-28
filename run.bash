#!/bin/bash
make -s 2>&1 | grep -v 'ignoring duplicate'
