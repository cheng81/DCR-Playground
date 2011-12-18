#!/bin/bash
PEGJS='../node_modules/pegjs/bin/pegjs'
SRC='./dcrde.pegjs'
OUT='../lib/dcrde.js'

${PEGJS} ${SRC} ${OUT}