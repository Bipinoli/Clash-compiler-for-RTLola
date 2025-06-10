#!/bin/sh

python3 setup.py

# run container by mounting the diretory to /thesis in container
docker build -t thesis .
docker run -v .:/thesis thesis