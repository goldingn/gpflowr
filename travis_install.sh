#!/bin/bash

# Based on installation script from tensorflow R package travis build

# Python dependencies
sudo -H pip install --upgrade pip
sudo -H pip install numpy
# tensorflow for separate os
if [ ${TRAVIS_OS_NAME} == "linux" ]; then
  export TF_BINARY_URL=https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.11.0rc0-cp27-none-linux_x86_64.whl
fi
if [ ${TRAVIS_OS_NAME} == "osx" ]; then
  export TF_BINARY_URL=https://storage.googleapis.com/tensorflow/mac/cpu/tensorflow-0.11.0rc0-py2-none-any.whl
fi

sudo -H pip install --upgrade $TF_BINARY_URL

