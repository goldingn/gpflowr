#!/bin/bash

# Based on installation script from tensorflow R package travis build

# Python dependencies
sudo -H pip install --upgrade pip
sudo -H pip install numpy
# tensorflow for separate os
if [ ${TRAVIS_OS_NAME} == "linux" ]; then
  sudo -H pip install https://ci.tensorflow.org/view/Nightly/job/nightly-matrix-cpu/TF_BUILD_IS_OPT=OPT,TF_BUILD_IS_PIP=PIP,TF_BUILD_PYTHON_VERSION=PYTHON2,label=cpu-slave/lastSuccessfulBuild/artifact/pip_test/whl/tensorflow-0.10.0-cp27-none-linux_x86_64.whl
fi
if [ ${TRAVIS_OS_NAME} == "osx" ]; then
  sudo -H pip install https://ci.tensorflow.org/view/Nightly/job/nightly-matrix-cpu/TF_BUILD_IS_OPT=OPT,TF_BUILD_IS_PIP=PIP,TF_BUILD_PYTHON_VERSION=PYTHON2,label=mac1-slave/lastSuccessfulBuild/artifact/pip_test/whl/tensorflow-0.10.0-py2-none-any.whl
fi

