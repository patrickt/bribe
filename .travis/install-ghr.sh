#!/bin/sh
set -o errexit -o verbose

if test ! "$BUILD_BINARY" || test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
else
    if [ "$TRAVIS_OS_NAME" = "linux" ]
    then
        ARCH="linux"
    else
        ARCH="darwin"
    fi
  echo "Installing ghr"
  URL="https://github.com/tcnksm/ghr/releases/download/v0.5.4/ghr_v0.5.4_${ARCH}_386.zip"
  curl -L ${URL} > /tmp/ghr.zip
  unzip /tmp/ghr.zip -d "$HOME/.local/bin"
  rm /tmp/ghr.zip
fi
