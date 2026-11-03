#!/bin/bash

set -e 
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $SCRIPT_DIR
# INSTALL_BRANCH is a git tag/branch on GitHub; override with: INSTALL_BRANCH=x ./deploy.sh
INSTALL_BRANCH="${INSTALL_BRANCH:-1.12.0r1}"

echo "Building OasisUI from '$INSTALL_BRANCH'"

# build
#docker build -f docker/Dockerfile.oasisui_proxy -t coreoasis/oasisui_proxy .
docker build --no-cache --progress=plain -f docker/Dockerfile.oasisui_app --build-arg REF_BRANCH=$INSTALL_BRANCH -t coreoasis/oasisui_app .

# run
docker network create shiny-net
docker-compose up -d
