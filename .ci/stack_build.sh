#!/bin/bash
set -xeo pipefail

apt update
apt install wget -y
wget -qO- https://get.haskellstack.org/ > install-stack.sh
sh install-stack.sh || (cat install-stack.sh; false)
stack build
