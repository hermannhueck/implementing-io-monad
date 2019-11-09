#!/bin/bash

bloopVersion="1.3.4"
sbtVersion="1.3.3"

curl -L https://github.com/scalacenter/bloop/releases/download/v$bloopVersion/install.py | python
echo "alias bloop=~/.bloop/bloop" >> ~/.bashrc
alias bloop=~/.bloop/bloop
~/.bloop/bloop server &>/dev/null &
curl -L https://piccolo.link/sbt-$sbtVersion.tgz > ~/sbt.tar.gz
tar -C ~ -xvf ~/sbt.tar.gz
alias sbt=~/sbt/bin/sbt
. ~/.bloop/bash/bloop
