#!/bin/bash

mkdir -p ~/.ssh
chown -R root:root ~/.ssh
chmod -R 0700 ~/.ssh
cp -ip /run/secrets/host_ssh_key ~/.ssh/id_rsa
chmod -R 0600 ~/.ssh

git config --global core.sshCommand "ssh -i ~/.ssh/id_rsa"

exec "$@"
