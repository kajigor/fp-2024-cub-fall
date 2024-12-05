#!/usr/bin/env bash
set -euo pipefail

mkdir -p tmp
pushd tmp
wget https://github.com/barrust/pyspellchecker/raw/refs/heads/master/spellchecker/resources/en.json.gz
gzip -d en.json.gz
mv en.json ../dictionary.json
popd
rm -rf tmp
