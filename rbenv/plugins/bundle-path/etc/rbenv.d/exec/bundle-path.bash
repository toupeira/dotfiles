#!/usr/bin/env bash
ruby_abi=$( echo "$RBENV_VERSION" | cut -d. -f1-2 ).0
export BUNDLE_PATH=".bundle/ruby/$ruby_abi"
