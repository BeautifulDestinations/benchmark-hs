#!/bin/bash

cd "$(dirname "$0")"

if [ ! -d node_modules/browserify ] || [ ! -d node_modules/benchmark ]; then
	echo "Please run 'npm install' first."
	exit
fi

$(npm bin)/browserify jsbits/benchmark-wrapper.js -o jsbits/benchmark-out.js
