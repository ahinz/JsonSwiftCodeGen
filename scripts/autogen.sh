#!/usr/bin/env bash

swiftc -dump-ast $1 2>&1 | java -jar target/extract-struct-0.1.0-SNAPSHOT-standalone.jar > $2
