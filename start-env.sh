#!/bin/sh
docker run --name cool -td --rm --mount type=bind,source==$(pwd),destination=/mnt youngyee/coolenv
