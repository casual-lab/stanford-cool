#!/bin/sh
docker run --name cool -td --mount type=bind,source==$(pwd),destination=/mnt youngyee/coolenv
