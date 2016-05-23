#!/bin/bash
rm -r payload/lib
cp -r ../target/universal/stage/lib payload
ls payload/lib