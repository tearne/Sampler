#!/bin/bash

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

cd $DIR

java \
-Xmx2g \
-Dakka.remote.netty.tcp.hostname=127.0.0.1 \
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@127.0.0.1:2552 \
-Dconfig.file=application.conf \
-Dlogback.configurationFile=logback.xml \
-classpath "lib/*" \
"$@" \
sampler.example.abc.UnfairCoin
