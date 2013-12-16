#!/bin/bash
java \
-Xmx3g \
-Dakka.remote.netty.tcp.hostname=127.0.0.1 \
-Dakka.cluster.seed-nodes.0=akka.tcp://ABCSystem@127.0.0.1:2552 \
-Dconfig.file=application.conf \
-Dlogback.configurationFile=logback.xml \
-classpath "*.jar" \
sampler.example.abc.ClusteredUnfairCoin
