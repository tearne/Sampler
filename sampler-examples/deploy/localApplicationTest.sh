#!/bin/bash
java \
-Xmx2g \
-Dakka.remote.netty.tcp.hostname=127.0.0.1 \
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@127.0.0.1:2552 \
-Dconfig.file=application.conf \
-Dlogback.configurationFile=logback.xml \
-classpath "lib/*" \
sampler.example.abc.FlockMortality
