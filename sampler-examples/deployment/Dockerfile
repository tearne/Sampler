FROM alpine:3.3 

USER root 

RUN \
  apk update && \
  apk upgrade && \
  apk add bash openjdk8-jre && \
  rm -rf /var/cache/apk/*
  
ENV JAVA_HOME /usr/lib/jvm/java-1.8-openjdk 

WORKDIR /workdir

COPY payload /workdir/

CMD ["sh"]