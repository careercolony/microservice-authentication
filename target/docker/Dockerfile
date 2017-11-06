FROM java:latest
WORKDIR /opt/docker
ADD opt /opt
RUN ["chown", "-R", "daemon:daemon", "."]
EXPOSE 8080
USER daemon
ENTRYPOINT ["bin/neo4j-akka-http-user-authentication"]
CMD []
