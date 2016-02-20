FROM debian:latest
RUN apt-get update
RUN apt-get install -y libsqlite3-0
RUN apt-get install -y libgmp10

MAINTAINER Danilaw
EXPOSE 9090
ADD cynoup-router-server /srv/cynoup-router-server
ADD sqlite-latest.sqlite /srv/neweden.sqlite

ENTRYPOINT ["/srv/cynoup-router-server"]
CMD ["-f", "/srv/neweden.sqlite"]
