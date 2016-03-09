FROM debian:latest
RUN apt-get update
RUN apt-get install -y libsqlite3-0
RUN apt-get install -y libgmp10

MAINTAINER Danilaw
EXPOSE 60001
ADD cynoup-router-server /srv/cynoup-router-server
ADD sqlite-latest.sqlite /srv/neweden.sqlite

ENTRYPOINT ["/srv/cynoup-router-server"]
CMD ["-f", "/srv/neweden.sqlite", "-p", "60001"]
