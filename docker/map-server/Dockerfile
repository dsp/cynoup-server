FROM debian:latest
RUN apt-get update
RUN apt-get install -y libsqlite3-0
RUN apt-get install -y libgmp10

MAINTAINER Danilaw
EXPOSE 9091
ADD cynoup-map-server /srv/cynoup-map-server
ADD sqlite-latest.sqlite /srv/neweden.sqlite

ENTRYPOINT ["/srv/cynoup-map-server"]
CMD ["-f", "/srv/neweden.sqlite"]
