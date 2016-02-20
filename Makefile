THRIFT?=thrift/compiler/cpp/thrift
interfaces=if/NewEden.thrift if/NewEdenMapService.thrift if/NewEdenRoutingService.thrift

all: thrift
	stack build

thrift: $(interfaces)
	echo $(interfaces) | xargs -n1 $(THRIFT) --gen py 
	echo $(interfaces) | xargs -n1 $(THRIFT) --gen hs
	echo $(interfaces) | xargs -n1 $(THRIFT) --gen php

docker-images:
	stack install --local-bin-path=docker/routing-server
	stack install --local-bin-path=docker/map-server
	ln sqlite-latest.sqlite  docker/routing-server
	ln sqlite-latest.sqlite  docker/map-server
	docker build -t cynoup-routing-server:latest docker/routing-server
	docker build -t cynoup-map-server:latest docker/map-server

.PHONY: thrift, all
