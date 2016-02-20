CynoUp Router
=============

CynoUp Router is a routing server for New Eden, the galaxy in which the MMO
Eve Online takes place. The server allows querying for shortest paths and
various other routing features. In addition the server allows to provide
new connections within the universe. This can be useful to provide wormhole
routing.

The main goal of the project is to provide a very fast, easy to use routing
interface for other applications.

Install & Run
-------
You need the static dump of EVE Online as an SQLite3 database. You can obtain
it [here](https://www.fuzzwork.co.uk/dump/).
Install [stack](https://github.com/commercialhaskell/stack).

    $ git clone https://bitbucket.org/tiransol/cynoup-server
    $ cd cynoup-router
    $ stack build
    $ stack exec cynoup-router-server -- -f neweden-latest.sqlite

Build your own bindings
----------------------
CynoUp uses Thrift as it's server side interface. Thrift is a fast and
versioned binary protocol. In order to implement your a client, generate the
client code via the thrift command line interface for the language of your
choice. For more information see https://thrift.apache.org.

*Example*:

    $ thrift --gen py cynoup-router/if/NewEdenRoutingService.thrift

You can then use the generated bindings to RPC call a service.

Copyright
---------
(c) 2016 Danilaw

Licensed under the terms of the GNU General Public License 3.0
