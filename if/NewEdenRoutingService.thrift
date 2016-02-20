namespace cpp cynoup.routing
namespace php cynoup.routing

include "NewEden.thrift"

const i8 OPTION_PREFER_SHORTEST = 0x1
const i8 OPTION_PREFER_SAFER = 0x2
const i8 OPTION_PREFER_HIGHSEC = 0x4

typedef i32 Handler
typedef string SolarSystemName
typedef list<i32> Route

service NewEdenRouting
{
    /**
     * \brief Find the shortest route between two solar systems
     *
     * Find the shortest routes between two solar system. Additional
     * connections between solar systems can be placed. Available
     * options are OPTION_PREFER_SHORTEST, OPTION_PREFER_SAFER or
     * OPTION_PREFER_HIGHSEC.
     *
     * \param fromSolarSystem The system id of the start system.
     * \param toSolarSystemID The system id of the end system.
     * \param connections A list of additional connections, not
                          found in the static dump (e.g wormhole connections)
     * \param opts Options on how to chose the routes.
     * \return A list of solar system ids, from start to the end system.
     */
    Route route(
        1: i32 fromSolarSystemId,
        2: i32 toSolarSystemId,
        3: list<NewEden.Connection> connections,
        4: i8 opts = OPTION_PREFER_SHORTEST)
        throws (1: NewEden.LogicalError le),
    Route jumps(
        1: i32 fromSolarSystemId,
        2: i32 toSolarSystemId,
        3: list<NewEden.SolarSystem> systems,
        4: double reachInLightyears,
        5: i8 opts = OPTION_PREFER_SHORTEST,
        6: i8 limit = -1)
        throws (1: NewEden.LogicalError le),
}
