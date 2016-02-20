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
     * Find the shortest route between two solar systems
     * 
     * Find the shortest routes between two solar system. Additional
     * connections between solar systems can be placed. Available
     * options are OPTION_PREFER_SHORTEST, OPTION_PREFER_SAFER or
     * OPTION_PREFER_HIGHSEC.
     *
     * Additional connections can contain a 'weight'. Every connection has a weight
     * of 1 by default. Lower values will result in the connection being preferred.
     * Higher numbers will make it less preferred. E.g. weight 2 means if your shortet
     * standard route is 3 jumps we are picking the additional provided connection,
     * if it's 2 we pick the original connection.
     * 
     * @param i32 fromSolarSystem - The system id of the start system.
     * @param i32 toSolarSystemID - The system id of the end system.
     * @param list connections - A list of additional connections, not
     *                           found in the static dump (e.g wormhole connections)
     * @param i8 opts  - Options on how to chose the routes.
     * @return list - A list of solar system ids, from start to the end system.
     */
    Route route(
        1: i32 fromSolarSystemId,
        2: i32 toSolarSystemId,
        3: list<NewEden.Connection> connections,
        4: i8 opts = OPTION_PREFER_SHORTEST)
        throws (1: NewEden.LogicalError le),

    /**
     * Find the shorteest jump routes
     *
     * @param i32 fromSolarSystemId - The system id of the start system.
     * @param i32 toSolarSystemID - The system id of the end system.
     * @param double rangeInLightyears - The jump range in lightyears, e.g. 5.0 for a Nyx.
     * @param i8 opts - Various options.
     */
    Route jumps(
        1: i32 fromSolarSystemId,
        2: i32 toSolarSystemId,
        4: double rangeInLightyears,
        5: i8 opts = OPTION_PREFER_SHORTEST)
        throws (1: NewEden.LogicalError le),
}
