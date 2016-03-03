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
     * <p>Find the shortest routes between two solar system. Additional
     * connections between solar systems can be placed. Available
     * options are <i>OPTION_PREFER_SHORTEST</i>, <i>OPTION_PREFER_SAFER</i> or
     * <i>OPTION_PREFER_HIGHSEC</i>.</p>
     *
     * <p>Additional connections can contain a 'weight'. Every connection has a weight
     * of 1 by default. Lower values will result in the connection being preferred.
     * Higher numbers will make it less preferred. E.g. weight 2 means if your shortet
     * standard route is 3 jumps we are picking the additional provided connection,
     * if it's 2 we pick the original connection.</p>
     */
    Route route(
        /** The system id to start from */
        1: i32 fromSolarSystemId,

        /** The system id to go to */
        2: i32 toSolarSystemId,

        /** A list of additional connections, not found in the static dump (e.g. wormholes). */
        3: list<NewEden.Connection> connections,

        /** Options on how to choose routes */
        4: i8 opts = OPTION_PREFER_SHORTEST)

        throws (1: NewEden.LogicalError le),

    /**
     * Find the shorteest jump routes.
     */
    Route jumps(
        /** The system id to start from */
        1: i32 fromSolarSystemId,

        /** The system id to go to */
        2: i32 toSolarSystemId,

        /** Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY */
        4: double rangeInLightyears,

        /** Options on how to choose routes */
        5: i8 opts = OPTION_PREFER_SHORTEST)

        throws (1: NewEden.LogicalError le, 2: NewEden.InvalidArgument ia),

    /**
     * Find all systems in range.
     */
    list<i32> range(
        /** The system id to start from */
        1: i32 fromSolarSystemId,

        /** Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY */
        4: double rangeInLightyears)

        throws (1: NewEden.LogicalError le, 2: NewEden.InvalidArgument ia),
}
