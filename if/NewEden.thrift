namespace cpp cynoup.neweden
namespace php cynoup.neweden

/**
 * Represents a coordinate in New Eden. The coordinate is absolute.
 */
struct Coordinate
{
    1: double x,
    2: double y,
    3: double z,
}

/**
 * Represents a simple solar system
 *
 * The solar system system ID matches the ids from the static dump or CREST.
 */
struct SolarSystem
{
    1: i32 systemId,
    2: string name,
    10: double security,
}

/**
 * Represents a connection between two solar systems
 *
 * Represents a solar system connection. A weight can be given for when passing
 * new connections to the a routing algorithms to make them harder to reach.
 * Weights < 1 are easier than average to reach, weights > 1 are proportional harder
 * to reach.
 */
struct Connection
{
    1: i32 fromSystemId,
    2: i32 toSystemId,
    3: optional double weight = 1.0,
}

struct Celestial
{
    1: i32 itemId,
    2: string name,
}

/**
 * Thrown in the event of a request that makes no sense in the
 * New Eden universe (e.g. non matching solar system ids)
 */
exception LogicalError
{
    1: i32 errno,
    2: string message,
}

exception InvalidArgument
{
    1: i32 errno,
    2: string message,
}
