namespace cpp cynoup.map
namespace php cynoup.map

include "NewEden.thrift"

service NewEdenMap
{
    /**
     * Returns all system in the static dump
     */
    map<i32, NewEden.SolarSystem> systems(),
    
    /**
     * Returns all connections in the static dump
     */
    map<i32, NewEden.Connection> connection(),

    /**
     * Return the the two celestials which are in line with the given coordinate,
     * or are closest to being in line.
     *
     * @note: This implementation is experimental and might not yield correct results.
     */
    list<NewEden.Celestial> closest(
        1: i32 solarSystemId,
        2: NewEden.Coordinate coord,
    ),

    /**
     * Closest celestial from a coordinate
     *
     * @note NOT YET IMPLEMENTED!
     */
    NewEden.Celestial closestCelestial(
        1: NewEden.Coordinate coord,
    ),

    /**
     * Distance from a celestial to a coordinate
     *
     * @note NOT YET IMPLMENETED
     */
    double distance(
        1: NewEden.Celestial celestial,
        2: NewEden.Coordinate coord,
    ),
}
