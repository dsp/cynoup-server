namespace cpp cynoup.map
namespace php cynoup.map

include "NewEden.thrift"

exception LogicalError
{
    1: i32 errno,
    2: string message,
}

service NewEdenMap
{
    map<i32, NewEden.SolarSystem> systems(),
    map<i32, NewEden.Connection> connection(),

    list<NewEden.Celestial> closest(
        1: i32 solarSystemId,
        2: NewEden.Coordinate coord,
    ),

    NewEden.Celestial closestCelestial(
        1: NewEden.Coordinate coord,
    ),

    double distance(
        1: NewEden.Celestial celestial,
        2: NewEden.Coordinate coord,
    ),
}
