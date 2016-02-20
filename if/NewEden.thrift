namespace cpp cynoup.neweden
namespace php cynoup.neweden

struct Coordinate
{
    1: double x,
    2: double y,
    3: double z,
}

struct SolarSystem
{
    1: i32 systemId,
    2: string name,
    10: double security,
}

struct Connection
{
    1: i32 fromSystemId,
    2: i32 toSystemId,
    3: optional i16 weight = 1,
}

struct Celestial
{
    1: i32 itemId,
    2: string name,
}

exception LogicalError
{
    1: i32 errno,
    2: string message,
}
