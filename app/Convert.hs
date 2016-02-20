module Convert where
import qualified Data.Text.Lazy as TL

import qualified NewEden as E
import qualified NewEden_Types as T

toThriftSolarSystem :: E.Solarsystem -> T.SolarSystem
toThriftSolarSystem s =
    T.SolarSystem
        (E.systemId s)
        (TL.pack (E.systemName s))
        (E.systemSecurity s)

toThriftCoordinate c =
    T.Coordinate
        (E.xCoord c)
        (E.yCoord c)
        (E.zCoord c)

toThriftConnection (E.Connection a b) =
    T.Connection
        (E.systemId a)
        (E.systemId b)

fromThriftCoordinate c =
    E.Coordinate
        (T.coordinate_x c)
        (T.coordinate_y c)
        (T.coordinate_z c)

toThriftCelestial c =
    T.Celestial
        (E.celestialItemID c)
        (TL.pack (E.celestialTypeName c))
