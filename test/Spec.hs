import TestLib
import Control.Monad
import System.Exit

main :: IO ()
main = do
    ret <- testLibSuite
    case ret of
        True -> exitSuccess
        otherwise -> exitFailure
