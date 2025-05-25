module MathResult (
    MathResult,
    success,
    failure,
    isSuccess,
    isFailure,
    value,
    message
) where

    data MathResult a = Success_ {
        value :: a
    } | Failure_ {
        message :: String
    }

    success :: a -> MathResult a
    success = Success_

    failure :: String -> MathResult a
    failure = Failure_

    isSuccess :: MathResult a -> Bool
    isSuccess Success_{} = True
    isSuccess _ = False

    isFailure :: MathResult a -> Bool
    isFailure = not . isSuccess
