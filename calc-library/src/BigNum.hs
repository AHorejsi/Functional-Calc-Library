module BigNum (
    BigNum,
    real,
    complex,
    quaternion
) where
    data BigNum a = BigReal_ {
        _rreal :: a
    } | BigComplex_ {
        _creal :: a,
        _cimag0 :: a
    } | BigQuaternion_ {
        _qreal :: a,
        _qimag0 :: a,
        _qimag1 :: a,
        _qimag2 :: a
    }

    instance (Eq a) => Eq (BigNum a) where
        (==) (BigReal_ leftReal) (BigReal_ rightReal) = leftReal == rightReal
        (==) (BigComplex_ leftReal leftImag0) (BigComplex_ rightReal rightImag0) = leftReal == rightReal && leftImag0 == rightImag0
        (==) (BigQuaternion_ leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = leftReal == rightReal && leftImag0 == rightImag0 && leftImag1 == rightImag1 && leftImag2 == rightImag2
        (==) _ _ = False

    real :: (Num a) => a -> BigNum a
    real = BigReal_

    complex :: (Num a, Eq a) => a -> a -> BigNum a
    complex realVal imag0Val
        | 0 == imag0Val = real realVal 
        | otherwise = BigComplex_ realVal imag0Val

    quaternion :: (Num a, Eq a) => a -> a -> a -> a -> BigNum a
    quaternion realVal imag0Val imag1Val imag2Val
        | 0 == imag1Val && 0 == imag2Val = complex realVal imag0Val
        | otherwise = BigQuaternion_ realVal imag0Val imag1Val imag2Val
