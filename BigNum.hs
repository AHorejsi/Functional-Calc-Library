module BigNum (
    BigNum,
    real,
    complex,
    quaternion,
    isReal,
    isComplex,
    isQuaternion,
    nplus,
    nminus,
    nmult,
    nnegate
) where
    import qualified MathResult as MR

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

    real :: a -> BigNum a
    real = BigReal_

    complex :: (Num a, Eq a) => a -> a -> BigNum a
    complex realVal imag0Val
        | 0 == imag0Val = real realVal 
        | otherwise = BigComplex_ realVal imag0Val

    quaternion :: (Num a, Eq a) => a -> a -> a -> a -> BigNum a
    quaternion realVal imag0Val imag1Val imag2Val
        | 0 == imag1Val && 0 == imag2Val = complex realVal imag0Val
        | otherwise = BigQuaternion_ realVal imag0Val imag1Val imag2Val

    isReal :: BigNum a -> Bool
    isReal BigReal_{} = True
    isReal _ = False

    isComplex :: BigNum a -> Bool
    isComplex BigComplex_{} = True
    isComplex _ = False

    isQuaternion :: BigNum a -> Bool
    isQuaternion BigQuaternion_{} = True
    isQuaternion _ = False

    nreal :: BigNum a -> a
    nreal (BigReal_ realVal) = realVal
    nreal (BigComplex_ realVal _) = realVal
    nreal (BigQuaternion_ realVal _ _ _) = realVal

    nimag0 :: (Num a) => BigNum a -> a
    nimag0 BigReal_{} = 0
    nimag0 (BigComplex_ _ imag0Val) = imag0Val
    nimag0 (BigQuaternion_ _ imag0Val _ _) = imag0Val

    nimag1 :: (Num a) => BigNum a -> a
    nimag1 BigReal_{} = 0
    nimag1 BigComplex_{} = 0
    nimag1 (BigQuaternion_ _ _ imag1Val _) = imag1Val

    nimag2 :: (Num a) => BigNum a -> a
    nimag2 BigReal_{} = 0
    nimag2 BigComplex_{} = 0
    nimag2 (BigQuaternion_ _ _ _ imag2Val) = imag2Val

    nplus :: (Num a, Eq a) => BigNum a -> BigNum a -> BigNum a
    nplus (BigReal_ leftReal) (BigReal_ rightReal) = real $ leftReal + rightReal
    nplus (BigReal_ leftReal) (BigComplex_ rightReal rightImag0) = complex (leftReal + rightReal) rightImag0
    nplus (BigReal_ leftReal) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) rightImag0 rightImag1 rightImag2
    nplus (BigComplex_ leftReal leftImag0) (BigComplex_ rightReal rightImag0) = complex (leftReal + rightReal) (leftImag0 + rightImag0)
    nplus (BigComplex_ leftReal leftImag0) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2
    nplus (BigQuaternion_ leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)
    nplus left right = nplus right left

    nminus :: (Num a, Eq a) => BigNum a -> BigNum a -> BigNum a
    nminus left right = nplus left (nnegate right)

    nmult :: (Num a, Eq a) => BigNum a -> BigNum a -> BigNum a
    nmult (BigReal_ leftReal) (BigReal_ rightReal) = real $ leftReal * rightReal
    nmult (BigReal_ leftReal) (BigComplex_ rightReal rightImag0) = complex (leftReal * rightReal) (leftReal * rightImag0)
    nmult (BigReal_ leftReal) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal) (leftReal * rightImag0) (leftReal * rightImag1) (leftReal * rightImag2)
    nmult (BigComplex_ leftReal leftImag0) (BigComplex_ rightReal rightImag0) = complex (leftReal * rightReal - leftImag0 * rightImag0) (leftReal * rightImag0 + rightReal * leftImag0)
    nmult (BigComplex_ leftReal leftImag0) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal - leftImag0 * rightImag0) (leftReal * rightImag0 + leftImag0 * rightReal) (leftReal * rightImag1 + leftImag0 * rightImag2) (leftReal * rightImag2 + leftImag0 * rightImag1)
    nmult (BigQuaternion_ leftReal leftImag0 leftImag1 leftImag2) (BigComplex_ rightReal rightImag0) = quaternion (leftReal * rightReal - leftImag0 * rightImag0) (leftReal * rightImag0 + leftImag0 * rightReal) (leftImag1 * rightReal - leftImag2 * rightImag0) ((-leftImag1) * rightImag0 + leftImag2 * rightReal)
    nmult (BigQuaternion_ leftReal leftImag0 leftImag1 leftImag2) (BigQuaternion_ rightReal rightImag0 rightImag1 rightImag2) = quaternion (leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2) (leftImag0 * rightReal + leftReal * rightImag0 - leftImag2 * rightImag1 + leftImag1 * rightImag2) (leftImag1 * rightReal + leftImag2 * rightImag0 + leftReal * rightImag1 - leftImag0 * rightImag2) (leftImag2 * rightReal - leftImag1 * rightImag0 + leftImag0 * rightImag1 + leftReal * rightImag2)
    nmult left right = nmult right left

    nnegate :: (Num a, Eq a) => BigNum a -> BigNum a
    nnegate (BigReal_ realVal) = real $ -realVal
    nnegate (BigComplex_ realVal imag0Val) = complex (-realVal) (-imag0Val)
    nnegate (BigQuaternion_ realVal imag0Val imag1Val imag2Val) = quaternion (-realVal) (-imag0Val) (-imag1Val) (-imag2Val)

    nconjugate :: (Num a, Eq a) => BigNum a -> BigNum a
    nconjugate (BigReal_ realVal) = real realVal
    nconjugate (BigComplex_ realVal imag0Val) = complex realVal (-imag0Val)
    nconjugate (BigQuaternion_ realVal imag0Val imag1Val imag2Val) = quaternion realVal (-imag0Val) (-imag1Val) (-imag2Val)
