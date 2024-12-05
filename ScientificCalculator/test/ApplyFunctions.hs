module ApplyFunctions where

applyUnary :: String -> Double -> Double
applyUnary "sin" = sin
applyUnary "cos" = cos
applyUnary "tan" = tan
applyUnary "exp" = exp
applyUnary "sinh" = sinh
applyUnary "cosh" = cosh
applyUnary "tanh" = tanh
applyUnary "deg" = \x -> x * (180 / pi)
applyUnary "rad" = \x -> x * (pi / 180) 
applyUnary "cbrt" = (** (1 / 3)) 
applyUnary "x^2" = (** 2) 
applyUnary "x^3" = (** 3) 
applyUnary "sqrt" = \x -> if x < 0 then error "Negative square root" else sqrt x
applyUnary "ln" = \x -> if x <= 0 then error "Logarithm of non-positive number" else log x
applyUnary "log10" = \x -> if x <= 0 then error "Logarithm of non-positive number" else logBase 10 x
applyUnary "asin" = \x -> if x < -1 || x > 1 then error "Arcsine out of domain" else asin x
applyUnary "acos" = \x -> if x < -1 || x > 1 then error "Arccosine out of domain" else acos x
applyUnary "atan" = atan
applyUnary "asinh" = asinh
applyUnary "acosh" = \x -> if x < 1 then error "Hyperbolic arccosine out of domain" else acosh x
applyUnary "atanh" = \x -> if x <= -1 || x >= 1 then error "Hyperbolic arctangent out of domain" else atanh x
applyUnary "1/x" = \x -> if x == 0 then error "Division by zero" else 1 / x
applyUnary _ = id

applyBinary :: String -> Double -> Double -> Double
applyBinary "+" = (+)
applyBinary "-" = (-)
applyBinary "*" = (*)
applyBinary "/" = \x y -> if y == 0 then error "Division by zero" else x / y
applyBinary "^" = (**)
applyBinary "Lny" = \x y -> if x <= 0 || x == 1 || y <= 0 then error "Invalid logarithm base or argument" else logBase x y
applyBinary "EE" = \x y -> x * (10 ** y)
applyBinary _ = const (const 0)
