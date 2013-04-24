/**
 * KeySpline - use bezier curve for transition easing function
 * is inspired from Firefox's nsSMILKeySpline.cpp
 * Usage:
 * var spline = new KeySpline(0.25, 0.1, 0.25, 1.0)
 * spline.get(x) => returns the easing value | x must be in [0, 1] range
 */
function Spline(mX1, mY1, mX2, mY2) {
    this.get = function(aX) {
        if(mX1 == mY1 && mX2 == mY2)
            return aX; // linear
        return CalcBezier(GetTForX(aX), mY1, mY2);
    };

    function A(aA1, aA2) {
        return 1.0 - 3.0 * aA2 + 3.0 * aA1;
    }
    
    function B(aA1, aA2) {
        return 3.0 * aA2 - 6.0 * aA1;
    }
    function C(aA1) {
        return 3.0 * aA1;
    }

    // Returns x(t) given t, x1, and x2, or y(t) given t, y1, and y2.
    function CalcBezier(aT, aA1, aA2) {
        return ((A(aA1, aA2) * aT + B(aA1, aA2)) * aT + C(aA1)) * aT;
    }

    // Returns dx/dt given t, x1, and x2, or dy/dt given t, y1, and y2.
    function GetSlope(aT, aA1, aA2) {
        return 3.0 * A(aA1, aA2) * aT * aT + 2.0 * B(aA1, aA2) * aT + C(aA1);
    }

    function GetTForX(aX) {
        // Newton raphson iteration
        var aGuessT = aX;
        for (var i = 0; i < 4; ++i) {
            var currentSlope = GetSlope(aGuessT, mX1, mX2);
            if (currentSlope == 0.0)
                return aGuessT;
            var currentX = CalcBezier(aGuessT, mX1, mX2) - aX;
            aGuessT -= currentX / currentSlope;
        }
        return aGuessT;
    }
}