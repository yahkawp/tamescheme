// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The rationalize library procedure                           Rationalize.cs |
// +----------------------------------------------------------------------------+
// | Copyright (c) 2005 Andrew Hunter                                           |
// |                                                                            |
// | Permission is hereby granted, free of charge, to any person obtaining a    |
// | copy of this software and associated documentation files (the "Software"), |
// | to deal in the Software without restriction, including without limitation  |
// | the rights to use, copy, modify, merge, publish, distribute, sublicense,   |
// | and/or sell copies of the Software, and to permit persons to whom the      |
// | Software is furnished to do so, subject to the following conditions:       |
// |                                                                            |
// | The above copyright notice and this permission notice shall be included in |
// | all copies or substantial portions of the Software.                        |
// |                                                                            |
// | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR |
// | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   |
// | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    |
// | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER |
// | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    |
// | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        |
// | DEALINGS IN THE SOFTWARE.                                                  |
// +----------------------------------------------------------------------------+

using System;
using Tame.Scheme.Data;
using Tame.Scheme.Data.Number;

namespace Tame.Scheme.Procedure.Number
{
    /// <summary>
    /// The rationalize library procedure
    /// </summary>
    [PreferredName("rationalize"),
     SchemeGroup(SchemeGroup.Library),
     SchemeUsage(SchemeUsage.Normal)]
    public sealed class Rationalize : IProcedure
    {
        public Rationalize() { }

        #region IProcedure Members

        #region Rationalizing doubles

        // Hmm, could really use proper templates here... Some way to extend the number of numeric types would be nice as well.

        /// <summary>
        /// Class to compute the continued fraction of a given double value
        /// </summary>
        /// <remarks>Algorithm based on http://mathworld.wolfram.com/ContinuedFraction.html</remarks>
        private struct ContinuedFraction
        {
            public long a;
            public double r;

            public long p, q;
            public long lastP, lastQ;

            public ContinuedFraction(double value)
            {
                // Initial values
                lastP = 1;
                lastQ = 0;

                // Compute the first value
                r = value;

                a = checked((long)Math.Floor(r));
                r = 1 / (r - a);

                p = a;
                q = 1;
            }

            public void Iterate()
            {
                // Compute the next value

                // a(n+1)
                a = checked((long)Math.Floor(r));
                r = 1 / (r - a);

                // p(n+1)/q(n+1)
                long newP = checked(a * p + lastP);
                long newQ = checked(a * q + lastQ);

                lastP = p;
                lastQ = q;
                p = newP;
                q = newQ;
            }
        }

        private double SimpleRationalize(double value, double difference)
        {
            // Algorithm based on http://zurich.csail.mit.edu/pipermail/rrrs-authors/1988-February/000834.html
            if (difference < 0) difference = -difference;

            // Compute the upper/lower continued fraction
            ContinuedFraction lower = new ContinuedFraction(value - difference);
            ContinuedFraction upper = new ContinuedFraction(value + difference);

            ContinuedFraction lastLower = lower;

            double rem = 0;

            // Base case
            if (lower.a != upper.a) return 0;

            // Allow the values to converge until they differ
            while (lower.a == upper.a)
            {
                // Remember the last iteration for lower, and the difference in the remainders
                lastLower = lower;
                rem = lower.r - upper.r;

                // Move to the next value
                lower.Iterate();
                upper.Iterate();
            }

            // Compute the final term
            long finalTerm = checked((long)Math.Ceiling(rem));

            // Compute the next value from lastLower given this term
            long finalP = finalTerm * lastLower.p + lastLower.lastP;
            long finalQ = finalTerm * lastLower.q + lastLower.lastQ;

            return (double)finalP/(double)finalQ;
        }

        #endregion

        #region Rationalizing rationals

        static Rational one = new Rational(1, 1);
        static Rational zero = new Rational(0, 1);

        // Hmm, could really use proper templates here... Some way to extend the number of numeric types would be nice as well.

        /// <summary>
        /// Class to compute the continued fraction of a given double value
        /// </summary>
        /// <remarks>Algorithm based on http://mathworld.wolfram.com/ContinuedFraction.html</remarks>
        private struct ContinuedFractionRational
        {
            public long a;
            public Rational r;

            public long p, q;
            public long lastP, lastQ;

            public ContinuedFractionRational(Rational value)
            {
                // Initial values
                lastP = 1;
                lastQ = 0;

                // Compute the first value
                r = value;

                a = r.Floor();
                if (r.Denominator != 1) r = (Rational)one.Divide(r.Subtract(new Rational(a, 1)));
                else r = zero;

                p = a;
                q = 1;
            }

            public void Iterate()
            {
                // Compute the next value

                // a(n+1)
                a = r.Floor();

                if (r.Denominator == 1) return;

                r = (Rational)one.Divide(r.Subtract(new Rational(a, 1)));

                // p(n+1)/q(n+1)
                long newP = a * p + lastP;
                long newQ = a * q + lastQ;

                lastP = p;
                lastQ = q;
                p = newP;
                q = newQ;
            }
        }

        private Rational SimpleRationalize(Rational value, Rational difference)
        {
            // Algorithm based on http://zurich.csail.mit.edu/pipermail/rrrs-authors/1988-February/000834.html

            if (difference.Numerator < 0) difference = new Rational(-difference.Numerator, difference.Denominator);

            // Compute the upper/lower continued fraction
            ContinuedFractionRational lower = new ContinuedFractionRational((Rational)value.Subtract(difference));
            ContinuedFractionRational upper = new ContinuedFractionRational((Rational)value.Add(difference));

            ContinuedFractionRational lastLower = lower;

            Rational rem = zero;

            // Base case
            if (lower.a != upper.a) return zero;

            // Allow the values to converge until they differ (or until they actually converge)
            while (lower.a == upper.a)
            {
                // Remember the last iteration for lower, and the difference in the remainders
                lastLower = lower;
                rem = (Rational)lower.r.Subtract(upper.r);

                // Move to the next value
                lower.Iterate();
                upper.Iterate();
            }

            // Compute the final term
            long finalTerm = rem.Ceiling();

            // Compute the next value from lastLower given this term
            long finalP = finalTerm * lastLower.p + lastLower.lastP;
            long finalQ = finalTerm * lastLower.q + lastLower.lastQ;

            return new Rational(finalP, finalQ);
        }

        #endregion

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 2) throw new Exception.RuntimeException("rationalize must be called with two arguments");

            // If either value is an inexact number, then ensure the other is inexact also
            object firstVal = args[0];
            object secondVal = args[1];

            if (secondVal is float || secondVal is double)
            {
                if (firstVal is int || firstVal is long || firstVal is decimal || firstVal is float || firstVal is double)
                    firstVal = (double)Convert.ToDouble(firstVal);
                else if (firstVal is Rational)
                    firstVal = ((Rational)firstVal).ToDouble();
                else
                    throw new Exception.RuntimeException("The first argument passed to rationalize must be a real number");
            }
            else if (firstVal is float || firstVal is double)
            {
                if (secondVal is int || secondVal is long || secondVal is decimal || secondVal is float || secondVal is double)
                    secondVal = (double)Convert.ToDouble(secondVal);
                else if (secondVal is Rational)
                    secondVal = ((Rational)secondVal).ToDouble();
                else
                    throw new Exception.RuntimeException("The second argument passed to rationalize must be a real number");
            }

            // Compute the rationalization based on the type
            if (firstVal is float || firstVal is double)
            {
                // Work out the difference depending on the type of SecondVal
                double value = Convert.ToDouble(firstVal);
                double difference = Convert.ToDouble(secondVal);

                if (value < 0)
                    return -SimpleRationalize(-value, difference);
                else
                    return SimpleRationalize(value, difference);
            }
            else if (firstVal is int || firstVal is long)
            {
                Rational value = new Rational(Convert.ToInt64(firstVal), 1);
                Rational difference;

                if (secondVal is int || secondVal is long)
                {
                    difference = new Rational(Convert.ToInt64(secondVal), 1);
                }
                else if (secondVal is decimal)
                {
                    difference = new Rational((decimal)secondVal);
                }
                else if (secondVal is Rational)
                {
                    difference = (Rational)secondVal;
                }
                else
                {
                    throw new Exception.RuntimeException("The second argument passed to rationalize must be a real number");
                }

                if (value.Numerator < 0)
                    return zero.Subtract(SimpleRationalize(new Rational(-value.Numerator, value.Denominator), difference));
                else
                    return SimpleRationalize(value, difference);
            }
            else if (firstVal is decimal)
            {
                Rational value = new Rational((decimal)firstVal);
                Rational difference;

                if (secondVal is int || secondVal is long)
                {
                    difference = new Rational(Convert.ToInt64(secondVal), 1);
                }
                else if (secondVal is decimal)
                {
                    difference = new Rational((decimal)secondVal);
                }
                else if (secondVal is Rational)
                {
                    difference = (Rational)secondVal;
                }
                else
                {
                    throw new Exception.RuntimeException("The second argument passed to rationalize must be a real number");
                }

                if (value.Numerator < 0)
                    return zero.Subtract(SimpleRationalize(new Rational(-value.Numerator, value.Denominator), difference));
                else
                    return SimpleRationalize(value, difference);
            }
            else if (firstVal is Rational)
            {
                Rational value = (Rational)firstVal;
                Rational difference;

                if (secondVal is int || secondVal is long)
                {
                    difference = new Rational(Convert.ToInt64(secondVal), 1);
                }
                else if (secondVal is decimal)
                {
                    difference = new Rational((decimal)secondVal);
                }
                else if (secondVal is Rational)
                {
                    difference = (Rational)secondVal;
                }
                else
                {
                    throw new Exception.RuntimeException("The second argument passed to rationalize must be a real number");
                }

                if (value.Numerator < 0)
                    return zero.Subtract(SimpleRationalize(new Rational(-value.Numerator, value.Denominator), difference));
                else
                    return SimpleRationalize(value, difference);
            }
            else
            {
                throw new Exception.RuntimeException("The first argument passed to rationalize must be a real number");
            }
        }

        #endregion
    }
}
