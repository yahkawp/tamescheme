// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The quotient procedure                                         Quotient.cs |
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
    /// The quotient procedure
    /// </summary>
    [PreferredName("quotient"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Primitive)]
    public class Quotient : IProcedure
    {
        public Quotient() { }

        #region Computing the quotient

        public struct Quot
        {
            public object quotient;
            public object modulo;
            public object remainder;
        }

        /// <summary>
        /// Utiltity function to compute the quotient + remainder between two scheme numbers
        /// </summary>
        public static Quot GetQuotient(object num1, object num2)
        {
            Quot res;

            // Simplify any INumbers (so (quotient 1+0i 2+0i) will work)
            if (num1 is INumber) num1 = ((INumber)num1).Simplify();
            if (num2 is INumber) num2 = ((INumber)num2).Simplify();

            // Use the number utilities to promote the numbers as appropriate
            object[] num = NumberUtils.Convert(num1, num2);

            // Work out the quotient depending on type
            if (num[0] is int || num[0] is long)
            {
                // Base case: the integer
                long lNum1 = (long)num[0];
                long lNum2 = (long)num[1];

                bool negative = (lNum1 < 0 && lNum2 > 0) || (lNum1 > 0 && lNum2 < 0);

                res.quotient = lNum1 / lNum2;
                res.remainder = lNum1 % lNum2;
                res.modulo = negative ? (lNum2+(long)res.remainder) : res.remainder;
            }
            else if (num[0] is decimal)
            {
                // Other cases: non-integer numbers
                decimal decNum1 = (decimal)num[0];
                decimal decNum2 = (decimal)num[1];

                bool negative = (decNum1 < 0 && decNum2 > 0) || (decNum1 > 0 && decNum2 < 0);

                decimal quot = decNum1 / decNum2;
                if (quot < 0)
                    quot = decimal.Ceiling(quot);
                else
                    quot = decimal.Floor(quot);

                decimal remainder = decNum1 - (quot * decNum2);
                decimal modulo = negative ? (decNum2 + remainder) : remainder;

                res.quotient = quot;
                res.modulo = modulo;
                res.remainder = remainder;
            }
            else if (num[0] is float || num[0] is double)
            {
                // Other cases: non-integer numbers
                double dNum1 = (double)num[0];
                double dNum2 = (double)num[1];

                bool negative = (dNum1 < 0 && dNum2 > 0) || (dNum1 > 0 && dNum2 < 0);

                double quot = dNum1 / dNum2;
                if (quot < 0)
                    quot = Math.Ceiling(quot);
                else
                    quot = Math.Floor(quot);

                double remainder = dNum1 - (quot * dNum2);
                double modulo = negative ? (dNum2+remainder) : remainder;

                res.quotient = quot;
                res.modulo = modulo;
                res.remainder = remainder;
            }
            else if (num[0] is Rational)
            {
                // Rational numbers
                Rational ratNum1 = (Rational)num[0];
                Rational ratNum2 = (Rational)num[1];

                bool negative = (ratNum1.Numerator < 0 && ratNum2.Numerator > 0) || (ratNum1.Numerator > 0 && ratNum2.Numerator < 0);

                Rational quot = (Rational)ratNum1.Divide(ratNum2);
                long quotInt = quot.Numerator/quot.Denominator;

                Rational remainder = (Rational)ratNum1.Subtract(new Rational(quotInt, 1).Multiply(ratNum2));
                Rational modulo = negative ? (Rational)(ratNum2.Add(remainder)) : remainder;

                res.quotient = quotInt;
                res.modulo = modulo.Simplify();
                res.remainder = remainder.Simplify();
            }
            else
            {
                throw new Exception.RuntimeException("The quotient can only be taken for real numbers");
            }

            return res;
        }

        #endregion

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 2) throw new Exception.RuntimeException("quotient takes exactly two arguments");

            return GetQuotient(args[0], args[1]).quotient;
        }

        #endregion
    }
}
