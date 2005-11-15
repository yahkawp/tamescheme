// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The numerator and denominator procedures                       NumDenom.cs |
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
    /// The numerator scheme procedure
    /// </summary>
    [PreferredName("numerator"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Primitive)]
    public sealed class Numerator : IProcedure
    {
        public Numerator() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 1) throw new Exception.RuntimeException("numerator takes exactly one argument");

            // If int or long, then this number is a 4/1 type number
            if (args[0] is int || args[0] is long) return args[0];

            // Work out the rational equivalent of the number
            Rational ratValue = null;
            bool exact = true;

            if (args[0] is float || args[0] is double)
            {
                ratValue = new Rational(checked((decimal)((double)args[0])));
                exact = false;
            }
            else if (args[0] is decimal)
            {
                ratValue = checked(new Rational((decimal)args[0]));
            }
            else if (args[0] is Rational)
            {
                ratValue = (Rational)args[0];
            }

            // Return the numerator
            if (ratValue != null)
            {
                if (exact)
                    return ratValue.Numerator;
                else
                    return (double)ratValue.Numerator;
            }
            else
            {
                throw new Exception.RuntimeException("numerator requires a real numeric argument");
            }
        }

        #endregion
    }

    /// <summary>
    /// The denominator scheme procedure
    /// </summary>
    [PreferredName("denominator"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Primitive)]
    public sealed class Denominator : IProcedure
    {
        public Denominator() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 1) throw new Exception.RuntimeException("denominator takes exactly one argument");

            // If int or long, then this number is a 4/1 type number
            if (args[0] is int || args[0] is long) return 1;

            // Work out the rational equivalent of the number
            Rational ratValue = null;
            bool exact = true;

            if (args[0] is float || args[0] is double)
            {
                ratValue = new Rational(checked((decimal)((double)args[0])));
                exact = false;
            }
            else if (args[0] is decimal)
            {
                ratValue = checked(new Rational((decimal)args[0]));
            }
            else if (args[0] is Rational)
            {
                ratValue = (Rational)args[0];
            }

            // Return the numerator
            if (ratValue != null)
            {
                if (exact)
                    return ratValue.Denominator;
                else
                    return (double)ratValue.Denominator;
            }
            else
            {
                throw new Exception.RuntimeException("denominator requires a real numeric argument");
            }
        }

        #endregion
    }
}
