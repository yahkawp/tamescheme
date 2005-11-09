// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The exact?/inexact? procedures                                  IsExact.cs |
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

namespace Tame.Scheme.Procedure.Number
{
    /// <summary>
    /// The exact? scheme procedure
    /// </summary>
    [PreferredName("exact?"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Primitive)]
    public class IsExact : IProcedure
    {
        public IsExact() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 1) throw new Exception.RuntimeException("exact? takes one argument only");

            if (args[0] is int || args[0] is long || args[0] is decimal ||
                args[0] is Data.Number.Rational || args[0] is Data.Number.RationalComplex)
                return true;
            else if (args[0] is float || args[0] is double ||
                     args[0] is Data.Number.Complex)
                return false;
            else
                throw new Exception.RuntimeException("exact? must be called with a numeric argument");
        }

        #endregion
    }

    /// <summary>
    /// The inexact? scheme procedure
    /// </summary>
    [PreferredName("inexact?"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Primitive)]
    public class IsInexact : IProcedure
    {
        public IsInexact() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 1) throw new Exception.RuntimeException("inexact? takes one argument only");

            if (args[0] is int || args[0] is long || args[0] is decimal ||
                args[0] is Data.Number.Rational || args[0] is Data.Number.RationalComplex)
                return false;
            else if (args[0] is float || args[0] is double ||
                     args[0] is Data.Number.Complex)
                return true;
            else
                throw new Exception.RuntimeException("exact? must be called with a numeric argument");
        }

        #endregion
    }
}
