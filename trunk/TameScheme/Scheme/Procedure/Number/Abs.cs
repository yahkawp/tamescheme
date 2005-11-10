// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The abs procedure                                                   Abs.cs |
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
    [PreferredName("abs"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Primitive)]
    public class Abs : IProcedure
    {
        public Abs() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 1) throw new Exception.RuntimeException("abs takes exactly one argument");

            // Use the simplified variant of the number if available
            object num = args[0];
            if (num is Data.INumber) num = ((Data.INumber)num).Simplify();

            // Compute the absolute value depending on type
            if (num is int || num is long)
            {
                long lNum = (long)num;

                if (lNum < 0) return -lNum; else return lNum;
            }
            else if (num is decimal)
            {
                decimal decNum = (decimal)num;

                if (decNum < 0) return -decNum; else return decNum;
            }
            else if (num is float || num is double)
            {
                double dNum = (double)num;

                if (dNum < 0) return -dNum; else return dNum;
            }
            else if (num is Data.INumber)
            {
                return ((Data.INumber)num).Abs();
            }
            else
            {
                throw new Exception.RuntimeException("abs must be called with a numeric argument");
            }
        }

        #endregion
    }
}
