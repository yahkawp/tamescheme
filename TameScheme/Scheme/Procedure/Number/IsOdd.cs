// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The odd? library procedure                                        IsOdd.cs |
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
    /// The odd? scheme library procedure
    /// </summary>
    [PreferredName("odd?"),
     SchemeGroup(SchemeGroup.Library),
     SchemeUsage(SchemeUsage.Normal)]
    public class IsOdd : IProcedure
    {
        public IsOdd() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length != 1) throw new Exception.RuntimeException("odd? takes exactly one argument");

            // Simplify if possible
            object num = args[0];
            if (num is Data.INumber) num = ((Data.INumber)num).Simplify();

            // Only the 'simple' types can be zero
            if (num is int || num is long)
                return ((long)num)%2 != 0;
            else if (num is decimal || num is float || num is double || num is Data.INumber)
                throw new Exception.RuntimeException("odd? cannot be determined for non-integer types");
            else
                throw new Exception.RuntimeException("The argument to odd? must be a numeric type");
        }

        #endregion
    }
}
