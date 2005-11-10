// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The min and max library procedures                               MinMax.cs |
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
    [PreferredName("min"),
     SchemeGroup(SchemeGroup.Library),
     SchemeUsage(SchemeUsage.Normal)]
    public class Min : IProcedure, INumberIterator
    {
        public Min() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length < 1) throw new Exception.RuntimeException("min requires at least one argument");
            return Data.NumberUtils.Iterate(args, this);
        }

        #endregion

        #region INumberIterator Members

        public int Iterate(int one, int two)
        {
            return one < two ? one : two;
        }

        public long Iterate(long one, long two)
        {
            return one < two ? one : two;
        }

        public decimal Iterate(decimal one, decimal two)
        {
            return one < two ? one : two;
        }

        public float Iterate(float one, float two)
        {
            return one < two ? one : two;
        }

        public double Iterate(double one, double two)
        {
            return one < two ? one : two;
        }

        public INumber Iterate(INumber one, INumber two)
        {
            return one.Compare(two) < 0 ? one : two;
        }

        #endregion
    }

    [PreferredName("max"),
     SchemeGroup(SchemeGroup.Library),
     SchemeUsage(SchemeUsage.Normal)]
    public class Max : IProcedure, INumberIterator
    {
        public Max() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length < 1) throw new Exception.RuntimeException("min requires at least one argument");
            return Data.NumberUtils.Iterate(args, this);
        }

        #endregion

        #region INumberIterator Members

        public int Iterate(int one, int two)
        {
            return one > two ? one : two;
        }

        public long Iterate(long one, long two)
        {
            return one > two ? one : two;
        }

        public decimal Iterate(decimal one, decimal two)
        {
            return one > two ? one : two;
        }

        public float Iterate(float one, float two)
        {
            return one > two ? one : two;
        }

        public double Iterate(double one, double two)
        {
            return one > two ? one : two;
        }

        public INumber Iterate(INumber one, INumber two)
        {
            return one.Compare(two) > 0 ? one : two;
        }

        #endregion
    }
}
