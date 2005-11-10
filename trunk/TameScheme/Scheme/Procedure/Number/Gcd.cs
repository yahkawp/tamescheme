// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The gcd and lcm library procedures                               MinMax.cs |
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
    /// Implementation of the gcd library procedure
    /// </summary>
    [PreferredName("gcd"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Library)]
    public class Gcd : IProcedure
    {
        public Gcd() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length == 0) return 0;

            if (!(args[0] is long) && !(args[0] is int)) throw new Exception.RuntimeException("The arguments to gcd must be integer numbers");

            // Get the initial divisor value
            long divisor = (long)args[0];

            // Iterate across the arguments
            for (int x = 1; x < args.Length; x++)
            {
                if (!(args[x] is long) && !(args[x] is int)) throw new Exception.RuntimeException("The arguments to gcd must be integer numbers");
                divisor = NumberUtils.Gcd(divisor, (long)args[x]);
            }

            return divisor;
        }

        #endregion
    }

    /// <summary>
    /// Implementation of the lcm library procedure
    /// </summary>
    [PreferredName("lcm"),
     SchemeUsage(SchemeUsage.Normal),
     SchemeGroup(SchemeGroup.Library)]
    public class Lcm : IProcedure
    {
        public Lcm() { }

        #region IProcedure Members

        public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
        {
            if (args.Length == 0) return 0;

            if (!(args[0] is long) && !(args[0] is int)) throw new Exception.RuntimeException("The arguments to lcm must be integer numbers");

            // Get the initial divisor value
            long lcm = (long)args[0];

            // Iterate across the arguments
            for (int x = 1; x < args.Length; x++)
            {
                if (!(args[x] is long) && !(args[x] is int)) throw new Exception.RuntimeException("The arguments to lcm must be integer numbers");


                lcm = (lcm * ((long)args[x])) / NumberUtils.Gcd(lcm, (long)args[x]);
            }

            return lcm;
        }

        #endregion
    }
}
