// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// |                                                    GreaterThanOrEqualTo.cs |
// | Implementation of the '>=' numeric comparison procedure                    |
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

namespace Tame.Scheme.Procedure.Comparison
{
	/// <summary>
	/// Summary description for GreaterThanOrEqualTo.
	/// </summary>
    [PreferredName(">="), SchemeGroup(SchemeGroup.Primitive), SchemeUsage(SchemeUsage.Normal)]
	public class GreaterThanOrEqualTo : NumberCompare
	{
		public GreaterThanOrEqualTo()
		{
		}

		protected override bool Compare(int a, int b)
		{
			return a >= b;
		}

		protected override bool Compare(long a, long b)
		{
			return a >= b;
		}

		protected override bool Compare(decimal a, decimal b)
		{
			return a >= b;
		}

		protected override bool Compare(float a, float b)
		{
			return a >= b;
		}

		protected override bool Compare(double a, double b)
		{
			return a >= b;
		}

		protected override bool Compare(Tame.Scheme.Data.INumber a, Tame.Scheme.Data.INumber b)
		{
			return a.Compare(b) >= 0;
		}
	}
}
