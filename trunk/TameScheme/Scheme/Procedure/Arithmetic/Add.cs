// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | '+' scheme procedure                                                Add.cs |
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

namespace Tame.Scheme.Procedure.Arithmetic
{
	/// <summary>
	/// The '+' scheme procedure
	/// </summary>
	[PreferredName("+")]
	public sealed class Add : IProcedure, Data.INumberIterator
	{
		public Add()
		{
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length == 0) return 0;
			return Data.NumberUtils.Iterate(args, this);
		}

		#endregion

		#region INumberIterator Members

		public int Iterate(int one, int two)
		{
			return one + two;
		}

		long Tame.Scheme.Data.INumberIterator.Iterate(long one, long two)
		{
			return one + two;
		}

		decimal Tame.Scheme.Data.INumberIterator.Iterate(decimal one, decimal two)
		{
			return one + two;
		}

		float Tame.Scheme.Data.INumberIterator.Iterate(float one, float two)
		{
			return one+two;
		}

		double Tame.Scheme.Data.INumberIterator.Iterate(double one, double two)
		{
			return one+two;
		}

		Tame.Scheme.Data.INumber Tame.Scheme.Data.INumberIterator.Iterate(Tame.Scheme.Data.INumber one, Tame.Scheme.Data.INumber two)
		{
			return one.Add(two);
		}

		#endregion
	}
}
