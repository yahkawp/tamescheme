// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | '/' scheme procedure                                             Divide.cs |
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

namespace Tame.Scheme.Procedure.Arithmetic
{
	/// <summary>
	/// The '/' scheme procedure.
	/// </summary>
    [PreferredName("/"), SchemeGroup(SchemeGroup.Primitive), SchemeUsage(SchemeUsage.Normal)]
	public sealed class Divide : IProcedure, Data.INumberIterator
	{
		public Divide()
		{
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			// Divide works differently with ints and longs to the standard iterator (first value is promoted to a rational always if it's an int, decimal or long)
			int length = args.Length;

			if (length == 0) throw new Exception.RuntimeException("/ must have at least one argument");
			if (length == 1)
			{
				// If there's only one argument, then take the inverse
				object[] inverse = new object[2];

				inverse[0] = new Rational(1, 1);
				inverse[1] = args[0];
				return Data.NumberUtils.Iterate(inverse, this);
			}

			// Convert the first argument to a Rational if it's an int, long
			// TODO: decimals??
			if (args[0] is int)
				args[0] = new Rational((long)(int)args[0], 1);
			else if (args[0] is long)
				args[0] = new Rational((long)args[0], 1);

			// Now iterate: the standard iterator will produce a good result now
			return Data.NumberUtils.Iterate(args, this);
		}

		#endregion

		#region INumberIterator Members

		public int Iterate(int one, int two)
		{
			// Should never be called
			return int.MaxValue;
		}

		long Tame.Scheme.Data.INumberIterator.Iterate(long one, long two)
		{
			// Also should never be called
			return long.MaxValue;
		}

		decimal Tame.Scheme.Data.INumberIterator.Iterate(decimal one, decimal two)
		{
			// TODO: decimal division is not exact (eg, evaluate (* (/ #e1.2 #e3.4) #e3.4)
			return one/two;
		}

		float Tame.Scheme.Data.INumberIterator.Iterate(float one, float two)
		{
			return one/two;
		}

		double Tame.Scheme.Data.INumberIterator.Iterate(double one, double two)
		{
			return one/two;
		}

		Tame.Scheme.Data.INumber Tame.Scheme.Data.INumberIterator.Iterate(Tame.Scheme.Data.INumber one, Tame.Scheme.Data.INumber two)
		{
			return one.Divide(two);
		}

		#endregion
	}
}
