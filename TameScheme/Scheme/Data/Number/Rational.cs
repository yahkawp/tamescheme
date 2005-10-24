// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Rational number class                                          Rational.cs |
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

namespace Tame.Scheme.Data.Number
{
	/// <summary>
	/// Summary description for Rational.
	/// </summary>
	public sealed class Rational : INumber
	{
		public Rational(long numerator, long denominator)
		{
			long gcd = NumberUtils.Gcd(numerator, denominator);

			this.numerator = numerator;
			this.denominator = denominator;

			if (gcd != 1)
			{
				this.numerator /= gcd;
				this.denominator /= gcd;
			}
		}

		private Rational(long numerator, long denominator, bool unused)
		{
			// Constructor used internally to build a Rational without normalising it first (ie, already simplified)
			this.numerator = numerator;
			this.denominator = denominator;
		}

		long numerator, denominator;

		public double ToDouble()
		{
			return ((double)numerator)/((double)denominator);
		}

		#region INumber Members

		public int Compare(INumber number)
		{
			// TODO: implement me
			return 0;
		}

		public INumber Add(INumber number)
		{
			Rational ratNum = (Rational) number;

			long d1 = NumberUtils.Gcd(denominator, ratNum.denominator);
			
			if (d1 == 1)
			{
				return new Rational(numerator*ratNum.denominator + ratNum.numerator*denominator, denominator*ratNum.denominator, true);
			}

			long t = numerator*(ratNum.denominator/d1) + ratNum.numerator*(denominator/d1);
			long d2 = NumberUtils.Gcd(t, d1);

			return new Rational(t/d2, (denominator/d1)*(ratNum.denominator/d2), true);
		}

		public INumber Subtract(INumber number)
		{
			Rational ratNum = (Rational) number;

			long d1 = NumberUtils.Gcd(denominator, ratNum.denominator);
			
			if (d1 == 1)
			{
				return new Rational(numerator*ratNum.denominator - ratNum.numerator*denominator, denominator*ratNum.denominator, true);
			}

			long t = numerator*(ratNum.denominator/d1) - ratNum.numerator*(denominator/d1);
			long d2 = NumberUtils.Gcd(t, d1);

			return new Rational(t/d2, (denominator/d1)*(ratNum.denominator/d2), true);
		}

		public INumber Multiply(INumber number)
		{
			Rational ratNum = (Rational) number;

			long d1 = NumberUtils.Gcd(numerator, ratNum.denominator);
			long d2 = NumberUtils.Gcd(denominator, ratNum.numerator);

			return new Rational((numerator/d1)*(ratNum.numerator/d2), (denominator/d2)*(ratNum.denominator/d1), true);
		}

		public INumber Divide(INumber number)
		{
			Rational ratNum = (Rational) number;

			if (ratNum.numerator == 0) 
			{
				throw new System.DivideByZeroException();
			}

			long d1 = NumberUtils.Gcd(numerator, ratNum.numerator);
			long d2 = NumberUtils.Gcd(denominator, ratNum.denominator);

			long newDenom = (denominator/d2)*(ratNum.numerator/d1);
			if (newDenom < 0) newDenom = -newDenom;

			return new Rational((numerator/d1)*(ratNum.denominator/d2)*(ratNum.numerator<0?-1:1), newDenom, true);
		}

		public object Simplify()
		{
			if (denominator == 1) return numerator;

			return this;
		}

		#endregion

		public override string ToString()
		{
			return numerator.ToString() + "/" + denominator.ToString();
		}

	}
}
