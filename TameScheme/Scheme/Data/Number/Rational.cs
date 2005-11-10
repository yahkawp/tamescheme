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

        public Rational(decimal val)
        {
            // Tease out the bits of the decimal value
            int[] bits = decimal.GetBits(val);

            // Certain values are too big to represent
            // TODO: some large values may be representable if the GCD brings their size back under the limit
            if (bits[2] != 0) throw new OverflowException("Decimal value is too large to be converted to a Rational");
            if ((bits[1] & (1 << 31)) != 0) throw new OverflowException("Decimal value is too large to be converted to a Rational");

            // The numerator is the (maximum 64-bit) numerical part of the value
            this.numerator = ((long)(uint)bits[0]) | (((long)(uint)bits[1]) << 32);

            // Work out the sign and the denominator
            bool sign = (bits[3] & (1 << 31)) != 0;
            int power = (bits[3] >> 16) & 0xff;

            this.denominator = (long)Math.Pow(10, power);

            // Apply the sign
            if (sign) this.numerator = -this.numerator;

            // Simplify
            long gcd = NumberUtils.Gcd(this.numerator, this.denominator);

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

		public long Numerator { get { return numerator; } }
		public long Denominator { get { return denominator; } }

		public double ToDouble()
		{
			return ((double)numerator)/((double)denominator);
		}

		#region INumber Members

        public bool IsEqualTo(INumber number)
        {
            Rational ratNum = (Rational)number;

            return numerator == ratNum.numerator && denominator == ratNum.denominator;
        }

		public int Compare(INumber number)
		{
            Rational ratNum = (Rational)number;
            Rational comp = (Rational)Subtract(ratNum);

            if (comp.numerator > 0)
                return 1;
            else if (comp.numerator < 0)
                return -1;
            else
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

			return new Rational(checked(t/d2), checked(denominator/d1)*checked(ratNum.denominator/d2), true);
		}

		public INumber Subtract(INumber number)
		{
			Rational ratNum = (Rational) number;

			long d1 = NumberUtils.Gcd(denominator, ratNum.denominator);
			
			if (d1 == 1)
			{
				return new Rational(numerator*ratNum.denominator - ratNum.numerator*denominator, denominator*ratNum.denominator, true);
			}

			long t = checked(numerator*(ratNum.denominator/d1) - ratNum.numerator*(denominator/d1));
			long d2 = NumberUtils.Gcd(t, d1);

			return new Rational(checked(t/d2), checked((denominator/d1)*(ratNum.denominator/d2)), true);
		}

		public INumber Multiply(INumber number)
		{
			Rational ratNum = (Rational) number;

			long d1 = NumberUtils.Gcd(numerator, ratNum.denominator);
			long d2 = NumberUtils.Gcd(denominator, ratNum.numerator);

			return new Rational(checked((numerator/d1)*(ratNum.numerator/d2)), checked((denominator/d2)*(ratNum.denominator/d1)), true);
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

			long newDenom = checked((denominator/d2)*(ratNum.numerator/d1));
			if (newDenom < 0) newDenom = -newDenom;

			return new Rational(checked((numerator/d1)*(ratNum.denominator/d2)*(ratNum.numerator<0?-1:1)), newDenom, true);
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
