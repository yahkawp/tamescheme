// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Exact complex number class                              RationalComplex.cs |
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
	/// Summary description for RationalComplex.
	/// </summary>
	public sealed class RationalComplex : INumber
	{
		public RationalComplex(Rational real, Rational imaginary)
		{
			this.real = real;
			this.imaginary = imaginary;
		}

		internal Rational real, imaginary;

		public Rational Real { get { return real; } }
		public Rational Imaginary { get { return imaginary; } }

		#region INumber Members

        public bool IsEqualTo(INumber number)
        {
            RationalComplex complex = (RationalComplex)number;

            return real.IsEqualTo(complex.real) && imaginary.IsEqualTo(complex.imaginary);
        }
		
		public int Compare(INumber number)
		{
			return 0;
		}

		public INumber Add(INumber number)
		{
            RationalComplex complex = (RationalComplex)number;

            return new RationalComplex(
                (Rational)real.Add(complex.real),
                (Rational)imaginary.Add(complex.imaginary)
                );
		}

		public INumber Subtract(INumber number)
		{
            RationalComplex complex = (RationalComplex)number;

            return new RationalComplex(
                (Rational)real.Subtract(complex.real),
                (Rational)imaginary.Subtract(complex.imaginary)
                );
        }

		public INumber Multiply(INumber number)
		{
            RationalComplex complex = (RationalComplex)number;

            return new RationalComplex(
                (Rational)real.Multiply(complex.real).Subtract(imaginary.Multiply(complex.imaginary)),
                (Rational)real.Multiply(complex.imaginary).Add(imaginary.Multiply(complex.real))
                );
		}

		public INumber Divide(INumber number)
		{
            RationalComplex complex = (RationalComplex)number;

            // Divisor is c^2 + d^2
            Rational divisor = (Rational)complex.real.Multiply(complex.real).Add(complex.imaginary.Multiply(complex.imaginary));

            return new RationalComplex(
                (Rational)(real.Multiply(complex.real).Add(imaginary.Multiply(complex.imaginary))).Divide(divisor),
                (Rational)(real.Multiply(complex.imaginary).Subtract(imaginary.Multiply(complex.real))).Divide(divisor)
                );
        }

		public object Simplify()
		{
            if (imaginary.Numerator == 0) return real.Simplify();

            return this;
		}

		#endregion

        public override string ToString()
        {
            if (real.Numerator == 0)
            {
                return imaginary.Simplify().ToString() + "i";
            }

            string res = real.Simplify().ToString();

            if (imaginary.Numerator > 0)
            {
                res += "+";
            }

            if (imaginary.Denominator != 1 || (imaginary.Numerator != 1 && imaginary.Numerator != -1))
            {
                res += imaginary.Simplify().ToString();
            }
            else
            {
                if (imaginary.Numerator == -1) res += "-";
            }
            
            res += "i";

            return res;
        }
	}
}
