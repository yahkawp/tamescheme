// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Complex number class                                            Complex.cs |
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
	/// Summary description for Complex.
	/// </summary>
	public sealed class Complex : INumber
	{
		public Complex(double real, double imaginary)
		{
            this.real = real;
            this.imaginary = imaginary;
		}

        double real, imaginary;

        public double Real { get { return real; } }
        public double Imaginary { get { return Imaginary; } }

		#region INumber Members

		public int Compare(INumber number)
		{
			return 0;
		}

		public INumber Add(INumber number)
		{
            Complex complex = (Complex)number;

            return new Complex(real + complex.real, imaginary + complex.imaginary);
		}

		public INumber Subtract(INumber number)
		{
            Complex complex = (Complex)number;

            return new Complex(real - complex.real, imaginary - complex.imaginary);
        }

		public INumber Multiply(INumber number)
		{
            Complex complex = (Complex)number;

            return new Complex(real*complex.real - imaginary*complex.imaginary, 
                real*complex.imaginary + imaginary*complex.real);
        }

		public INumber Divide(INumber number)
		{
            Complex complex = (Complex)number;

            double divisor = complex.real*complex.real + complex.imaginary*complex.imaginary;

            return new Complex((real*complex.real + imaginary*complex.imaginary) / divisor,
                (imaginary*complex.real - real*complex.imaginary) / divisor);
		}

		public object Simplify()
		{
            if (imaginary == 0.0) return real;
            return this;
		}

		#endregion

        public override string ToString()
        {
            if (real == 0)
            {
                return imaginary.ToString() + "i";
            }

            string res = real.ToString();

            if (imaginary > 0)
            {
                res += "+" + imaginary.ToString();
            }
            else
            {
                res += imaginary.ToString();
            }

            res += "i";

            return res;
        }
    }
}
