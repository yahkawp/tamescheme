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
			//
			// TODO: Add constructor logic here
			//
		}

		#region INumber Members

		public int Compare(INumber number)
		{
			return 0;
		}

		public INumber Add(INumber number)
		{
			// TODO:  Add Complex.Add implementation
			return null;
		}

		public INumber Subtract(INumber number)
		{
			// TODO:  Add Complex.Subtract implementation
			return null;
		}

		public INumber Multiply(INumber number)
		{
			// TODO:  Add Complex.Multiply implementation
			return null;
		}

		public INumber Divide(INumber number)
		{
			// TODO:  Add Complex.Divide implementation
			return null;
		}

		public object Simplify()
		{
			// TODO:  Add Complex.Simplify implementation
			return null;
		}

		#endregion
	}
}
