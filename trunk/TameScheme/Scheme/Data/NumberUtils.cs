// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Useful utilities for dealing with numbers                   NumberUtils.cs |
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

namespace Tame.Scheme.Data
{
	public interface INumberIterator
	{
		// Iterating on various number types
		int Iterate(int one, int two);
		long Iterate(long one, long two);
		decimal Iterate(decimal one, decimal two);
		float Iterate(float one, float two);
		double Iterate(double one, double two);
		INumber Iterate(INumber one, INumber two);
	}

	/// <summary>
	/// Static class for doing things to numbers
	/// </summary>
	public class NumberUtils
	{
		#region Converting Numbers

		/// <summary>
		/// Given a number, makes a long value if possible (rounding floating/rational types)
		/// </summary>
		/// <param name="number">The number to make long</param>
		/// <returns>The long value for this number, if there is one</returns>
		/// <exception cref="Exception.RuntimeException">If the value cannot be made into a long number</exception>
		static public long MakeLong(object number)
		{
			if (number is long) return (long)number;
			else if (number is int) return (long)(int)number;
			else if (number is float) return checked((long)(float)number);
			else if (number is double) return checked((long)(double)number);
			else if (number is Number.Rational)
			{
				Number.Rational num = (Number.Rational)number;

				return checked(num.Numerator/num.Denominator);
			}

			// TODO: complex types can be cast to long if they have a 0 imaginary part
			throw new Exception.RuntimeException("Cannot convert a value of type " + number.GetType().ToString() + " to a long number");
		}

		/// <summary>
		/// Returns true if number is an exact type, false otherwise
		/// </summary>
		static bool IsExact(object number)
		{
			return (number is int)||(number is long)||(number is decimal)||(number is Number.Rational)||(number is Number.RationalComplex);
		}

		/// <summary>
		/// Makes number into an inexact number
		/// </summary>
		static object MakeInexact(object number)
		{
			if (number is int)
				return (float)(int)number;
			if (number is long)
				return (double)(long)number;
			if (number is decimal)
				return (double)(decimal)number;
			if (number is Number.Rational)
				return ((Number.Rational)number).ToDouble();
			if (number is Number.RationalComplex) 
			{
				Number.RationalComplex rc = (Number.RationalComplex)number;
				return new Number.Complex(rc.real.ToDouble(), rc.imaginary.ToDouble());
			}
			return number;
		}

		static Number.Rational rationalZero = new Number.Rational(0, 1);

		/// <summary>
		/// Promtes an exact number class to another one
		/// </summary>
		/// <param name="number">The number to promote</param>
		/// <param name="promoteTo">The type to promote it to</param>
		/// <returns>A promoted number (or null if the promotion wasn't possible)</returns>
		static object PromoteExact(object number, Type promoteTo)
		{
			// Exact promotions
			if (number is int) 
			{
				if (promoteTo == typeof(long)) return (long)(int)number;
				if (promoteTo == typeof(decimal)) return (decimal)(int)number;
				if (promoteTo == typeof(Number.Rational)) return new Number.Rational((int)number, 1);
				if (promoteTo == typeof(Number.RationalComplex)) return new Number.RationalComplex(new Number.Rational((int)number, 1), rationalZero);
				return null;
			}

			if (number is long) 
			{
				if (promoteTo == typeof(decimal)) return (decimal)(long)number;
				if (promoteTo == typeof(Number.Rational)) return new Number.Rational((long)number, 1);
				if (promoteTo == typeof(Number.RationalComplex)) return new Number.RationalComplex(new Number.Rational((long)number, 1), rationalZero);
				return null;
			}

			if (number is decimal) 
			{
				// TODO: work out a way to convert decimals to rationals (be nice if we could deconstruct the fixed point value)
				//decimal dec = (decimal)number;
				//if (promoteTo == typeof(Number.Rational)) return new Number.Rational(dec, 1);
				//if (promoteTo == typeof(Number.RationalComplex)) return new Number.RationalComplex(new Number.Rational((int)number, 1), rationalZero);
				return null;
			}
					
			if (number is Number.Rational)
			{
				if (promoteTo == typeof(Number.RationalComplex)) return new Number.RationalComplex((Number.Rational)number, rationalZero);
				return null;
			}

			return null;
		}

		static object PromoteInexact(object number, Type promoteTo)
		{
			if (number is float)
			{
				if (promoteTo == typeof(double)) return (double)(float)number;
				if (promoteTo == typeof(Number.Complex)) return new Number.Complex((double)(float)number, 0);
				return null;
			}
			if (number is double)
			{
				if (promoteTo == typeof(Number.Complex)) return new Number.Complex((double)number, 0);
				return null;
			}

			return null;
		}

		/// <summary>
		/// Converts two numbers so that they are the same type
		/// </summary>
		/// <param name="firstNumber">A number (either a .NET number type: int, uint, long, decimal or float; or an INumber)</param>
		/// <param name="secondNumber">Another number (either a .NET number type: int, uint, long, decimal or float; or an INumber)</param>
		/// <returns>An array of object[2] of two numbers of the same type</returns>
		/// <remarks>
		/// In scheme, numbers are divided into 'exact' and 'inexact' types. An exact type can be changed into an inexact type, but not
		/// vice-versa.
		/// 
		/// At present, there's no way to include user-defined numeric types.
		/// </remarks>
		static public object[] Convert(object firstNumber, object secondNumber)
		{
			object[] res = new object[2];

			Type firstType = firstNumber.GetType();
			Type secondType = secondNumber.GetType();

			// If the types are the same, there's nothing to do
			if (firstType == secondType)
			{
				res[0] = firstNumber;
				res[1] = secondNumber;

				return res;
			}

			// Otherwise, we need to convert one of the numbers to match the other's type
			// Exact:			int			long		decimal		Rational	RationalComplex
			// Inexact:			float		double								Complex

			// -- Convert exact to inexact if necessary
			bool firstExact  = IsExact(firstNumber);
			bool secondExact = IsExact(secondNumber);

			if (firstExact != secondExact)
			{
				if (firstExact)
				{
					firstNumber = MakeInexact(firstNumber);
					firstType = firstNumber.GetType();
					firstExact = false;
				}
				else
				{
					secondNumber = MakeInexact(secondNumber);
					secondType = secondNumber.GetType();
					secondExact = false;
				}

				// Stop here if this is all that's needed
				if (firstType == secondType)
				{
					res[0] = firstNumber;
					res[1] = secondNumber;

					return res;
				}
			}

			// One number needs to be promoted to be the same as the other number
			object firstPromote, secondPromote;

			if (firstExact)
			{
				firstPromote = PromoteExact(firstNumber, secondType);
				secondPromote = PromoteExact(secondNumber, firstType);
			}
			else
			{
				firstPromote = PromoteInexact(firstNumber, secondType);
				secondPromote = PromoteInexact(secondNumber, firstType);
			}

			// Return the results if one number promoted successfully
			if (firstPromote != null || secondPromote != null)
			{
				res[0] = firstPromote!=null?firstPromote:firstNumber;
				res[1] = secondPromote!=null?secondPromote:secondNumber;

				return res;
			}

			// If there's no valid conversions, then throw an exception
			throw new Exception.CantConvertNumbersException(firstType + " and " + secondType + " are not compatible numeric types");
		}

		#endregion

		#region GCD

		/// <summary>
		/// Computes the Greatest Common Divisor of two numbers
		/// </summary>
		public static int Gcd(int u, int v)
		{
			// Knuth 4.5.2 Alg B
			if (u == 0) return v;
			if (v == 0) return u;
			if (u < 0) u = -u;
			if (v < 0) v = -v; // ?? I think

			// Find power of 2
			int k = 0;

			while ((u&1) == 0 && (v&1) == 0)
			{
				k++;
				u >>= 1;
				v >>= 1;
			}

			// Initialise
			int t = ((u&1)==0)?(u>>1):-v;

            do
            {
                while ((t & 1) == 0)	// Is t even?
                {
                    // Halve t
                    t >>= 1;
                }

                // Reset max(u,v)
                if (t > 0) u = t; else v = -t;

                // Subtract
                t = u - v;
            }
            while (t != 0);

			return u << k;
		}
		
		/// <summary>
		/// Computes the Greatest Common Divisor of two numbers
		/// </summary>
		public static long Gcd(long u, long v)
		{
			// Knuth 4.5.2 Alg B
			if (u == 0) return v;
			if (v == 0) return u;
			if (u < 0) u = -u;
			if (v < 0) v = -v; // ?? I think

			// Find power of 2
			int k = 0;

			while ((u&1) == 0 && (v&1) == 0)
			{
				k++;
				u >>= 1;
				v >>= 1;
			}

			// Initialise
			long t = ((u&1)==0)?(u>>1):-v;

            do
            {
                while ((t & 1) == 0)	// Is t even?
                {
                    // Halve t
                    t >>= 1;
                }

                // Reset max(u,v)
                if (t > 0) u = t; else v = -t;

                // Subtract
                t = u - v;
            }
            while (t != 0);

			return u << k;
		}

		#endregion

		#region Iterating over lists of numbers

		// These routines are used in, for example, the Add routine to iterate over a list of numbers and produce a result

		static public object Iterate(object[] args, INumberIterator iterate)
		{
			if (args.Length < 2) return args[0];

			// The value we've accumulated, and the next object
			object[] values = Convert(args[0], args[1]);

			// The index we're at
			int index = 1;
			int length = args.Length;

			// Integer iterator
			while (index < length && values[0] is int)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((int)values[0], (int)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return values[0];

				values[1] = args[index];
				values = Convert(values[0], values[1]);
			}

			// Long iterator
			while (index < length && values[0] is long)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((long)values[0], (long)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return values[0];

				values[1] = args[index];
				values = Convert(values[0], values[1]);
			}

			// Decimal iterator
			while (index < length && values[0] is decimal)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((decimal)values[0], (decimal)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return values[0];

				values[1] = args[index];
				values = Convert(values[0], values[1]);
			}

			// INumber iterator
			while (index < length && values[0] is INumber)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((INumber)values[0], (INumber)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return ((INumber)values[0]).Simplify();

				values[1] = args[index];
				values = Convert(values[0], values[1]);
			}

			// Float iterator
			while (index < length && values[0] is float)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((float)values[0], (float)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return values[0];

				values[1] = args[index];
				values = Convert(values[0], values[1]);
			}

			// Double iterator
			while (index < length && values[0] is double)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((double)values[0], (double)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return values[0];

				values[1] = args[index];
				values = Convert(values[0], values[1]);
			}

			// INumber iterator (again: after a double, we can only end up as INumbers)
			// TODO: we also get here if we've an object that isn't a number: throw a suitable exception in this case
			while (index < length)
			{
				// Perform the iteration step
				values[0] = iterate.Iterate((INumber)values[0], (INumber)values[1]);

				// Move to the next object
				index++;
				if (index >= length) return ((INumber)values[0]).Simplify();

				values[1] = args[index];
			}

			// Return the result
			if (values[0] is INumber)
				return ((INumber)values[0]).Simplify();
			else
				return values[0];
		}

		#endregion
	}
}
