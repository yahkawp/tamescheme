// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interface for 'advanced' number classes                         INumber.cs |
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
	/// <summary>
	/// Interface implemented by scheme numbers. Numbers should be immutable, and are usually sealed classes.
	/// </summary>
	public interface INumber
	{
		#region Arithmetic

		/// <summary>
		/// Adds a number to this number, and returns the result
		/// </summary>
		/// <param name="number">The number to add (this should be the same class as this number)</param>
		/// <returns>A new number containing the result of adding the two numbers together</returns>
		INumber Add(INumber number);

		/// <summary>
		/// Subtracts a number from this number, and returns the result.
		/// </summary>
		/// <param name="number">The number to subtract from this number (same class as this number)</param>
		/// <returns>A new number containing the result of subtracting the two numbers</returns>
		INumber Subtract(INumber number);

		/// <summary>
		/// Multiplies this number with another number.
		/// </summary>
		/// <param name="number">The number to multiply this number with (same class as this number)</param>
		/// <returns>A new number containing the result of multiplying the two numbers</returns>
		INumber Multiply(INumber number);

		/// <summary>
		/// Divides this number by another number.
		/// </summary>
		/// <param name="number">The number to divide this number by (same class as this number)</param>
		/// <returns>A new number containing the result of dividing the two numbers</returns>
		INumber Divide(INumber number);

		#endregion

		#region Comparison

        /// <summary>
        /// Compares this number to another of the same type, testing for equality rather than ordering
        /// </summary>
        /// <returns>true if the numbers are equal, false otherwise</returns>
        bool IsEqualTo(INumber compareTo);

		/// <summary>
		/// Compares this number to another of the same type, testing for ordering rather than equality
		/// </summary>
		/// <returns>-1 if the compareTo is less than this number, 0 if equal, 1 if greater than</returns>
		int Compare(INumber compareTo);

		#endregion

        #region Singular operations

        /// <summary>
        /// Returns the absolute value of this number
        /// </summary>
        object Abs();

        #endregion

        /// <summary>
		/// Changes a number to a simpler form (for example, normalises a rational, or turns it into an integer)
		/// </summary>
		/// <returns>The number, or a simpler version if one is available</returns>
		object Simplify();
	}
}
