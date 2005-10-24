// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Base class for dealing with number comparisons            NumberCompare.cs |
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

namespace Tame.Scheme.Procedure.Comparison
{
	/// <summary>
	/// Base class for numeric comparison procedures (=, &lt;=, etc)
	/// </summary>
	public abstract class NumberCompare : IProcedure
	{
		public NumberCompare()
		{
		}

		protected abstract bool Compare(int a, int b);
		protected abstract bool Compare(long a, long b);
		protected abstract bool Compare(decimal a, decimal b);
		protected abstract bool Compare(float a, float b);
		protected abstract bool Compare(double a, double b);
		protected abstract bool Compare(INumber a, INumber b);

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length < 2) return true;

			// The value we've accumulated, and the next object
			object[] values = NumberUtils.Convert(args[0], args[1]);

			// The index we're at
			int index = 1;
			int length = args.Length;

			// Integer iterator
			while (index < length && values[0] is int)
			{
				// Perform the iteration step
				if (!Compare((int)values[0], (int)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// Long iterator
			while (index < length && values[0] is long)
			{
				// Perform the iteration step
				if (!Compare((long)values[0], (long)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// Decimal iterator
			while (index < length && values[0] is decimal)
			{
				// Perform the iteration step
				if (!Compare((decimal)values[0], (decimal)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// INumber iterator
			while (index < length && values[0] is INumber)
			{
				// Perform the iteration step
				if (!Compare((INumber)values[0], (INumber)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// Float iterator
			while (index < length && values[0] is float)
			{
				// Perform the iteration step
				if (!Compare((float)values[0], (float)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// Double iterator
			while (index < length && values[0] is double)
			{
				// Perform the iteration step
				if (!Compare((double)values[0], (double)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// INumber iterator (again: after a double, we can only end up as INumbers)
			while (index < length)
			{
				// Perform the iteration step
				if (!Compare((INumber)values[0], (INumber)values[1])) return false;

				// Move to the next object
				index++;
				if (index >= length) return true;

				values[0] = values[1];
				values[1] = args[index];
				values = NumberUtils.Convert(values[0], values[1]);
			}

			// Return the result
			return true;
		}

		#endregion
	}
}
