// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Base class for the three ass* functions                             Ass.cs |
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

namespace Tame.Scheme.Procedure.Lists
{
	/// <summary>
	/// Abstract base class for the assoc, assv, assq functions
	/// </summary>
	public abstract class Ass : IProcedure
	{
		public Ass()
		{
		}

		/// <summary>
		/// The comparison function to use to compare objects while searching
		/// </summary>
		protected abstract bool Compare(object a, object b);

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length != 2) throw new Exception.RuntimeException("Associative functions require exactly two arguments");
			if (args[1] == null) return false;
			if (!(args[1] is Data.Pair)) throw new Exception.RuntimeException("The second parameter to the associative functions must be a list");

			Data.Pair fast = (Data.Pair)args[1];
			Data.Pair slow = fast;

			for (;;)
			{
				// Compare this object
				if (fast.Car != null)
				{
					if (!(fast.Car is Data.Pair)) throw new Exception.RuntimeException("The list passed to the associative functions must be a list of pairs");
					if (Compare(((Data.Pair)fast.Car).Car, args[0])) return fast.Car;
				}

				// Move on
				if (fast.Cdr == slow) break;
				if (fast.Cdr == null) break;
				if (!(fast.Cdr is Data.Pair)) throw new Exception.RuntimeException("The second parameter to the associative functions must be a proper list");

				fast = (Data.Pair)fast.Cdr;

				// Compare this object
				if (fast.Car != null)
				{
					if (!(fast.Car is Data.Pair)) throw new Exception.RuntimeException("The list passed to the associative functions must be a list of pairs");
					if (Compare(((Data.Pair)fast.Car).Car, args[0])) return fast.Car;
				}

				// Move on
				if (fast.Cdr == slow) break;
				if (fast.Cdr == null) break;
				if (!(fast.Cdr is Data.Pair)) throw new Exception.RuntimeException("The second parameter to the associative functions must be a proper list");

				fast = (Data.Pair)fast.Cdr;
				slow = (Data.Pair)slow.Cdr;
			}

			return false;
		}

		#endregion
	}
}
