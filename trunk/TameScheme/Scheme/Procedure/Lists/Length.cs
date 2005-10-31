// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The 'length' library procedure                                   Length.cs |
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

namespace Tame.Scheme.Procedure.Lists
{
	/// <summary>
	/// Implementation of the scheme 'length' procedure.
	/// </summary>
	[PreferredName("length")]
	public class Length : IProcedure
	{
		public Length()
		{
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length != 1) throw new Exception.RuntimeException("length must have exactly one argument");
			if (args[0] == null) return 0;
			if (!(args[0] is Pair)) throw new Exception.RuntimeException("length called with an argument that is not a proper list");

			Pair fast = (Pair)args[0];
			Pair slow = (Pair)args[0];

			int count = 0;

			for (;;)
			{
				count++;
				if (fast.Cdr == null) break;
				if (!(fast.Cdr is Pair)) throw new Exception.RuntimeException("length called with an argument that is not a proper list");
				
				fast = (Pair)fast.Cdr;
				if (fast == slow) throw new Exception.RuntimeException("length called with an argument that is a circular list");

				count++;
				if (fast.Cdr == null) break;
				if (!(fast.Cdr is Pair)) throw new Exception.RuntimeException("length called with an argument that is not a proper list");
				
				fast = (Pair)fast.Cdr;
				if (fast == slow) throw new Exception.RuntimeException("length called with an argument that is a circular list");

				slow = (Pair)slow.Cdr;
			}

			return count;
		}

		#endregion
	}
}
