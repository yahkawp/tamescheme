// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The 'append' library procedure                                   Append.cs |
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
	/// The 'append' library procedure.
	/// </summary>
	[PreferredName("append")]
	public class Append : IProcedure
	{
		public Append()
		{
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length == 0) return null;

			Pair result = new Pair(null, null);						// Placeholder
			Pair pointer = result;

			for (int argNum=0; argNum<args.Length-1; argNum++)
			{
				if (args[argNum] == null) continue;
				if (!(args[argNum] is Pair)) throw new Exception.RuntimeException("append called with an argument that is not a proper list");

				Pair fast = (Pair)args[argNum];
				Pair slow = (Pair)args[argNum];

				for (;;)
				{
					pointer.Cdr = new Pair(fast.Car, null);
					pointer = (Pair)pointer.Cdr;

					if (fast.Cdr == null) break;
					if (!(fast.Cdr is Pair)) throw new Exception.RuntimeException("append called with an argument that is not a proper list");
				
					fast = (Pair)fast.Cdr;
					if (fast == slow) throw new Exception.RuntimeException("append called with an argument that is a circular list");

					pointer.Cdr = new Pair(fast.Car, null);
					pointer = (Pair)pointer.Cdr;

					if (fast.Cdr == null) break;
					if (!(fast.Cdr is Pair)) throw new Exception.RuntimeException("append called with an argument that is not a proper list");
				
					fast = (Pair)fast.Cdr;
					if (fast == slow) throw new Exception.RuntimeException("append called with an argument that is a circular list");

					slow = (Pair)slow.Cdr;
				}
			}

			pointer.Cdr = args[args.Length-1];

			// Result is the pair following the placeholder pair
			return result.Cdr;
		}

		#endregion
	}
}
