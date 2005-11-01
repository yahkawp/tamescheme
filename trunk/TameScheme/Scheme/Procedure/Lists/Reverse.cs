// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The 'reverse' library procedure                                 Reverse.cs |
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
	/// The 'reverse' scheme library procedure
	/// </summary>
    [PreferredName("reverse"), SchemeGroup(SchemeGroup.Library), SchemeUsage(SchemeUsage.Normal)]
	public sealed class Reverse : IProcedure
	{
		public Reverse()
		{
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length != 1) throw new Exception.RuntimeException("reverse must be called with exactly one argument");
			if (args[0] == null) return null;
			if (!(args[0] is Pair)) throw new Exception.RuntimeException("The argument to reverse must be a proper list");

			Pair result = null;

			Pair fast = (Pair)args[0];
			Pair slow = (Pair)args[0];

			for (;;)
			{
				result = new Pair(fast.Car, result);
				if (fast.Cdr == null) break;
				if (!(fast.Cdr is Pair)) throw new Exception.RuntimeException("reverse called with an argument that is not a proper list");
				
				fast = (Pair)fast.Cdr;
				if (fast == slow) throw new Exception.RuntimeException("reverse called with an argument that is a circular list");

				result = new Pair(fast.Car, result);
				if (fast.Cdr == null) break;
				if (!(fast.Cdr is Pair)) throw new Exception.RuntimeException("reverse called with an argument that is not a proper list");
				
				fast = (Pair)fast.Cdr;
				if (fast == slow) throw new Exception.RuntimeException("reverse called with an argument that is a circular list");

				slow = (Pair)slow.Cdr;
			}

			return result;
		}

		#endregion
	}
}
