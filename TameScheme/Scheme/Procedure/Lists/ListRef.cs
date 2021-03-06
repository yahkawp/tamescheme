// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The 'list-ref' library procedure                                ListRef.cs |
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
	/// The list-ref library procedure
	/// </summary>
    [PreferredName("list-ref"), SchemeGroup(SchemeGroup.Library), SchemeUsage(SchemeUsage.Normal)]
	public sealed class ListRef : IProcedure
	{
		public ListRef()
		{
		}
		
		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length != 2) throw new Exception.RuntimeException("list-ref should be called with precisely two arguments");

			long count = NumberUtils.MakeLong(args[1]);
			object result = args[0];

			for (int x=0; x<count; x++)
			{
				if (result == null || !(result is Pair)) throw new Exception.RuntimeException("Not enough elements in the list passed to list-ref");

				result = ((Pair)result).Cdr;
			}

			if (!(result is Pair)) throw new Exception.RuntimeException("Not enough elements in the list passed to list-ref");

			return ((Pair)result).Car;
		}

		#endregion
	}
}
