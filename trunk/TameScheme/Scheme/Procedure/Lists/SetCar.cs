// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | The set-car! scheme procedure                                    SetCar.cs |
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
	/// The set-car! scheme procedure
	/// </summary>
	[PreferredName("set-car!")]
	public sealed class SetCar : IProcedure
	{
		public SetCar()
		{
		}

		#region IProcedure Members

		public object Call(Tame.Scheme.Data.Environment environment, ref object[] args)
		{
			if (args.Length != 2) throw new Exception.RuntimeException("set-car! takes two arguments");
			if (!(args[0] is Pair)) throw new Exception.RuntimeException("The first argument to set-car! must be a pair");

			((Pair)args[0]).Car = args[1];

			return args[0];
		}

		#endregion
	}
}
