// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Object representing a set of valid syntaxes                      Syntax.cs |
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

namespace Tame.Scheme.Syntax
{
	/// <summary>
	/// Object representing a syntax for a keyword in scheme.
	/// </summary>
	public class Syntax
	{
		public Syntax(params SyntaxElement[] element)
		{
			this.element = new SyntaxElement[element.Length];
			element.CopyTo(this.element, 0);
		}

		public int Match(object scheme, out SyntaxEnvironment bindingEnvironment)
		{
			int x = 0;
			foreach (SyntaxElement elem in element)
			{
				if (elem.Match(scheme, out bindingEnvironment)) return x;
				x++;
			}

			bindingEnvironment = null;
			return -1;
		}

		SyntaxElement[] element;
	}
}
