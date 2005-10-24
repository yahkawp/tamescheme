// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Attribute defining the syntax of an object        SchemeSyntaxAttribute.cs |
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
	/// Attribute that objects implementing scheme syntax can use to define their syntax.
	/// </summary>
	[AttributeUsage(AttributeTargets.Class)]
	public class SchemeSyntaxAttribute : Attribute
	{
		public SchemeSyntaxAttribute(string literals, params string[] syntaxes)
		{
			SyntaxElement[] elements = new SyntaxElement[syntaxes.Length];
			int x = 0;
			foreach (string syntax in syntaxes)
			{
				elements[x++]= SyntaxElement.MakeElementFromScheme(literals, syntax);
			}

			InitWithSyntax(elements);
		}

		public SchemeSyntaxAttribute(System.Collections.ICollection literals, params object[] syntaxes)
		{
			SyntaxElement[] elements = new SyntaxElement[syntaxes.Length];
			int x = 0;
			foreach (object syntax in syntaxes)
			{
				elements[x++]= SyntaxElement.MakeElementFromScheme(syntax, literals);
			}

			InitWithSyntax(elements);
		}

		public SchemeSyntaxAttribute(params SyntaxElement[] syntaxes)
		{
			InitWithSyntax(syntaxes);
		}

		private void InitWithSyntax(SyntaxElement[] syntaxes)
		{
			theSyntax = new Syntax(syntaxes);
		}

		public Syntax Syntax { get { return theSyntax; } }

		Syntax theSyntax;
	}
}
