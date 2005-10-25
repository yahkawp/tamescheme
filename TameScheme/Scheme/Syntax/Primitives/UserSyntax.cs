// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | User-defined syntax                                          UserSyntax.cs |
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
using System.Collections;

using Tame.Scheme.Runtime;
using Tame.Scheme.Syntax.Transformer;

namespace Tame.Scheme.Syntax.Primitives
{
	/// <summary>
	/// UserSyntax represents syntax defined by the define-syntax operator.
	/// </summary>
	public class UserSyntax : SchemeSyntax, ISyntax
	{
		public UserSyntax(Syntax syntaxMatcher, ArrayList transformers) : base(syntaxMatcher, null)
		{
			this.transformers = transformers;
		}

		ArrayList transformers;

		#region ISyntax Members

		public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, Tame.Scheme.Data.Environment topLevel, Tame.Scheme.Data.Environment localEnvironment, int syntaxMatch)
		{
			// Get the transformation to use
			Transformation matchingTransformer = (Transformation)transformers[syntaxMatch];

			// Perform the transformation
			object translatedScheme = matchingTransformer.Transform(env.SyntaxTree);

			// TODO: we need a unified binder (we shouldn't be creating a new one here)
			Binder binder = new Binder();

			// Rename any temporary variables
			translatedScheme = binder.BindScheme(translatedScheme, topLevel);

			// Compile the result
			return BExpression.BuildExpression(translatedScheme, topLevel, localEnvironment);
		}

		#endregion
	}
}