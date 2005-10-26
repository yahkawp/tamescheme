// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Interface implemented by scheme syntax classes                  ISyntax.cs |
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
	/// Interface implemented by objects implementing scheme syntax.
	/// </summary>
	/// <remarks>This is usually combined with a Syntax object describing the syntax to match in the top-level environment, via the SchemeSyntax object.</remarks>
	public interface ISyntax
	{
		/// <summary>
		/// Builds an BExpression when the syntax element is matched with the given environment
		/// </summary>
		/// <param name="env">The syntax environment built while matching</param>
		/// <param name="state">The current state of the compilation</param>
		/// <param name="syntaxMatch">The index of the match in the Syntax object</param>
		/// <returns>An BExpression for the syntax</returns>
		Runtime.BExpression MakeExpression(SyntaxEnvironment env, Runtime.CompileState state, int syntaxMatch);
	}
}
