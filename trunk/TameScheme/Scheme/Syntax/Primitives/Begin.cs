// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'begin' syntax							                           Begin.cs |
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

using Tame.Scheme.Procedure;
using Tame.Scheme.Runtime;

namespace Tame.Scheme.Syntax.Primitives
{
	/// <summary>
	/// Implements the scheme 'begin' syntax.
	/// </summary>
	[PreferredName("begin"), SchemeSyntax("()", "(expression ...)")]
	public class Begin : ISyntax
	{
		public Begin()
		{
		}

		static Data.Symbol expression = new Data.Symbol("expression");

		#region ISyntax Members

		public BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			// Find the expression
			SyntaxNode expressionNode = env[expression];

			// Push an unspecified value if there are no expressions
			if (expressionNode == null) return new BExpression(new Operation(Op.Push, Data.Unspecified.Value));
			BExpression beginExpression = null;

			// Create the compilation state for the subexpressions (these are not in tail context)
			CompileState subExpressionState = new CompileState(state, false);

			// Compile each expression, with the exception of the last one, and pop the result
			while (expressionNode.Sibling != null)
			{
				BExpression subExpression = BExpression.BuildExpression(expressionNode.Value, subExpressionState);

				if (beginExpression == null)
					beginExpression = subExpression;
				else
					beginExpression = beginExpression.Add(subExpression);

				// Pop the result
				beginExpression = beginExpression.Add(new Operation(Op.Pop));

				// Next node
				expressionNode = expressionNode.Sibling;
			}

			// The final node has the same state as the overall expression, and the result is not popped
			BExpression finalExpression = BExpression.BuildExpression(expressionNode.Value, state);

			if (beginExpression == null)
				beginExpression = finalExpression;
			else
				beginExpression = beginExpression.Add(finalExpression);

			// Return the result
			return beginExpression;
		}

		#endregion
	}


}
