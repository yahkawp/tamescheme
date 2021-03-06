// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'if' syntax class                                                    If.cs |
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
using Tame.Scheme.Procedure;
using Tame.Scheme.Runtime;

namespace Tame.Scheme.Syntax.Primitives
{
	/// <summary>
	/// Class implementing the syntax of 'if'.
	/// </summary>
    [PreferredName("if"), SchemeSyntax("()", "(cond then)", "(cond then else)"), SchemeGroup(SchemeGroup.Primitive), SchemeUsage(SchemeUsage.Normal)]
	public sealed class If : ISyntax
	{
		public If()
		{
		}

		static Data.Symbol condSym = new Data.Symbol("cond");
		static Data.Symbol thenSym = new Data.Symbol("then");
		static Data.Symbol elseSym = new Data.Symbol("else");

		#region ISyntax Members

		public Runtime.BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			object condObj = null;
			object thenObj = null;
			object elseObj = Data.Unspecified.Value;

			// The condition is not in a tail context
			CompileState conditionState = new CompileState(state, false);

			// 'cond' and 'then' always exist
			condObj = env[condSym].Value;
			thenObj = env[thenSym].Value;

			// 'else' only exists in some circumstances
			if (env.Contains(elseSym))
			{
				elseObj = env[elseSym].Value;
			}

			// Build the expressions for the various components
			BExpression condExpr = BExpression.BuildExpression(condObj, conditionState);
			BExpression thenExpr = BExpression.BuildExpression(thenObj, state);
			BExpression elseExpr = elseObj==Data.Unspecified.Value?null:BExpression.BuildExpression(elseObj, state);

			// Paste the operations together
			BExpression result = condExpr;

			result = result.AddIf(thenExpr, elseExpr);

			return result;
		}

		#endregion
	}
}
