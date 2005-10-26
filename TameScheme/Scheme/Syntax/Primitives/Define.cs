// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'define' syntax                                                  Define.cs |
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
	/// Syntax for the 'define' primitive.
	/// </summary>
	[PreferredName("define"), SchemeSyntax("()", "((variable . formals) statement ...)", "(variable expression)")]
	public class Define : ISyntax
	{
		public Define()
		{
		}

		#region ISyntax Members

		const int functionSyntax = 0;
		const int variableSyntax = 1;

		static Data.Symbol variableSymbol = new Data.Symbol("variable");
		static Data.Symbol expressionSymbol = new Data.Symbol("expression");

		public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			if (syntaxMatch == functionSyntax)
			{
				// (define (x y) y) style function definition
				Console.WriteLine(env.ToString());

				return null;
			}
			else
			{
				// (define x y) variable definition
				object variable = env[variableSymbol].Value;
				object expression = env[expressionSymbol].Value;

				// The variable must be a symbol
				if (!(variable is Data.Symbol)) throw new Exception.SyntaxError("Attempt to define an object (" + Runtime.Interpreter.ToString(variable) + ") that is not a symbol");

				// The BExpression from evaluating the expression
				BExpression expr = BExpression.BuildExpression(expression, state).NonTail();

				// The BExpression that defines the result and pushes the defined symbol onto the stack
				Operation[] defineOps = new Operation[2];

				defineOps[0] = new Operation(Op.Define, ((Data.Symbol)variable).SymbolNumber);
				defineOps[1] = new Operation(Op.Push, (Data.Symbol)variable);

				BExpression defineExpr = new BExpression(defineOps);

				// Result is the combination of these two expressions
				return expr.Add(defineExpr);
			}
		}

		#endregion
	}
}
