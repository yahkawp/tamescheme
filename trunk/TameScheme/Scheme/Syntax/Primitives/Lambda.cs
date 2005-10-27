// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'lambda' syntax class                                            Lambda.cs |
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

// TODO: quite important: this won't work ATM: (define oops (let ((x 1)) (lambda () x))) - the function produced will evaluate in the wrong environment!

namespace Tame.Scheme.Syntax.Primitives
{
	/// <summary>
	/// Implementation of the scheme (lambda (x) x) expression.
	/// </summary>
	[PreferredName("lambda"), SchemeSyntax("()", "(args statement ...)")]
	public sealed class Lambda : ISyntax
	{
		public Lambda()
		{
		}

		private static Data.Symbol argsSymbol = new Data.Symbol("args");
		private static Data.Symbol statementSymbol = new Data.Symbol("statement");

		#region ISyntax Members

		public BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			// Load the argumetns from the frame
			ArrayList lambdaOperations = new ArrayList();			// Initial operations in the lambda expression
			ArrayList arguments = new ArrayList();					// List of arguments for the lambda expression

			bool lastIsAList = false;								// If 'true', then the list of arguments is improper: the last argument is a list of all the remaining arguments
			object args = env[argsSymbol].Value;					// The 'args' syntax parameter

			// Create a new local environment from the arguments
			Data.Environment argumentEnvironment = new Data.Environment(state.Local);
			CompileState lambdaState = new CompileState(state);
			lambdaState.Local = argumentEnvironment;

			// Read the list of arguments
			while (args != null)
			{
				if (args is Data.Pair)
				{
					Data.Pair pair = (Data.Pair) args;

					if (pair.Car is Data.Symbol)
					{
						// Store this argument
						arguments.Add(pair.Car);
						argumentEnvironment[(Data.Symbol)pair.Car] = Data.Unspecified.Value;

						// Move on to the next object
						args = pair.Cdr;
					}
					else
					{
						throw new Exception.SyntaxError("The function arguments in a lambda expression must be symbols: " + Interpreter.ToString(pair.Cdr) + " is not a symbol");
					}
				}
				else if (args is Data.Symbol)
				{
					// Improper list: mark as such, and add the argument
					lastIsAList = true;
					arguments.Add(args);
					break;
				}
				else
				{
					throw new Exception.SyntaxError("The function arguments in a lambda expression must be symbols: " + Interpreter.ToString(args) + " is not a symbol");
				}
			}

			// Load the environment
			Data.Symbol[] symbols = new Data.Symbol[arguments.Count];

			for (int x=0; x<arguments.Count; x++)
			{
				symbols[x] = (Data.Symbol)arguments[x];
			}

			// TODO: define, let in a tail context might introduce new arguments: we need to define this later
			lambdaOperations.Add(Operation.CreateLoadEnvironment(argumentEnvironment, symbols, lastIsAList, false));

			// Build the expression from the statements
			BExpression lambdaExpression = new BExpression(lambdaOperations);
			BExpression lastExpression = null;

			SyntaxNode statement = env[statementSymbol];

			while (statement != null)
			{
				if (lastExpression != null)
				{
					// Append the last expression to this expression
					lambdaExpression = lambdaExpression.Add(lastExpression.NonTail());
					
					// Discard the result
					lambdaExpression = lambdaExpression.Add(new Operation(Op.Pop));
				}

				// Compile the next statement
				lastExpression = BExpression.BuildExpression(statement.Value, lambdaState);

				// Move to the next statement
				statement = statement.Sibling;
			}

			// Append the last expression.
			lambdaExpression = lambdaExpression.Add(lastExpression.MakeTail());

			// Pop the stack before returning
			lambdaExpression = lambdaExpression.Add(new Operation(Op.PopFrame));

			// Clear up the expression (remove labels, etc)
			lambdaExpression.RemoveLabels();
			lambdaExpression = lambdaExpression.RemoveNops();

			// The BExpression result of a lambda expression is a new SProcecure
			return new BExpression(new Operation(Op.Push, new BProcedure(lambdaExpression)));
		}

		#endregion
	}
}
