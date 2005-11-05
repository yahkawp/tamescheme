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

namespace Tame.Scheme.Syntax.Primitives
{
	/// <summary>
	/// Implementation of the scheme (lambda (x) x) expression.
	/// </summary>
    [PreferredName("lambda"), SchemeSyntax("()", "(args statement moreStatements ...)"), SchemeGroup(SchemeGroup.Primitive), SchemeUsage(SchemeUsage.Normal)]
	public sealed class Lambda : ISyntax, IBinding
	{
		public Lambda()
		{
		}

		private static Data.Symbol argsSymbol = new Data.Symbol("args");
		private static Data.Symbol statementSymbol = new Data.Symbol("statement");

		#region ISyntax Members

		public static BExpression MakeFunction(object args, SyntaxNode firstStatement, CompileState state)
		{
			// Load the arguments from the frame
			ArrayList lambdaOperations = new ArrayList();			// Initial operations in the lambda expression
			ArrayList arguments = new ArrayList();					// List of arguments for the lambda expression

			bool lastIsAList = false;								// If 'true', then the list of arguments is improper: the last argument is a list of all the remaining arguments

			// Create a new local environment from the arguments
			Data.Environment argumentEnvironment = new Data.Environment(state.Local);
			CompileState lambdaState = new CompileState(state, false);
			lambdaState.Local = argumentEnvironment;

			// Read the list of arguments
			while (args != null)
			{
				if (args is Data.Pair)
				{
					Data.Pair pair = (Data.Pair) args;

					if (pair.Car is Data.ISymbolic)
					{
						// Store this argument
						arguments.Add(pair.Car);
						argumentEnvironment[(Data.ISymbolic)pair.Car] = Data.Unspecified.Value;

						// Move on to the next object
						args = pair.Cdr;
					}
					else
					{
						throw new Exception.SyntaxError("The function arguments in a lambda expression must be symbols: " + Interpreter.ToString(pair.Cdr) + " is not a symbol");
					}
				}
				else if (args is Data.ISymbolic)
				{
					// Improper list: mark as such, and add the argument
					lastIsAList = true;
					arguments.Add(args);
					argumentEnvironment[(Data.ISymbolic)args] = Data.Unspecified.Value;
					break;
				}
				else
				{
					throw new Exception.SyntaxError("The function arguments in a lambda expression must be symbols: " + Interpreter.ToString(args) + " is not a symbol");
				}
			}

			// Load the environment
			Data.ISymbolic[] symbols = new Data.Symbol[arguments.Count];

			for (int x=0; x<arguments.Count; x++)
			{
				symbols[x] = (Data.ISymbolic)arguments[x];
			}

			// Build the expression from the statements
			BExpression lambdaExpression = null;
			BExpression lastExpression = null;
			CompileState tailState = new CompileState(lambdaState, true);

			SyntaxNode statement = firstStatement;

			while (statement != null)
			{
				if (lastExpression != null)
				{
					// Append the last expression to this expression
					if (lambdaExpression == null)
						lambdaExpression = lastExpression;
					else
						lambdaExpression = lambdaExpression.Add(lastExpression);
					
					// Discard the result
					lambdaExpression = lambdaExpression.Add(new Operation(Op.Pop));
				}

				// Compile the next statement (in tail context if it's the last expression)
				if (statement.Sibling == null)
					lastExpression = BExpression.BuildExpression(statement.Value, tailState);
				else
					lastExpression = BExpression.BuildExpression(statement.Value, lambdaState);

				// Move to the next statement
				statement = statement.Sibling;
			}

			// Append the last expression.
			if (lambdaExpression == null)
				lambdaExpression = lastExpression;
			else
				lambdaExpression = lambdaExpression.Add(lastExpression);

			// At this point, we know how to create the environment for this expression (we know where the locals go)
			lambdaOperations.Add(Operation.CreateLoadEnvironment(argumentEnvironment, symbols, lastIsAList, false));

			lambdaExpression = new BExpression(lambdaOperations).Add(lambdaExpression);

			// Pop the stack before returning
			lambdaExpression = lambdaExpression.Add(new Operation(Op.PopFrame));

			// Clear up the expression (remove labels, etc)
			lambdaExpression.RemoveLabels();
			lambdaExpression = lambdaExpression.RemoveNops();

			// The BExpression result of a lambda expression is a new SProcecure
			return new BExpression(new Operation(Op.PushContext, new BProcedure(lambdaExpression)));
		}

		public BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			return MakeFunction(env[argsSymbol].Value, env[statementSymbol], state);
		}

		#endregion

		#region IBinding Members

		public object BindScheme(object scheme, SyntaxEnvironment syntaxEnv, int syntaxMatch, Tame.Scheme.Syntax.Transformer.Binder.BindingState state)
		{
			// Build the rebound variable list
			object args = syntaxEnv[argsSymbol].Value;					// The 'args' syntax parameter
			object lastArg = args;

			if (args is Data.Pair)
			{
				Data.Pair thisArg = (Data.Pair)args;

				lastArg = null;

				while (thisArg != null)
				{
					// This is a rebound variable if it's
					if (thisArg.Car is Data.LiteralSymbol)
					{
						state.BindSymbol(thisArg.Car, state.TemporarySymbol());
					}
					
					// Move on to the next value
					if (thisArg.Cdr == null || thisArg.Cdr is Data.Pair)
					{
						thisArg = (Data.Pair)thisArg.Cdr;
					}
					else
					{
						lastArg = thisArg.Cdr;
						thisArg = null;
					}
				}
			}

			// The argument list might be improper: deal with this case (the improper section will be in lastArg)
			if (lastArg != null && lastArg is Data.LiteralSymbol)
			{
				state.BindSymbol(lastArg, state.TemporarySymbol());
			}

			// Perform the rebinding
			return state.Bind(scheme);
		}

		#endregion
	}
}
