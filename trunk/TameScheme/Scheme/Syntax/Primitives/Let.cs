// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'let'/'let*'/'letrec' syntax class                                  Let.cs |
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
	/// Implementation of the 'let', 'let*' and 'letrec' scheme syntaxes
	/// </summary>
	[SchemeSyntax("()", "(((variable expression) ...) firstStatement statements ...)")]
	public class Let : ISyntax
	{
		public enum Type
		{
			Let, LetStar, Letrec
		}

		public Let(Type letType)
		{
			this.letType = letType;
		}

		Type letType;

		#region ISyntax Members

		static Data.Symbol variable = new Data.Symbol("variable");
		static Data.Symbol firstStatement = new Data.Symbol("firstStatement");
		static Data.Symbol statements = new Data.Symbol("statements");

		public BExpression MakeExpression(SyntaxEnvironment env, Tame.Scheme.Data.Environment topLevel, Data.Environment local, int syntaxMatch)
		{
			// First part of a let is to create and load the new environment
			ArrayList loadEnvironment = new ArrayList();
			SyntaxNode variables = env[variable].Parent;

			// Create a new local environment for the expressions within the let statement
			Data.Environment letLocal = new Data.Environment(local);

			// When the let is in a tail context, no new environment is pushed (but the previous environment is overwritten instead)

			// For let*, letrec, push the environment now
			if (letType == Type.LetStar || letType == Type.Letrec)
			{
				loadEnvironment.Add(new Operation(Op.PushEnvironment, null, true));
			}

			// Evaluate the variable expressions in turn
			if (variables != null)
			{
				// (Used for let expressions: the eventual list to use with LoadEnvironment)
				ArrayList symbols = new ArrayList();
				SyntaxNode thisVariable = variables;

				// For letrec, load undefined values first
				if (letType == Type.Letrec)
				{
					while (thisVariable != null)
					{
						// First item must be a symbol
						if (!(thisVariable.Child.Value is Data.Symbol)) throw new Exception.SyntaxError("Variables in a let statement must be symbols (found " + Runtime.Interpreter.ToString(thisVariable.Child.Value) + ")");

						Data.Symbol varSym = (Data.Symbol)thisVariable.Child.Value;

						// Push an undefined value
						loadEnvironment.Add(new Operation(Op.Push, Data.Unspecified.Value));
						letLocal[(Data.Symbol)varSym] = Data.Unspecified.Value;

						// Store it for let* and letrec (for let, defer until we get to the later LoadEnvironment)
						symbols.Add(varSym.SymbolNumber);

						// Move on
						thisVariable = thisVariable.Sibling;
					}

					thisVariable = variables;

					// Load the undefined values
					int[] loadList = new int[symbols.Count];
					symbols.CopyTo(loadList);
					loadEnvironment.Add(new Operation(Op.LoadStackEnvironment, loadList));

					symbols.Clear();
				}

				// Actually evaluate the values
				while (thisVariable != null)
				{
					// First item must be a symbol
					if (!(thisVariable.Child.Value is Data.Symbol)) throw new Exception.SyntaxError("Variables in a let statement must be symbols (found " + Runtime.Interpreter.ToString(thisVariable.Child.Value) + ")");

					Data.Symbol varSym = (Data.Symbol)thisVariable.Child.Value;
					letLocal[varSym] = Data.Unspecified.Value;

					// Evaluate this variable
					BExpression varValueExpr = BExpression.BuildExpression(thisVariable.Child.Sibling.Value, topLevel, letLocal);
					loadEnvironment.AddRange(varValueExpr.NonTail().expression);

					// Store it for let* (for let and letrec, defer until we get to the later LoadEnvironment)
					if (letType == Type.LetStar)
					{
						loadEnvironment.Add(new Operation(Op.Define, varSym.SymbolNumber));
					}
					else
					{
						symbols.Insert(0, varSym.SymbolNumber);
					}

					thisVariable = thisVariable.Sibling;
				}

				// For let, this is the point at which we push a new environment
				if (letType == Type.Let) loadEnvironment.Add(new Operation(Op.PushEnvironment, null, true));

				// For let and letrec, load the symbols from the stack
				if (letType == Type.Let || letType == Type.Letrec)
				{
					int[] loadList = new int[symbols.Count];
					symbols.CopyTo(loadList);
					loadEnvironment.Add(new Operation(Op.LoadStackEnvironment, loadList));
				}
			}
			else if (letType == Type.Let)
			{
				// For let, push an empty environment (this won't otherwise be done)
				loadEnvironment.Add(new Operation(Op.PushEnvironment, null, true));
			}

			// loadEnvironment now contains the expression to set up the environment for this let statement
			BExpression letExpr = new BExpression(loadEnvironment);
			object lastStatement = env[firstStatement].Value;
			SyntaxNode statement = env[Let.statements];

			// Build the statements themselves
			while (statement != null)
			{
				// Evaluate lastStatement
				letExpr = letExpr.Add(BExpression.BuildExpression(lastStatement, topLevel, letLocal).NonTail());

				// Pop the result
				letExpr = letExpr.Add(new Operation(Op.Pop));

				// Move lastStatement on to the next statement
				lastStatement = statement;
				statement = statement.Sibling;
			}

			// The very last statement is in tail context (and the result isn't popped)
			letExpr = letExpr.Add(BExpression.BuildExpression(lastStatement, topLevel, letLocal));

			// Pop the environment we pushed
			letExpr = letExpr.Add(new Operation(Op.PopEnvironment, null, true));

			// Return the result
			return letExpr;
		}

		#endregion
	}
}