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
	public class Let : ISyntax, IBinding
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

		public BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			// First part of a let is to create and load the new environment
			ArrayList loadEnvironment = new ArrayList();
			SyntaxNode variables = env[variable].Parent;
			ArrayList symbols = new ArrayList();

			// Create a new local environment for the expressions within the let statement
			Data.Environment letLocal = new Data.Environment(state.Local);
			CompileState letState = new CompileState(state);
			letState.Local = letLocal;

			// Specify where the operation to actually create the environment should go (we don't know the full extent of it
			// until after we've compiled all the expressions)
			int newEnvironmentOpPos = -1;			

			// When the let is in a tail context, no new environment is pushed (but the previous environment is overwritten instead)

			// For let* we create the environment immediately
			if (letType == Type.LetStar)
			{
				newEnvironmentOpPos = 0;
			}

			// Evaluate the variable expressions in turn
			if (variables != null)
			{
				// (Used for let expressions: the eventual list to use with LoadEnvironment)
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
						// loadEnvironment.Add(new Operation(Op.Push, Data.Unspecified.Value));
						letLocal[(Data.Symbol)varSym] = Data.Unspecified.Value;

						// Move on
						thisVariable = thisVariable.Sibling;
					}

					thisVariable = variables;

					// This is the point where the environment is created
					newEnvironmentOpPos = loadEnvironment.Count;
				}

				// Actually evaluate the values
				while (thisVariable != null)
				{
					// First item must be a symbol
					if (!(thisVariable.Child.Value is Data.Symbol)) throw new Exception.SyntaxError("Variables in a let statement must be symbols (found " + Runtime.Interpreter.ToString(thisVariable.Child.Value) + ")");

					Data.Symbol varSym = (Data.Symbol)thisVariable.Child.Value;

					// Evaluate this variable
					BExpression varValueExpr = BExpression.BuildExpression(thisVariable.Child.Sibling.Value, letState);
					loadEnvironment.AddRange(varValueExpr.expression);

					if (letType == Type.LetStar) letLocal[varSym] = Data.Unspecified.Value;

					// Store it for let* (for let and letrec, defer until we get to the later LoadEnvironment)
					if (letType == Type.LetStar || letType == Type.Letrec)
					{
						loadEnvironment.Add(new Operation(Op.DefineRelative, letLocal.RelativeBindingForSymbol(varSym)));
					}
					else if (letType == Type.Let)
					{
						symbols.Insert(0, varSym);
					}

					thisVariable = thisVariable.Sibling;
				}

				// For let, now the symbols become visible in letLocal
				if (letType == Type.Let)
				{
					foreach (Data.Symbol symbol in symbols)
					{
						letLocal[symbol] = Data.Unspecified.Value;
					}
				}

				// For let, this is the point at which we push a new environment
				if (letType == Type.Let) newEnvironmentOpPos = loadEnvironment.Count;
			}
			else if (letType == Type.Let)
			{
				// For let, push an empty environment (this won't otherwise be done)
				newEnvironmentOpPos = loadEnvironment.Count;
			}

			// loadEnvironment now contains the expression to set up the environment for this let statement
			BExpression letExpr = null;
			BExpression lastExpr = null;
			object lastStatement = env[firstStatement].Value;
			SyntaxNode statement = env[Let.statements];

			// Build the statements themselves
			while (statement != null)
			{
				// Evaluate lastStatement
				lastExpr = BExpression.BuildExpression(lastStatement, letState);
				if (letExpr == null)
					letExpr = lastExpr;
				else
					letExpr = letExpr.Add(lastExpr);

				// Pop the result
				letExpr = letExpr.Add(new Operation(Op.Pop));

				// Move lastStatement on to the next statement
				lastStatement = statement.Value;
				statement = statement.Sibling;
			}

			// The very last statement is in tail context (and the result isn't popped)
			lastExpr = BExpression.BuildExpression(lastStatement, letState);
			if (letExpr == null)
				letExpr = lastExpr;
			else
				letExpr = letExpr.Add(lastExpr);

			// Pop the environment we pushed
			letExpr = letExpr.Add(new Operation(Op.PopEnvironment, null));

			// At this point, we know exactly what the environment will look like, so we can add the operation to build it
			switch (letType)
			{
				case Type.Let:
					// Create/load from the stack
					loadEnvironment.Insert(newEnvironmentOpPos, Operation.CreateLoadEnvironment(letLocal, symbols, false, true));
					break;

				case Type.Letrec:
				case Type.LetStar:
					loadEnvironment.Insert(newEnvironmentOpPos, Operation.CreateEnvironment(letLocal));
					break;
			}

			// Add the loading expressions to the evaluation expressions
			BExpression loadingExpr = new BExpression(loadEnvironment);

			// Return the result
			return loadingExpr.Add(letExpr);
		}

		#endregion

		#region IBinding Members

		public object BindScheme(object scheme, SyntaxEnvironment env, Tame.Scheme.Syntax.Transformer.Binder.BindingState state)
		{
			// Build the rebound variable list
			Data.Pair oldVariables = (Data.Pair)((Data.Pair)scheme).Car;

			// Special case: if there are no variables
			if (oldVariables == null) return new Data.Pair(null, state.Bind(((Data.Pair)scheme).Cdr));

			if (letType == Type.Letrec)
			{
				// Variable bindings are self-referential: add them immediately to our binding state
				foreach (Data.Pair variablePair in oldVariables)
				{
					if (variablePair.Car is Data.LiteralSymbol)
					{
						// LiteralSymbols need to be rebound to temporary variables
						state.BindSymbol(variablePair.Car, state.TemporarySymbol());
					}
				}
			}

			// For each variable...
			ArrayList newVariableList = new ArrayList();								// We convert this later into the actual scheme involved

			foreach (Data.Pair variablePair in oldVariables)
			{
				// The name and the assignment for this variable
				object newVariable = variablePair.Car;
				object newAssignment = ((Data.Pair)variablePair.Cdr).Car;

				if (letType != Type.Let)
				{
					// Letrec and LetStar both rebind their assignments (let assigns from 'outside')
					newAssignment = state.Bind(newAssignment);
				}

				if (letType != Type.Letrec)
				{
					// (If this is letrec, the names are already assigned)
					if (newVariable is Data.LiteralSymbol)
					{
						// Reassign this value
						state.BindSymbol(newVariable, state.TemporarySymbol());
					}
				}

				// Rebind the variable
				newVariable = state.Bind(newVariable);

				// Add the value for this binding
				newVariableList.Add(new Data.Pair(newVariable, new Data.Pair(newAssignment, null)));
			}

			// Produce the result (the variable list we just produced, followed by the rebound code
			return new Data.Pair(new Data.Pair(newVariableList), state.Bind(((Data.Pair)scheme).Cdr));
		}

		#endregion
	}
}