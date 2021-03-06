// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | Expression made up of Operations                            BExpression.cs |
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
using System.Collections.Specialized;

namespace Tame.Scheme.Runtime
{
	/// <summary>
	/// A BExpression represents a partially compiled scheme statement. These can be executed directly by the interpreter, or (in the future)
	/// compiled directly to code.
	/// </summary>
	/// <remarks>Note that BExpressions are usually treated as immutable</remarks>
	public sealed class BExpression
	{
		public BExpression(ICollection operations)
		{
			// Code to build a BExpression
			expression = new Operation[operations.Count];
			operations.CopyTo(expression, 0);
		}

		public BExpression(Operation op)
		{
			// Code to build a BExpression from a single operation
			expression = new Operation[1];
			expression[0] = op;
		}

		private BExpression()
		{
			expression = null;
		}

		#region The expression

		internal Operation[] expression;				// The set of operations that make up this expression

		#endregion

		#region Creating combined and modified expressions

		/// <summary>
		/// Adds an expression that, if the value on the top of the stack is true, evaluates thenExpr, otherwise evaluates elseExpr
		/// </summary>
		/// <param name="thenExpr">The expression to evaluate if the topmost value is true</param>
		/// <param name="elseExpr">The expression to evaluate otherwise</param>
		/// <returns>An If expression</returns>
		public BExpression AddIf(BExpression thenExpr, BExpression elseExpr)
		{
			// 'else' should push an unspecified value if null is passed
			if (elseExpr == null) elseExpr = new BExpression(new Operation(Op.Push, Data.Unspecified.Value));

			// Allocate the result (2 extra instructions: an If and a Branch)
			BExpression result = new BExpression();
			result.expression = new Operation[expression.Length + thenExpr.expression.Length + elseExpr.expression.Length + 2];

			// Copy this expression to the new expression
			expression.CopyTo(result.expression, 0);

			// Skip over the 'else' part if we succeed
			result.expression[expression.Length] = new Operation(Op.If, elseExpr.expression.Length + 1);

			// If we fail, evaluate the 'else' part
			elseExpr.expression.CopyTo(result.expression, expression.Length+1);

			// ... but then skip the 'then' part
			result.expression[expression.Length + elseExpr.expression.Length + 1] = new Operation(Op.Branch, thenExpr.expression.Length);

			// finally, the 'then' part is executed if the test succeeded
			thenExpr.expression.CopyTo(result.expression, expression.Length + elseExpr.expression.Length + 2);

			return result;
		}

		/// <summary>
		/// Adds a new expression to this one.
		/// </summary>
		/// <param name="expr">The expression to append to the end of this expression</param>
		/// <returns>The new expression</returns>
		public BExpression Add(BExpression expr)
		{
			// Allocate the result
			BExpression result = new BExpression();
			result.expression = new Operation[expression.Length + expr.expression.Length];

			// Copy this expression to the new expression
			expression.CopyTo(result.expression, 0);

			// ... and then copy the expression to add
			expr.expression.CopyTo(result.expression, expression.Length);

			return result;
		}

		/// <summary>
		/// Adds a new operation to this expressiom.
		/// </summary>
		/// <param name="op">The expression to append to the end of this expression</param>
		/// <returns>The new expression</returns>
		public BExpression Add(Operation op)
		{
			// Allocate the result
			BExpression result = new BExpression();
			result.expression = new Operation[expression.Length + 1];

			// Copy this expression to the new expression
			expression.CopyTo(result.expression, 0);

			// ... and then add the new operation
			result.expression[expression.Length] = op;

			return result;
		}

		/// <summary>
		/// Replaces labels with Nops and BranchLabel and IfLabels with 
		/// </summary>
		/// <remarks>This is one of the few operations that alters the expression in place.</remarks>
		public void RemoveLabels()
		{
			// TODO: this should be an immutable operation, too
			// TODO: we need an ExpressionBuilder class (which should operate like StringBuilder)
			int pc;

			// First: get the locations of all the labels
			HybridDictionary labels = new HybridDictionary();
			
			for (pc=0; pc<expression.Length; pc++)
			{
				if (expression[pc].operation == Op.Label) labels.Add(expression[pc].a, pc);
			}

			// Next change any BranchLabel or IfLabels into Branch or Ifs
			for (pc=0; pc<expression.Length; pc++)
			{
				switch (expression[pc].operation)
				{
					case Op.BranchLabel:
					case Op.IfLabel:
						// Switch to an If/Branch as appropriate
						int newPc = (int)labels[expression[pc].a];
						Op newOp;

						if (expression[pc].operation == Op.IfLabel)
							newOp = Op.If;
						else
							newOp = Op.Branch;

						expression[pc] = new Operation(newOp, newPc-pc);
						break;

					case Op.Label:
						// Remove any labels (should remove Nops later)
						expression[pc] = new Operation(Op.Nop);
						break;
				}
			}
		}

		/// <summary>
		/// Removes the No-Ops from an expression
		/// </summary>
		/// <returns>A new BExpression that's the same as the last one, but with no No-Ops</returns>
		public BExpression RemoveNops()
		{
			// Mapping addresses array (for each entry in this expression, this contains the address that it corresponds to in the destination expression)
			ArrayList mapping = new ArrayList();
			ArrayList newExpression = new ArrayList();

			// Transcribe the expression into newExpression
			int opNum;
			bool hasNops = false;
			for (opNum=0; opNum<expression.Length; opNum++)
			{
				// Add the mapping for this entry
				mapping.Add(newExpression.Count);

				// Unless this is a No-Op, add to the new expression
				if (expression[opNum].operation != Op.Nop)
				{
					newExpression.Add(expression[opNum]);
				}
				else
				{
					hasNops = true;
				}
			}

			// Nothing to change if there are no No-Ops
			if (!hasNops) return this;

			// Change any Branch/If statements to map to the new address range
			for (opNum=0; opNum<newExpression.Count; opNum++)
			{
				Operation op = (Operation)newExpression[opNum];

				if (op.operation == Op.Branch || op.operation == Op.If)
				{
					// Work out the destination of the branch
					int oldDest = opNum + 1 + (int)op.a;

					if (oldDest < 0) throw new System.Exception("While removing No-Ops: found a branch that falls off the beginning of, the expression (this is always a bug)");

					int newDest = newExpression.Count;
					if (oldDest < mapping.Count) newDest = (int)mapping[oldDest];

					// Create a new branch instruction to go with it
					newExpression[opNum] = new Operation(op.operation, newDest - 1 - opNum);
				}
			}

			// Create and return the new expression
			return new BExpression(newExpression);
		}

		#endregion

		#region Building expressions

		public static BExpression BuildExpression(object expression, Data.Environment topLevel)
		{
			CompileState state = new CompileState();

			state.TopLevel = topLevel;

			return BuildExpression(expression, state);
		}

		/// <summary>
		/// Builds a scheme BExpression from a .NET object
		/// </summary>
		/// <param name="expression">The expression to build from</param>
		/// <param name="state">The environment to build the expression in</param>
		/// <returns>A new BExpression object</returns>
		/// <remarks>
		/// A stub environment is one that contains entries for the symbols that will be defined, but where the definitions themselves are
		/// meaningless. Scheme macro syntax in particular requires that we know the difference between a 'local' symbol and a 'top level'
		/// one. See the example at the end of section 4.2.3 of R5RS. Other syntax objects may also make use of this if necessary: the 'local'
		/// environment defines what's in the current and preceeding stack frames, so can be used to replace expensive symbol lookups with
		/// cheap frame lookups.
		/// 
		/// A future revision may define something to go in the 'local' environment (it is, in particular, only possible to determine that
		/// a variable exists in a frame, not where it exists at the moment)
		/// </remarks>
		public static BExpression BuildExpression(object expression, CompileState state)
		{
			// The array of operations that make up the new expression
			ArrayList operations = new ArrayList();

			// How the expression is evaluated depends on the type of expression
			if (expression == null)
			{
				operations.Add(new Operation(Op.Push, null));
			}
			else if (expression is Data.ISymbolic)
			{
				Data.ISymbolic iSym = (Data.ISymbolic)expression;
				Data.Environment symbolEnv = iSym.Location;

				if (symbolEnv == null)
				{
					// Use a standard PushSymbol operation
					operations.Add(Operation.PushSymbol(iSym, state));
				}
				else
				{
					// This symbol specifies an environment: push using that environment
					if (!state.TopLevel.Contains(iSym))
					{
						// The symbol must at least exist in the top-level environment
						state.TopLevel[iSym] = Data.Unspecified.Value;
					}

					// If the symbol does not exist in the specified environment, look in the top-level environment instead
					if (!symbolEnv.Contains(iSym)) symbolEnv = state.TopLevel;

					// Generate a relative/absolute binding instruction as appropriate
					Data.Environment.Binding absoluteBinding = symbolEnv.BindingForSymbol(iSym);
					Data.Environment.RelativeBinding relativeBinding = absoluteBinding.RelativeTo(state.Local, state.TopLevel);

					if (relativeBinding != null)
					{
						operations.Add(new Operation(Op.PushRelativeValue, relativeBinding));
					}
					else
					{
						operations.Add(new Operation(Op.PushBindingValue, absoluteBinding));
					}
				}
			}
			else if (expression is Data.Pair)
			{
				// Pairs are either syntax, in which case the BExpression is defined by the syntax routine, or a function call
				Data.Pair currentPair = (Data.Pair)expression;

				// If the first element of the pair is a symbol, then see if this pair can be interpreted as syntax
				Syntax.SchemeSyntax syntax = null;

				if (currentPair.Car is Data.ISymbolic)
				{
					Data.ISymbolic iSym = (Data.ISymbolic)currentPair.Car;
					Data.Environment symbolEnvironment = iSym.Location;
					if (symbolEnvironment == null) symbolEnvironment = state.TopLevel;

					if (symbolEnvironment.Contains(iSym))
					{
						object maybeSyntax = symbolEnvironment[iSym];
						if (maybeSyntax is Syntax.SchemeSyntax) syntax = (Syntax.SchemeSyntax)maybeSyntax;
					}
				}

				if (syntax != null)
				{
					// Syntax: try to match against the expression
					Syntax.SyntaxEnvironment syntaxEnvironment = null;

					int match = syntax.Syntax.Match(currentPair.Cdr, state, out syntaxEnvironment);

					if (match >= 0)
					{
						// Build the expression from the syntax
						BExpression syntaxExpression = syntax.Implementation.MakeExpression(syntaxEnvironment, state, match);

						operations.AddRange(syntaxExpression.expression);
					}
					else
					{
						// Report a syntax error
						throw new Exception.SyntaxError("Invalid syntax for '" + currentPair.Car.ToString() + "'");
					}
				}
				else
				{
					// Function call
					int count = 0;
				
					// The pair containing the element that defines the function to call
					Data.Pair iprocedure = (Data.Pair)expression;

					// Create expressions for each of the elements of the pair
					CompileState argState = new CompileState(state, false);

					// Skip the procedure node
					if (currentPair.Cdr != null && !(currentPair.Cdr is Data.Pair)) throw new Exception.SyntaxError("Function call expressions must be a well-formed list");
					currentPair = (Data.Pair)currentPair.Cdr;

					// Build the arguments on the stack
					while (currentPair != null)
					{
						// Create the expression for the current pair Car value
						BExpression pairExpr = BuildExpression(currentPair.Car, argState);

						// Add the expresion
						operations.AddRange(pairExpr.expression);

						// Move on to the next element
						if (currentPair.Cdr != null && !(currentPair.Cdr is Data.Pair)) throw new Exception.SyntaxError("Function call expressions must be a well-formed list");
						currentPair = (Data.Pair)currentPair.Cdr;
						count++;
					}

					// Push the procedure on to the stack
					BExpression iprocedureExpr = BuildExpression(iprocedure.Car, argState);
					operations.AddRange(iprocedureExpr.expression);

					// Perform the procedure call
					if (state.TailContext)
						operations.Add(new Operation(Op.TailCallIProcedure, count));
					else
						operations.Add(new Operation(Op.CallIProcedure, count));
				}
			}
			else
			{
				// Default action is just to push the object onto the stack
				operations.Add(new Operation(Op.Push, expression));
			}
			
			// operations now contains the list of operations that will make up the expression
			return new BExpression(operations);
		}

		#endregion

		#region Expression convienience functions

		public override string ToString()
		{
			string res = "";

			foreach (Operation op in expression)
			{
				res += op.ToString() + " ";
			}

			return "(" + res.Trim() + ")";
		}

		#endregion
	}
}
