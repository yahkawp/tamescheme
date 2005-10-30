// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'set!' syntax							                             Set.cs |
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
	/// 
	/// </summary>
	[PreferredName("set!"), SchemeSyntax("()", "(var expression)")]
	public class Set : ISyntax
	{
		public Set() { }

		static Data.Symbol var = new Data.Symbol("var");
		static Data.Symbol expression = new Data.Symbol("expression");

		#region ISyntax Members

		public BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			// Get the values
			object varObj = env[var].Value;
			object expressionObj = env[expression].Value;

			// varObj must be symbolic
			if (!(varObj is Data.ISymbolic))
			{
				throw new Exception.SyntaxError("set! requires a symbol to set");
			}

			Data.ISymbolic varSym = (Data.ISymbolic)varObj;

			// Get the binding for this symbol
			Data.Environment.Binding symbolBinding = null;

			if (state.Local != null) symbolBinding = state.Local.BindingForSymbol(varSym);
			if (symbolBinding == null && state.TopLevel != null) symbolBinding = state.TopLevel.BindingForSymbol(varSym);

			if (symbolBinding == null)
			{
				throw new Exception.SyntaxError("Cannot set '" + varSym.ToString() + "' as it is unbound in this context");
			}

			// Build the expression
			BExpression expr = BExpression.BuildExpression(expressionObj, new CompileState(state, false));

			// Set the value
			expr = expr.Add(Operation.Define(varSym, state));

			// Result is unspecified by R5RS; we act as for define
			expr = expr.Add(new Operation(Op.Push, varSym));

			// Return the result
			return expr;
		}

		#endregion
	}
}
