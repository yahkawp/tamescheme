// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'quote' syntax							                           Quote.cs |
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
	/// Quote implements the (quote foo) syntax
	/// </summary>
	[PreferredName("quote"), SchemeSyntax("()", "(something)")]
	public class Quote : ISyntax, IQuoted
	{
		public Quote()
		{
		}

		static Data.Symbol something = new Data.Symbol("something");

		#region ISyntax Members

		public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, Tame.Scheme.Runtime.CompileState state, int syntaxMatch)
		{
			// Just push whatever 'something' is onto the stack
			return new BExpression(new Operation(Op.Push, env[something].Value));
		}

		#endregion

		#region IQuoted Members

		private object GetSymbol(object scheme)
		{
			if (scheme is Data.Pair)
			{
				// Transform the Car and the Cdr
				return new Data.Pair(GetSymbol(((Data.Pair)scheme).Car), GetSymbol((((Data.Pair)scheme).Cdr)));
			}
			else if (scheme is ICollection)
			{
				// Transform each element in the collection to create a new vector
				ICollection col = (ICollection)scheme;
				object[] res = new object[col.Count];
				IEnumerator colEnum = col.GetEnumerator();

				for (int item=0; item<col.Count; item++)
				{
					colEnum.MoveNext();
					res[item] = colEnum.Current;
				}

				return res;
			}
			else if (scheme is Data.ISymbolic)
			{
				return ((Data.ISymbolic)scheme).Symbol;
			}
			else
			{
				return scheme;
			}
		}

		public object QuoteScheme(object scheme, SyntaxEnvironment matchEnvironment, Tame.Scheme.Syntax.Transformer.Binder.BindingState bindState)
		{
			// Change all ISymbolic children to their simpler equivalent
			object res =  new Data.Pair(((Data.Pair)scheme).Car, GetSymbol(((Data.Pair)scheme).Cdr));

			return res;
		}

		#endregion
	}
}
