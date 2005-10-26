// +----------------------------------------------------------------------------+
// |                               = TAMESCHEME =                               |
// | 'define-syntax' syntax                                     DefineSyntax.cs |
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

using Tame.Scheme.Data;
using Tame.Scheme.Procedure;
using Tame.Scheme.Runtime;
using Tame.Scheme.Syntax.Transformer;

namespace Tame.Scheme.Syntax.Primitives
{
	/// <summary>
	/// Implementation of the scheme define-syntax operation.
	/// </summary>
	[PreferredName("define-syntax"), SchemeSyntax("(syntax-rules)", "(name (syntax-rules (literal ...) (pattern template) ...))")]
	public class DefineSyntax : ISyntax
	{
		public DefineSyntax()
		{ }

		static Symbol name = new Symbol("name");
		static Symbol literal = new Symbol("literal");
		static Symbol pattern = new Symbol("pattern");

		#region ISyntax Members

		public Tame.Scheme.Runtime.BExpression MakeExpression(SyntaxEnvironment env, CompileState state, int syntaxMatch)
		{
			// Get the name of the syntax we're defining
			Symbol syntaxName = null;

			if (!(env[name].Value is Symbol))
				throw new Exception.SyntaxError("(define-syntax) called to define something other than a symbol");

			syntaxName = (Symbol)env[name].Value;

			// Locate the literals
			SyntaxNode literalNode = null;

			if (env[literal] != null)
				literalNode = env[literal].Parent;

			// ... and the list of patterns and templates
			SyntaxNode templateNode = null;

			if (env[pattern] != null)
				templateNode = env[pattern].Parent;

			// Get the list of literals
			ArrayList literalList = new ArrayList();

			while (literalNode != null)
			{
				if (!(literalNode.Value is Symbol))
					throw new Exception.SyntaxError("(define-syntax) called with syntax-rules that specify a value other than a symbol as literal");

				literalList.Add((Symbol)literalNode.Value);

				literalNode = literalNode.Sibling;
			}

			// Iterate across the list of pattern
			ArrayList syntaxMatchers = new ArrayList();
			ArrayList transformers = new ArrayList();

			while (templateNode != null)
			{
				// Get the pattern and template for this portion of the syntax
				object matchPattern = templateNode.Child.Value;
				object matchTemplate = templateNode.Child.Sibling.Value;

				// matchPattern should be a pair
				if (!(matchPattern is Pair))
					throw new Exception.SyntaxError("Patterns in syntax-rules must be a list beginning with the syntax symbol");

				// The first part of matchPattern is ignored (it should just be the name again)
				matchPattern = ((Pair)matchPattern).Cdr;

				// Generate the pattern to match against
				SyntaxElement thisMatcher = SyntaxElement.MakeElementFromScheme(matchPattern, literalList);

				// Generate the transformation to produce this syntax
				Transformation thisTransformer = new SyntaxCompiler(thisMatcher).Compile(matchTemplate, state.TopLevel);

				// Add to our collection of results
				syntaxMatchers.Add(thisMatcher);
				transformers.Add(thisTransformer);

				// Move to the next node
				templateNode = templateNode.Sibling;
			}

			if (syntaxMatchers.Count <= 0)
				throw new Exception.SyntaxError("You must define at least one pattern and template in your syntax-rules");

			// Generate the UserSyntax object
			UserSyntax newSyntax = new UserSyntax(new Syntax(syntaxMatchers), transformers);

			// Create the expression
			Operation[] defineOps = new Operation[3];

			defineOps[0] = new Operation(Op.Push, newSyntax);
			defineOps[1] = new Operation(Op.Define, ((Data.Symbol)syntaxName).SymbolNumber);
			defineOps[2] = new Operation(Op.Push, (Data.Symbol)syntaxName);

			return new BExpression(defineOps);
		}

		#endregion
	}
}
